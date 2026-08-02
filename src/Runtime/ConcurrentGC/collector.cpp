#include "collector.h"

#include <atomic>
#include <iostream>
#include <mutex>
#include <ranges>

#include "../../Includes/fmt/core.h"
#include "../../Includes/rpmalloc/rpmalloc.h"
#include "gc-heuristic.h"

using namespace gc;
using namespace detail;

enum gc_operation {
    op_mark_stack = 1,
    op_stw = 2
};

std::vector<tcb *> collector::post_with_state(gc_state s, uint8_t op)  {
    std::vector<tcb*> blocked;
    _tcb_registry.with_snapshot([&](auto& thds) {
        _gc_flag.store((uint8_t)s, std::memory_order_release);
        blocked = _thd_state_mngr.post(std::views::all(thds), op);
    });
    return blocked;
}

[[gnu::cold, clang::noinline]] void collector::force_collection(int64_t sz, tcb* t) {
    _collection_req.request_express();

    int64_t snapshot = _heuristic.live_size() + _alloc_sz;
    set_paused(t);
    _collection_req.await_express_served();
    set_resumed(t);
    if (snapshot - _heuristic.live_size() > sz) return;

    std::cerr
        <<"Thread "
        <<std::this_thread::get_id()
        <<fmt::format(" failed to allocate {} bytes, heap size is {}, exiting...\n", sz, _alloc_sz);
    std::abort();
}

void collector::stw_enter(std::span<tcb*> owned) {
    for (auto t : owned) {
        auto& arena = t->get_arena();
        arena.flush_alloc_caches();

        auto dbt = arena.get_debt();
        _alloc_sz += dbt;
        arena.remove_debt(dbt);
        _thd_state_mngr.ack();
    }
    // Have to wait for everyone to flush their alloc caches before we can start accurate marking
    _thd_state_mngr.wait_on_all_ack();
}

size_t collector::stw_mark(std::span<tcb*> owned, stw_role role, bool copying) {
    // TODO: this is wrong and only takes into account the mark pause time, not the concurrent mark time
    auto _ = _metrics.time(gc_metrics::phase::mark);
    for (auto t : owned)
        _marker.scan_stack(t->get_mark_info(), copying, get_obj_base());

    size_t snapshot = 0;
    if (role == stw_role::collector) {
        // Scan temps in stw when noone can mutate them
        for (auto [tcb, sp] : _temp_roots)
            _marker.scan_temp(sp, get_obj_base(), copying);
        _marker.scan_globals(_roots);
        snapshot = _alloc_sz.load();
        // Every thread has stopped at this point so this is safe to do
        _gc_flag.store((uint8_t)gc_state::none, std::memory_order_release);
    }

    while (_marker.trace_n(config::trace_batch * 1024)) {}
    _gate.arrive_and_wait();
    if (role == stw_role::collector)
        _cycle.mark_time = gc_clock::now().time_since_epoch() - _cycle.mark_time;
    return snapshot;
}

void collector::stw_copy(std::span<tcb*> owned, stw_role role) {
    auto _ = _metrics.time(gc_metrics::phase::copy);

    for (auto t : owned) t->get_arena().mutate_owned(copy_objs_fn());
    if (role == stw_role::collector) _pg_manager.mutate_owned(copy_objs_fn());
    _gate.arrive_and_wait();

    for (auto t : owned) t->get_arena().mutate_owned(update_ptrs_fn());
    if (role == stw_role::collector) {
        _pg_manager.mutate_owned(update_ptrs_fn());
        _copier.update_globals(_roots);
    }
    _gate.arrive_and_wait();
}

void collector::stw_prune(std::span<tcb*> owned, stw_role role) {
    auto _ = _metrics.time(gc_metrics::phase::sweep);
    for (auto t : owned) t->get_arena().mutate_owned(prune_pgs_fn());
    if (role == stw_role::collector) _pg_manager.mutate_owned(prune_pgs_fn());
}

size_t collector::stw(std::span<tcb*> owned, stw_role role) {
    stw_enter(owned);
    // Decided after stw_enter's wait_on_all_ack
    auto copying = _collection_req.is_express() || _heuristic.should_copy();
    auto snapshot = stw_mark(owned, role, copying);
    if (copying) [[unlikely]] stw_copy(owned, role);
    stw_prune(owned, role);
    return snapshot;
}

void collector::concurrent_mark() {
    auto blocked = post_with_state(gc_state::marking, op_mark_stack);
    for (auto t : blocked)
        _marker.scan_stack(t->get_mark_info(), false, get_obj_base());
    _thd_state_mngr.complete_handshake(blocked);
    _thd_state_mngr.wait_on_all_ack();
    // Must be under lock because we can be adding global variables dynamically
    // (need to fix this honestly and add every global as root on start of program)
    {
        auto _ = std::scoped_lock { _root_mtx };
        _marker.scan_globals(_roots);
    }
    
    while (_marker.trace_n(config::trace_batch) && !_collection_req.is_express()) {}
}

size_t collector::worker_stw() {
    auto _ = _metrics.time(gc_metrics::phase::pause);
    auto blocked = post_with_state(gc_state::stw, op_stw);
    auto snapshot = stw(blocked, stw_role::collector);
    _tcb_registry.with_snapshot([&](auto& thds){
        _thd_state_mngr.finish_stw(std::views::all(thds));
    });
    return snapshot;
}

void collector::mutator_stw(tcb* handle) {
    auto _ = _metrics.time(gc_metrics::phase::pause);
    stw({ &handle, 1 }, stw_role::mutator);
}

void collector::collect_metrics() {
    using phase = gc_metrics::phase;
    _metrics.collect(phase::pause);
    _metrics.collect(phase::mark);
    _metrics.collect(phase::sweep);
    _cycle.copy_time = _metrics.collect(phase::copy);
}

void collector::end_cycle(size_t alloc_snapshot) {
    _alloc_sz -= alloc_snapshot;
    _cycle.allocated_bytes = alloc_snapshot;
    _cycle.live_bytes      = _pruner.get_live_size();
    auto evac = _pruner.estimate_evacuation();
    _cycle.evac_gain_bytes = evac.gain_bytes;
    _cycle.evac_move_bytes = evac.move_bytes;
    _heuristic.end_cycle(gc_clock::now(), _cycle);
    _pruner.reset();
    // If some thread paused because it was out of memory wake it up AFTER all the calculations have been done
    _collection_req.clear();
}


void collector::concurrent_loop() {
    rpmalloc_thread_initialize();
    while (true) {
        if (_collection_req.await_request()) break;
        _cycle = {};
        _cycle.mark_time = gc_clock::now().time_since_epoch();
        concurrent_mark();

        _gate.register_waiter();
        auto snapshot = worker_stw();
        // This makes sure all other threads have left the stw phase
        _gate.arrive_and_wait();
        _gate.deregister_waiter();

        collect_metrics();
        end_cycle(snapshot);

        _pg_manager.free_pgs();

        _pg_manager.foreach_active_pg([&](pg_meta* meta) { meta->clear_mark_bitmap(); });
        _tcb_registry.under_lock([&] {
            assert(_gc_flag.load(std::memory_order_acquire) == 0);
            _marker.remove_empty();
        });
    }
    rpmalloc_thread_finalize(1);
}

// Stw only registers us as waiters and flushes the cache, then _synchronizer will ack for us
void collector::handle_pending(tcb* t) {
    auto op = t->get_opcode();
    if (op == op_mark_stack) {
         _marker.scan_stack(t->get_mark_info(), false, get_obj_base());
        _thd_state_mngr.ack();
    } else if (op == op_stw) {
        _gate.register_waiter();
        mutator_stw(t);
        _gate.deregister_waiter();
    }
    else
        assert(false && "Unreachable");
}

[[gnu::cold, clang::noinline]] void collector::alloc_update(tcb* t, size_t debt) {
    auto heap_sz = _alloc_sz.fetch_add(debt, std::memory_order_relaxed) + debt + _heuristic.live_size();
    if (heap_sz > _heuristic.heap_trigger()) _collection_req.request_normal();
    t->get_arena().remove_debt(debt);
}

void collector::thd_prologue(tcb *t) {
    rpmalloc_thread_initialize();
    _thd_state_mngr.prologue(t);
    _tcb_registry.under_lock([&](){ _temp_roots.erase(t); });
}

void collector::set_paused(tcb *t) {
    t->get_mark_info().capture_ctx();
    flush_wbbuf(t);
    _thd_state_mngr.enter_blocked(t, [&](tcb* thd) { handle_pending(thd); });
}

void collector::set_resumed(tcb *t) {
    _thd_state_mngr.exit_blocked(t);
}

tcb *collector::create_tcb(size_t *start_args, uint8_t args_cnt) {
    auto t = new (rpmalloc(sizeof(tcb))) tcb { start_args, args_cnt };
    _tcb_registry.add(t, [&]() {
        // Threads created during stw will be paused and needs to be started manually
        if (_gc_flag.load(std::memory_order_acquire) == (uint8_t)gc_state::stw) {
            t->transition(thd_state::need_start);
            _temp_roots.insert_or_assign(t, std::span<size_t>{ start_args, args_cnt });
        }

        // TODO: this is kinda inefficient, can we do better?
        t->get_mark_info().set_wbbuf(_marker.get_buf());
    });
    return t;
}

void collector::delete_tcb(tcb *t) {
    auto& mark_info = t->get_mark_info();
    mark_info.capture_ctx();
    flush_wbbuf(t);
    // Invariant: every live object must be reachable by the GC at all times - via
    // a pending request to a running thread or by reading a blocked thread's
    // resources. Setting a thread DEAD removes its resources from both paths, so
    // we hand any live objects off to global storage before the transition.
    auto& arena = t->get_arena();
    arena.flush_alloc_caches();
    arena.mutate_owned([&](pg_meta* start) {
        _pg_manager.transfer_ownership(start);
        return nullptr;
    });
    // Only attempt to leave after handing over resources
    _thd_state_mngr.thread_exit(t, [&](tcb* thd) { handle_pending(thd); });
    _tcb_registry.remove(t, [&]() {
        // Guaranteed to be a valid buf
        _marker.push_buf(mark_info.get_wbbuf());
        mark_info.set_wbbuf(nullptr);
    });
    // Safe to do since we removed it under lock
    rpfree(t);
    rpmalloc_thread_finalize(1);
}

void collector::flush_wbbuf(tcb *t) {
    auto& info = t->get_mark_info();
    _marker.flush_wbbuf(info.get_wbbuf());
}

void collector::register_root(size_t *root) {
    auto _ = std::scoped_lock { _root_mtx };
    _roots.push_back(root);
}

managed *collector::alloc(size_t sz, tcb * t) {
    auto& arena = t->get_arena();
    auto res = arena.alloc(sz, _pg_manager);
    if (!res) [[unlikely]] {
        force_collection(sz, t);
        return alloc(sz, t);
    }
    if (_gc_flag.load(std::memory_order_acquire) != (uint8_t)gc_state::none)
        pg_from_obj(res)->record_mark(res, false);
    // Only add size when allocation goes through
    auto debt = arena.get_debt();
    if (debt > config::debt_trigger) [[unlikely]] alloc_update(t, debt);
    return res;
}

void collector::process_pending(tcb *t) {
    auto& mark_info = t->get_mark_info();
    mark_info.capture_ctx();
    t->get_arena().flush_alloc_caches();
    flush_wbbuf(t);
    _thd_state_mngr.execute_pending(t, [&](tcb* thd) { handle_pending(thd); });
}
