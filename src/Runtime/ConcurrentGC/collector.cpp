#include "collector.h"

#include <atomic>
#include <iostream>
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

void collector::force_collection(int64_t sz, tcb* t) {
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

size_t collector::run_stw(std::span<tcb*> owned, bool is_worker) {
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

    // decision to copy happens after every thread has paused(wait_on_all_ack) so this is stable across all threads
    auto copying = _collection_req.is_express() ? true : _heuristic.should_copy();

    // TODO: this is the only big part of the code that is worker specific, can we move it out?
    auto snapshot = 0ull;
    if (is_worker) {
        snapshot = _alloc_sz.load();
        // Every thread has stopped at this point so this is safe to do
        _gc_flag.store((uint8_t)gc_state::none, std::memory_order_release);
    }

    {
        // TODO: this is wrong and only takes into account the mark pause time, not the concurrent mark time
        auto _ = _metrics.time(gc_metrics::phase::mark);
        for (auto t : owned) phase1(t, copying);
        if (is_worker) {
            auto get_base = [&](uint8_t* ptr) -> managed* {
                auto pg = _pg_manager.pg_from_ptr((uintptr_t)ptr);
                if (!pg) return nullptr;
                return pg->from_interior((uint8_t*)ptr);
            };
            for (auto [tcb, sp] : _temp_roots) _marker.scan_temp(sp, get_base);
        }

        while (_marker.trace_n(config::trace_batch * 1024)) {}
        _gate.arrive_and_wait();
    }
    if (is_worker) _cycle.mark_time = gc_clock::now().time_since_epoch() - _cycle.mark_time;

    auto for_each_owned = [&](auto f) {
        for (auto t : owned) t->get_arena().mutate_owned(f);
        if (is_worker) _pg_manager.mutate_owned(f);
    };

    if (copying) [[unlikely]] {
        auto _ = _metrics.time(gc_metrics::phase::copy);

        for_each_owned(copy_objs_fn());
        _gate.arrive_and_wait();

        for_each_owned(update_ptrs_fn());
        if (is_worker) _copier.update_globals(_roots);
        _gate.arrive_and_wait();
    }

    {
        auto _ = _metrics.time(gc_metrics::phase::sweep);
        for_each_owned(prune_pgs_fn());
    }

    return snapshot;
}

void collector::phase1(tcb* t, bool pin) {
    auto get_base = [&](uint8_t* ptr) -> managed* {
        auto pg = _pg_manager.pg_from_ptr((uintptr_t)ptr);
        if (!pg) return nullptr;
        return pg->from_interior((uint8_t*)ptr);
    };
    _marker.scan_stack(t->get_mark_info(), pin, get_base);
}

void collector::phase2(tcb* t) {
    auto _ = _metrics.time(gc_metrics::phase::pause);
    run_stw({ &t, 1 }, false);
}

void collector::mark_phase() {
    auto blocked = post_with_state(gc_state::marking, op_mark_stack);
    for (auto t : blocked) phase1(t, false);
    _thd_state_mngr.complete_handshake(blocked);
    _marker.scan_globals(_roots);
}

size_t collector::stw_phase() {
    auto _ = _metrics.time(gc_metrics::phase::pause);
    auto blocked = post_with_state(gc_state::stw, op_stw);
    auto snapshot = run_stw(blocked, true);
    _tcb_registry.with_snapshot([&](auto& thds){
        _thd_state_mngr.finish_stw(std::views::all(thds));
    });
    return snapshot;
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
        mark_phase();

        while (_marker.trace_n(config::trace_batch) && !_collection_req.is_express()) {}
        _thd_state_mngr.wait_on_all_ack();
        // Drain again after every thread has marked its stack
        while (_marker.trace_n(config::trace_batch) && !_collection_req.is_express()) {}

        _gate.register_waiter();
        auto snapshot = stw_phase();
        // This makes sure all other threads have left the stw phase
        _gate.arrive_and_wait();
        _gate.deregister_waiter();

        collect_metrics();
        end_cycle(snapshot);

        _pg_manager.free_pgs();

        _pg_manager.foreach_active_pg([&](pg_meta* meta) { meta->clear_mark_bitmap(); });
    }
    rpmalloc_thread_finalize(1);
}

// Stw only registers us as waiters and flushes the cache, then _synchronizer will ack for us
void collector::handle_pending(tcb* t) {
    auto op = t->get_opcode();
    if (op == op_mark_stack) {
        phase1(t, false);
        _thd_state_mngr.ack();
    } else if (op == op_stw) {
        _gate.register_waiter();
        phase2(t);
        _gate.deregister_waiter();
    }
    else
        assert(false && "Unreachable");
}

[[gnu::cold]] void collector::alloc_update(tcb* t, size_t debt) {
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
    // Guaranteed to be a valid buf
    _marker.push_buf(mark_info.get_wbbuf());
    mark_info.set_wbbuf(nullptr);
    // Only attempt to leave after handing over resources
    _thd_state_mngr.thread_exit(t, [&](tcb* thd) { handle_pending(thd); });
    _tcb_registry.remove(t);
    // Safe to do since we removed it under lock
    rpfree(t);
    rpmalloc_thread_finalize(1);
}

void collector::flush_wbbuf(tcb *t) {
    auto& info = t->get_mark_info();
    _marker.flush_wbbuf(info.get_wbbuf());
}

void collector::register_root(size_t *root) {
    _roots.push_back(root);
}

managed *collector::alloc(size_t sz, tcb * t) {
    auto& arena = t->get_arena();
    auto res = arena.alloc(sz, _pg_manager);
    if (!res) [[unlikely]] {
        force_collection(sz, t);
        return alloc(sz, t);
    }
    if (_gc_flag.load(std::memory_order_acquire) != (uint8_t)gc_state::none) [[unlikely]]
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
