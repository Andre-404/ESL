#include "collector.h"

#include <iostream>
#include <ranges>

#include "../../Includes/fmt/core.h"
#include "../../Includes/rpmalloc/rpmalloc.h"

using namespace gc;
using namespace detail;

static size_t ms_diff(auto start) {
    return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count();
}

static collector* GC = nullptr;

enum gc_operation {
    op_mark_stack = 1,
    op_stw = 2
};

std::vector<tcb *> collector::post_with_state(gc_state s, uint8_t op)  {
    std::vector<tcb*> blocked;
    _tcb_registry.with_snapshot([&](auto& thds) {
        set_state(s);
        blocked = _synchronizer.post(std::views::all(thds), op);
    });
    return blocked;
}

void collector::force_collection(int64_t sz, tcb* t) {
    _collection_req.store(request_type::express);
    _collection_req.notify_all();

    int64_t snapshot = _heuristic.get_live_size() + _alloc_sz;
    set_paused(t);
    _collection_req.wait(request_type::express);
    set_resumed(t);
    if (snapshot - _heuristic.get_live_size() > sz) return;

    std::cerr
        <<"Thread "
        <<std::this_thread::get_id()
        <<fmt::format("failed to allocate {} bytes, heap size is {}, exiting...\n", sz, _alloc_sz);
    std::abort();
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
    auto& arena = t->get_arena();

    arena.flush_alloc_caches();
    arena.remove_debt(arena.get_debt());

    // Have to wait for everyone to flush their alloc caches before we can start accurate marking
    _synchronizer.ack();
    _synchronizer.wait_on_all_ack();
    auto copying_collection = should_force_stw() ? true : _heuristic.should_copy();

    phase1(t, copying_collection);

    while (_marker.trace_n(config::trace_batch * 1024)) {}
    _gate.arrive_and_wait();

    if (copying_collection) [[unlikely]] {
        arena.mutate_owned(copy_objs_fn());
        _gate.arrive_and_wait();
        arena.mutate_owned(update_ptrs_fn());
        _gate.arrive_and_wait();
    }

    arena.mutate_owned(prune_pgs_fn());
}

void collector::mark_phase() {
    auto blocked = post_with_state(gc_state::marking, op_mark_stack);
    for (auto t : blocked) phase1(t, false);
    _synchronizer.complete_handshake(blocked);
    _marker.scan_globals(_roots);
}

size_t collector::stw_phase() {
    auto blocked = post_with_state(gc_state::stw, op_stw);

    for (auto t : blocked) {
        auto& arena = t->get_arena();
        arena.flush_alloc_caches();
        arena.remove_debt(arena.get_debt());
        _synchronizer.ack();
    }
    // Have to wait for everyone to flush their alloc caches before we can start accurate marking
    _synchronizer.wait_on_all_ack();
    auto snapshot = _alloc_sz.load();
    auto copying_collection = should_force_stw() ? true : _heuristic.should_copy();
    set_state(gc_state::none); // Every thread has stopped at this point so this is safe to do
    // Needs to be set before threads start deallocating pages
    _pg_manager.set_empty_limit(snapshot * config::dead_commited_to_alloced_ratio);

    for (auto t : blocked) phase1(t, copying_collection);
    _marker.scan_globals(_roots);

    while (_marker.trace_n(config::trace_batch * 1024)) {}
    _gate.arrive_and_wait();
    _heuristic.mark_end();

    if (copying_collection) [[unlikely]] {
        auto copy_start = std::chrono::steady_clock::now();

        for (auto t : blocked) t->get_arena().mutate_owned(copy_objs_fn());
        _pg_manager.mutate_owned(copy_objs_fn());
        _gate.arrive_and_wait();

        for (auto t : blocked) t->get_arena().mutate_owned(update_ptrs_fn());
        _pg_manager.mutate_owned(update_ptrs_fn());
        _copier.update_globals(_roots);
        _gate.arrive_and_wait();

        _heuristic.set_copy_ms(ms_diff(copy_start));
    } else _heuristic.set_copy_ms(0);

    for (auto t : blocked) t->get_arena().mutate_owned(prune_pgs_fn());
    _pg_manager.mutate_owned(prune_pgs_fn());

    _tcb_registry.with_snapshot([&](auto& thds) {
        _synchronizer.finish_stw(std::views::all(thds));
    });

    return snapshot;
}

void collector::end_cycle(size_t alloc_snapshot) {
    _alloc_sz -= alloc_snapshot;
    _heuristic.calc(_pruner.get_ppg_frag(), alloc_snapshot, _pruner.get_live_size());
    _pruner.reset();

    auto old = _collection_req.load(std::memory_order_relaxed);
    if (old == request_type::end) return;
    // If some thread paused because it was out of memory wake it up AFTER all the calculations have been done
    _collection_req.compare_exchange_strong(old, request_type::none, std::memory_order_release);
    _collection_req.notify_all();
}


void collector::concurrent_loop() {
    rpmalloc_thread_initialize();
    while (true) {
        _pg_manager.dealloc_pgs();
        _pg_manager.foreach_active_pg([&](pg_meta* meta) { meta->clear_mark_bitmap(); });
        _collection_req.wait(request_type::none);
        if (_collection_req.load(std::memory_order_relaxed) == request_type::end) return;
        _heuristic.mark_start();
        mark_phase();

        while (_marker.trace_n(config::trace_batch)) if (should_force_stw()) break;
        _synchronizer.wait_on_all_ack();
        // Drain again after every thread has marked its stack
        while (_marker.trace_n(config::trace_batch)) if (should_force_stw()) break;

        _gate.register_waiter();
        auto snapshot = stw_phase();
        // This makes sure all other threads have left the stw phase
        _gate.arrive_and_wait();
        _gate.deregister_waiter();

        end_cycle(snapshot);
    }
    rpmalloc_thread_finalize(1);
}
// Stw only registers us as waiters and flushes the cache, then _synchronizer will ack for us
void collector::handle_pending(tcb* t) {
    auto op = t->get_opcode();
    if (op == op_mark_stack) {
        phase1(t, false);
        _synchronizer.ack();
    } else if (op == op_stw) {
        _gate.register_waiter();
        phase2(t);
        _gate.deregister_waiter();
    }
    else
        assert(false && "Unreachable");
}

[[gnu::cold]] void collector::alloc_update(tcb* t, size_t debt) {
    auto heap_sz = _alloc_sz.fetch_add(debt, std::memory_order_relaxed) + debt + _heuristic.get_live_size();
    if(heap_sz > _heuristic.heap_trigger()) {
        auto old = request_type::none;
        if (_collection_req.compare_exchange_strong(old, request_type::normal, std::memory_order_acq_rel))
            _collection_req.notify_all();
    }
    t->get_arena().remove_debt(debt);
}

void collector::thd_prologue(tcb *t) {
    rpmalloc_thread_initialize();
    _synchronizer.prologue(t);
}

void collector::set_paused(tcb *t) {
    t->get_mark_info().capture_ctx();
    flush_wbbuf(t);
    _synchronizer.enter_blocked(t, [&](tcb* thd) { handle_pending(thd); });
}

void collector::set_resumed(tcb *t) {
    _synchronizer.exit_blocked(t);
}

tcb *collector::create_tcb(size_t *start_args, uint8_t args_cnt) {
    auto t = new (rpmalloc(sizeof(tcb))) tcb { start_args, args_cnt };
    _tcb_registry.add(t, [&]() {
        // Threads created during stw will be paused and needs to be started manually
        if (_gc_flag.load(std::memory_order_acquire) == (uint8_t)gc_state::stw)
            t->transition(thd_state::need_start);
    });
    // TODO: this is kinda inefficient, can we do better?
    t->get_mark_info().set_wbbuf(_marker.get_buf());
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
        _pg_manager.release_pgs(start);
        return nullptr;
    });
    // Guaranteed to be a valid buf
    _marker.push_buf(mark_info.get_wbbuf());
    // Only attempt to leave after handing over resources
    _synchronizer.thread_exit(t, [&](tcb* thd) { handle_pending(thd); });
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
    if (_gc_flag.load(std::memory_order_relaxed) != (uint8_t)gc_state::none) [[unlikely]] {
        auto pg = pg_from_obj(res);
        pg->record_mark(res, false);
    }
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
    _synchronizer.execute_pending(t, [&](tcb* thd) { handle_pending(thd); });
}
