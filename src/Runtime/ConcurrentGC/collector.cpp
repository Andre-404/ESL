#include "collector.h"

#include <iostream>
#include <ranges>

#include "../../Includes/fmt/format.h"
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

void collector::mark_phase() {
    auto blocked = post_with_state(gc_state::marking, op_mark_stack);
    for (auto t : blocked) phase1(t, false);
    _synchronizer.complete_handshake(blocked);
}

void collector::stw_phase() {
    auto blocked = post_with_state(gc_state::stw, op_stw);
    bool copying_collection = _heuristic.should_copy();

    for (auto t : blocked) {
        auto& arena = t->get_arena();
        arena.flush_alloc_caches();
        arena.remove_debt(arena.get_debt());
        _synchronizer.ack();
    }
    // Have to wait for everyone to flush their alloc caches before we can start accurate marking
    _synchronizer.wait_on_all_ack();

    for (auto t : blocked) phase1(t, copying_collection);

    while (_marker.trace_n(config::trace_batch)) {}
    _gate.arrive_and_wait();

    if (copying_collection) [[unlikely]] {
        auto copy_start = std::chrono::steady_clock::now();

        for (auto t : blocked) t->get_arena().mutate_owned(copy_objs_fn());
        _pg_manager.mutate_owned(copy_objs_fn());
        _gate.arrive_and_wait();

        for (auto t : blocked) t->get_arena().mutate_owned(update_ptrs_fn());
        _pg_manager.mutate_owned(update_ptrs_fn());
        _gate.arrive_and_wait();

        _heuristic.set_copy_ms(ms_diff(copy_start));
    } else _heuristic.set_copy_ms(0);

    for (auto t : blocked) t->get_arena().mutate_owned(prune_pgs_fn());
    _pg_manager.mutate_owned(prune_pgs_fn());

    _tcb_registry.with_snapshot([&](auto& thds) {
        set_state(gc_state::none);
        _synchronizer.finish_stw(std::views::all(thds));
    });
}


[[noreturn]] void collector::concurrent_loop() {
    while (true) {
        _pg_manager.foreach_active_pg([&](pg_meta* meta) {
            meta->clear_mark_bitmap();
        });
        // TODO: implement waiting between gc cycles
        auto mark_start = std::chrono::steady_clock::now();
        mark_phase();

        while (_marker.trace_n(config::trace_batch)) {}
        _synchronizer.wait_on_all_ack();
        // Drain again after all acks
        while (_marker.trace_n(config::trace_batch)) {}

        _heuristic.set_mark_ms(ms_diff(mark_start));

        _gate.register_waiter();
        stw_phase();
        // This makes sure all other threads have left the stw phase
        _gate.arrive_and_wait();
        _gate.deregister_waiter();
        _pruner.reset();
        _heuristic.calc(_pruner.get_ppg_frag(), _alloc_sz, _pruner.get_live_size());
    }
}

void collector::force_collection(size_t sz) {

    std::cerr
        <<"Thread "
        <<std::this_thread::get_id()
        <<fmt::format("failed to allocate {} bytes, heap size is {}, exiting...\n", sz, _alloc_sz);
    std::abort();
}

void collector::set_state(gc_state new_state) {
    _gc_flag.store((uint8_t)new_state, std::memory_order_seq_cst);
    _gc_flag.notify_all();
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
            t->safe_transition(thd_state::need_start);
    });
    return t;
}

void collector::delete_tcb(tcb *t) {
    auto& mark_info = t->get_mark_info();
    mark_info.capture_ctx();
    // Invariant: every live object must be reachable by the GC at all times — via
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
}

void collector::flush_wbbuf(tcb *t) {
    auto& info = t->get_mark_info();
    _marker.push_buf(info.get_wbbuf());
    info.set_wbbuf(_marker.get_buf());
}

void collector::register_root(size_t *root) {
    _marker.register_root(root);
}

managed *collector::alloc(size_t sz, tcb * t) {
    auto& arena = t->get_arena();
    if (arena.get_debt() > config::debt_trigger) {
        if (_gc_flag.load(std::memory_order_acquire) == (uint8_t)gc_state::marking)
            arena.remove_debt(_marker.trace_n(config::debt_trigger));

    }
    auto res = arena.alloc(sz, _pg_manager);
    if (!res) [[unlikely]] {
        force_collection(sz);
        return alloc(sz, t);
    }
    _alloc_sz.fetch_add(sz, std::memory_order_relaxed);
    return res;
}

void collector::phase1(tcb* t, bool pin) {
    auto get_base = [&](size_t ptr) -> managed* {
        auto pg = _pg_manager.pg_from_ptr(ptr);
        if (!pg) return nullptr;
        return pg->from_interior((uint8_t*)ptr);
    };
    _marker.scan_stack(t->get_mark_info(), pin, get_base);
}

void collector::phase2(tcb* t) {
    auto& arena = t->get_arena();
    // TODO: implement deciding whether collection is copying or not
    bool copying_collection = _heuristic.should_copy();

    arena.flush_alloc_caches();
    arena.remove_debt(arena.get_debt());

    // Have to wait for everyone to flush their alloc caches before we can start accurate marking
    _synchronizer.ack();
    _synchronizer.wait_on_all_ack();

    phase1(t, copying_collection);

    while (_marker.trace_n(1 << 20)) {}
    _gate.arrive_and_wait();

    if (copying_collection) [[unlikely]] {
        arena.mutate_owned(copy_objs_fn());
        _gate.arrive_and_wait();
        arena.mutate_owned(update_ptrs_fn());
        _gate.arrive_and_wait();
    }

    arena.mutate_owned(prune_pgs_fn());
}

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

void collector::process_pending(tcb *handle) {
    _synchronizer.execute_pending(handle, [&](tcb* t) { handle_pending(t); });
}
