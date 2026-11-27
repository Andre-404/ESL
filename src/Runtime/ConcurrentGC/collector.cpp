#include "collector.h"

#include <atomic>
#include <iostream>
#include <mutex>
#include <ranges>
#include <tuple>

#include "../../Includes/fmt/core.h"
#include "../../Includes/rpmalloc/rpmalloc.h"
#include "gc-heuristic.h"
#include "platform-specific.h"

using namespace gc;
using namespace detail;

enum gc_operation : uint8_t {
    op_mark_stack = 1,
    op_stw = 2,
};

std::vector<tcb *> collector::post_with_state(uint8_t flags, uint8_t op)  {
    std::vector<tcb*> blocked;
    _tcb_registry.with_snapshot([&](auto& thds) {
        _gc_flag.store(flags, std::memory_order_release);
        blocked = _thd_state_mngr.post(std::views::all(thds), op);
    });
    return blocked;
}

[[gnu::cold, clang::noinline]] managed* collector::force_collection(size_t sz, tcb* t) {
    for (int attempt = 0; attempt < 2; attempt++) {
        _collection_req.request_express();

        run_blocked(t, [&] { _collection_req.await_express_served(); });
        if (auto res = t->get_arena().alloc(sz, _pg_manager)) return res;
    }

    std::cerr << "Thread " << std::this_thread::get_id()
        << fmt::format(" failed to allocate {} bytes with {} bytes live, exiting...\n", sz, _heuristic.live_size());
    std::abort();
}

void collector::stw_enter(std::span<tcb*> owned, stw_role role) {
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
    if (role == stw_role::collector) _pg_manager.bits().flip();
}

size_t collector::stw_mark(std::span<tcb*> owned, stw_role role) {
    auto _ = _metrics.time(gc_metrics::phase::mark);
    for (auto t : owned)
        _marker.scan_words({ t->get_mark_info().get_stack() }, true, get_obj_base());

    size_t snapshot = 0;
    if (role == stw_role::collector) {
        // Safe to scan roots because all mutators are stopped
        for (auto [tcb, sp] : _temp_roots)
            _marker.scan_words({ sp }, true, get_obj_base());
        _marker.scan_globals(_roots);
        _marker.scan_roots(_cleanups.keepalive_slots());
        snapshot = _alloc_sz.load();
        // Every thread has stopped at this point so this is safe to do
        _gc_flag.store((uint8_t)gc_state::none, std::memory_order_release);
    }

    while (_marker.trace_n(config::trace_batch * 1024)) {}
    _gate.arrive_and_wait();
    return snapshot;
}

void collector::stw_copy(std::span<tcb*> owned, stw_role role) {
    auto _ = _metrics.time(gc_metrics::phase::copy);

    // Gather: every thread offers the lists it owns
    mutate_all(owned, role, gather_fn());
    _gate.arrive_and_wait();

    if (role == stw_role::collector) {
        // An express collection has a mutator blocked out of memory, so it takes all gather found
        auto budget = _express ? SIZE_MAX : _heuristic.copy_budget_bytes();
        _copier.select(budget, _express ? 1 : _heuristic.min_copy_bytes(), _gate.expected());
    }
    _gate.arrive_and_wait();
    if (!_copier.has_plans()) return;

    {
        auto _ = _metrics.time(gc_metrics::phase::compact);
        while (auto plan = _copier.take_plan()) _copier.compact(*plan, _pg_manager.relocs());
    }

    if (role == stw_role::collector) _good_color.fetch_xor(config::color_bit, std::memory_order_relaxed);
    _gate.arrive_and_wait();
    reload_color();
}

void collector::stw_prune(std::span<tcb*> owned, stw_role role) {
    auto _ = _metrics.time(gc_metrics::phase::sweep);
    mutate_all(owned, role, prune_pgs_fn());
    _gate.arrive_and_wait();
}

size_t collector::stw(std::span<tcb*> owned, stw_role role) {
    stw_enter(owned, role);
    // Every thread has stopped, the pause before this is the wait for them
    auto _ = _metrics.time(gc_metrics::phase::stw);
    auto snapshot = stw_mark(owned, role);
    // Must run before relocs are cleared (need to update tracked)
    // And must also run before we try to copy (copy pass can potentially overwrite mark bit of dead obj)
    if (role == stw_role::collector) {
        _verifier.after_mark();
        _cleanups.queue_dead();
        _pg_manager.relocs().clear();
    }
    _gate.arrive_and_wait();
    if (_heuristic.wants_copy() || _express) [[unlikely]] stw_copy(owned, role);
    stw_prune(owned, role);
    return snapshot;
}

void collector::concurrent_mark() {
    auto _ = _metrics.time(gc_metrics::phase::concurrent_mark);
    auto blocked = post_with_state(uint8_t(gc_state::marking), op_mark_stack);
    _mark_alloc_start = _alloc_sz.load(std::memory_order_relaxed);
    for (auto t : blocked)
        _marker.scan_words({ t->get_mark_info().get_stack() }, false, get_obj_base());
    _thd_state_mngr.complete_handshake(blocked);
    _thd_state_mngr.wait_on_all_ack();
    // Must be under lock because we can be adding global variables dynamically
    // (need to fix this honestly and add every global as root on start of program)
    {
        auto _ = std::scoped_lock { _root_mtx };
        _marker.scan_globals(_roots);
    }
    _marker.scan_roots(_cleanups.keepalive_slots());

    while (_marker.trace_n(config::trace_batch) && !_collection_req.is_express()) {}
}

size_t collector::worker_stw() {
    auto _ = _metrics.time(gc_metrics::phase::pause);
    _express = _collection_req.is_express();
    auto blocked = post_with_state(uint8_t(gc_state::stw), op_stw);
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
    auto t = _metrics.collect_cycle();
    _cycle.mark_time = t.mark;
    _cycle.compact_time = t.compact;
}

gc_stats collector::stats() {
    auto s = _metrics.snapshot();
    s.live_bytes = _heuristic.live_size();
    s.heap_trigger = _heuristic.heap_trigger();
    s.heap_in_use_bytes = _pg_manager.in_use_bytes();
    s.heap_resident_bytes = _pg_manager.resident_bytes();
    s.threads = _tcb_registry.size();
    std::tie(s.cleanups_registered, s.cleanups_queued) = _cleanups.count();
    return s;
}

uint64_t* collector::end_cycle(size_t alloc_snapshot) {
    _alloc_sz -= alloc_snapshot;
    _cycle.allocated_bytes = alloc_snapshot;
    _cycle.live_bytes      = _pruner.live_bytes();
    _cycle.black_bytes     = alloc_snapshot > _mark_alloc_start ? alloc_snapshot - _mark_alloc_start : 0;
    _cycle.evac_gain_bytes = _pruner.evac_gain_bytes();
    _heuristic.end_cycle(gc_clock::now(), _cycle);
    return _pruner.end_cycle();
}


void collector::concurrent_loop() {
    rpmalloc_thread_initialize();
    reload_color();
    while (true) {
        if (_collection_req.await_request()) break;
        _cycle = {};
        concurrent_mark();

        _gate.register_waiter();
        auto snapshot = worker_stw();
        // This makes sure all other threads have left the stw phase
        _gate.arrive_and_wait();
        _gate.deregister_waiter();

        collect_metrics();
        _cycle.copied_bytes = _copier.end_copy();

        auto watermark = end_cycle(snapshot);

        _pg_manager.bits().clear_mark(watermark);
        _pg_manager.free_pgs();

        _tcb_registry.under_lock([&] {
            assert(_gc_flag.load(std::memory_order_acquire) == 0);
            _marker.remove_empty();
        });
        _metrics.publish(_cycle);

        // If threads are waiting because they request an express collection,
        // wake them after pages have been freed
        _collection_req.clear(_express);
    }
    rpmalloc_thread_finalize();
}

// For op_stw this thread joins the gate, and stw_enter acks once it has flushed its caches
void collector::handle_pending(tcb* t) {
    auto op = t->get_opcode();
    if (op == op_mark_stack) {
         _marker.scan_words({ t->get_mark_info().get_stack() }, false, get_obj_base());
        _thd_state_mngr.ack();
    } else if (op == op_stw) {
        _gate.register_waiter();
        mutator_stw(t);
        _gate.deregister_waiter();
    }
    else
        assert(false && "Unreachable");
}

void collector::request_if_over(size_t alloc) {
    if (alloc + _cleanups.external() + _heuristic.live_size() > _heuristic.heap_trigger())
        _collection_req.request_normal();
}

[[gnu::cold, clang::noinline]] void collector::alloc_update(tcb* t, size_t debt) {
    request_if_over(_alloc_sz.fetch_add(debt, std::memory_order_relaxed) + debt);
    t->get_arena().remove_debt(debt);
}

int64_t collector::add_cleanup(managed* tracked, size_t cleanup, size_t arg, size_t external) {
    auto id = _cleanups.add(tracked, cleanup, arg, external);
    if (id && external) request_if_over(_alloc_sz.load(std::memory_order_relaxed));
    return id;
}

void collector::thd_prologue(tcb *t) {
    rpmalloc_thread_initialize();
    _thd_state_mngr.prologue(t);
    _tcb_registry.under_lock([&](){ _temp_roots.erase(t); });
    reload_color();
}

void collector::block(tcb *t) {
    flush_wbbuf(t);
    _thd_state_mngr.enter_blocked(t, [&](tcb* thd) { handle_pending(thd); });
}

void collector::run_blocked(tcb *t, function_ref<void() const> f) {
    with_stack_end(t, [&] {
        block(t);
        f();
        _thd_state_mngr.exit_blocked(t);
    });
}

void collector::with_stack_end(tcb *t, function_ref<void() const> f) {
    struct call { tcb* t; function_ref<void() const> f; } c { t, f };
    platform::with_regs_pushed(&c, [](void* arg, size_t* sp) {
        auto& c = *static_cast<call*>(arg);
        c.t->get_mark_info().set_stack_end(sp);
        c.f();
    });
    // Right now we store the pointer color in a register so we have to do this dance
    // to make sure the right color is in the register
    reload_color();
}

tcb *collector::create_tcb(size_t *start_args, uint8_t args_cnt) {
    auto t = new (rpmalloc(sizeof(tcb))) tcb { start_args, args_cnt };
    _tcb_registry.add(t, [&]() {
        // Threads created during stw will be paused and needs to be started manually
        if (state_is(gc_state::stw)) {
            t->transition(thd_state::need_start);
            _temp_roots.insert_or_assign(t, std::span<size_t>{ start_args, args_cnt });
        }

        t->get_mark_info().set_wbbuf(_marker.get_buf());
    });
    return t;
}

void collector::delete_tcb(tcb *t) {
    auto& mark_info = t->get_mark_info();
    flush_wbbuf(t);
    // Invariant: every live object must be reachable by the GC at all times - via
    // a pending request to a running thread or by reading a blocked thread's
    // resources. Setting a thread DEAD removes its resources from both paths, so
    // we hand any live objects off to global storage before the transition.
    auto& arena = t->get_arena();
    arena.flush_alloc_caches();
    // Debt under debt_trigger would die with the thread, and short-lived threads would never trigger a cycle
    if (auto debt = arena.get_debt(); debt > 0) alloc_update(t, debt);
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
    rpmalloc_thread_finalize();
}

void collector::flush_wbbuf(tcb *t) {
    auto& info = t->get_mark_info();
    _marker.flush_wbbuf(info.get_wbbuf());
}

void collector::register_root(size_t *root) {
    auto _ = std::scoped_lock { _root_mtx };
    _roots.push_back(root);
}

void collector::collect(tcb* t) {
    run_blocked(t, [&] { _collection_req.await_cycle(); });
}

managed *collector::alloc(size_t sz, tcb * t) {
    assert(t->is_running() && "a blocked thread can't touch the heap");
    auto& arena = t->get_arena();
    auto res = arena.alloc(sz, _pg_manager);
    if (!res) [[unlikely]] res = force_collection(sz, t);
    if (!state_is(gc_state::none))
        pg_meta::head_from_ptr(res)->record_mark(res);
    // Only add size when allocation goes through
    auto debt = arena.get_debt();
    if (debt > config::debt_trigger) [[unlikely]] alloc_update(t, debt);
    return res;
}

void collector::process_pending(tcb *t) {
    with_stack_end(t, [&] {
        t->get_arena().flush_alloc_caches();
        flush_wbbuf(t);
        _thd_state_mngr.execute_pending(t, [&](tcb* thd) { handle_pending(thd); });
    });
}
