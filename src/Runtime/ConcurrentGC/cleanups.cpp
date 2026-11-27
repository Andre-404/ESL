#include "cleanups.h"
#include "collector.h"
#include "marker.h"

#include "../../Includes/rpmalloc/rpmalloc.h"

using namespace gc::detail;

void cleanups::add(managed* tracked, size_t cleanup, size_t arg) {
    assert(tracked);
    // Constants compiled into the program live outside the heap and never die
    if (_state.load(std::memory_order_acquire) == state::drain || !heap_layout::in_heap(tracked))
        return;
    auto _ = std::scoped_lock { _live_mtx };
    auto& e = _live.emplace_back();
    store_ref(e.tracked, tracked);
    store_root(e.cleanup, cleanup);
    store_root(e.arg, arg);
}

std::generator<size_t&> cleanups::keepalive_slots() {
    {
        auto _ = std::scoped_lock { _live_mtx };
        for (auto& e : _live) { co_yield e.cleanup; co_yield e.arg; }
    }
    auto _ = std::scoped_lock { _pending_mtx };
    for (auto& e : _pending) { co_yield e.cleanup; co_yield e.arg; }
}

// Runs in the stw pause, the locks are for count
void cleanups::queue_dead() {
    auto _ = std::scoped_lock { _live_mtx, _pending_mtx };
    size_t kept = 0;
    for (auto& e : _live) {
        auto obj = read_ref(e.tracked);
        if (!pg_meta::head_from_ptr(obj)->is_marked(obj)) _pending.push_back(e);
        else _live[kept++] = e;
    }
    if (kept == _live.size()) return;
    _live.resize(kept);
    auto expected = state::none;
    if (_state.compare_exchange_strong(expected, state::queued, std::memory_order_release, std::memory_order_relaxed))
        _state.notify_one();
}

std::optional<std::pair<size_t, size_t>> cleanups::pop() {
    auto _ = std::scoped_lock { _pending_mtx };
    if (_pending.empty()) return std::nullopt;
    auto& e = _pending.front();
    // A word that isn't a reference was stored as it came
    auto word = [](size_t& slot) { auto obj = read_root(slot); return obj ? ptr_to_word(obj) : load_raw(slot); };
    auto res = std::pair { word(e.cleanup), word(e.arg) };
    _pending.pop_front();
    return res;
}

std::pair<size_t, size_t> cleanups::count() {
    auto _ = std::scoped_lock { _live_mtx, _pending_mtx };
    return { _live.size(), _pending.size() };
}

void cleanups::run(collector& gc) {
    _runner = std::thread { &cleanups::runner, this, std::ref(gc) };
}

// Blocked while it waits, so it never holds up a pause. Once woken it runs everything queued as a
// running mutator, polling between cleanups, and only then waits again
void cleanups::runner(collector& gc) {
    rpmalloc_thread_initialize();
    auto t = gc.create_tcb(nullptr, 0);
    t->get_mark_info().track_stack((size_t)__builtin_frame_address(0));
    gc.thd_prologue(t);
    while (true) {
        gc.run_blocked(t, [&] { _state.wait(state::none, std::memory_order_acquire); });
        auto _ = state::queued;
        _state.compare_exchange_strong(_, state::none, std::memory_order_relaxed);
        while (auto e = pop()) {
            invoke_cleanup(t, e->first, e->second);
            gc.poll(t);
        }
        if (_state.load(std::memory_order_acquire) == state::drain) break;
    }
    gc.delete_tcb(t);
}

void cleanups::drain() {
    _state.store(state::drain, std::memory_order_release);
    _state.notify_one();
    if (_runner.joinable()) _runner.join();
}
