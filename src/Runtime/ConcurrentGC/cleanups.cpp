#include "cleanups.h"
#include "collector.h"
#include "marker.h"

#include "../../Includes/rpmalloc/rpmalloc.h"

using namespace gc::detail;

int64_t cleanups::add(managed* tracked, size_t cleanup, size_t arg, size_t external) {
    assert(tracked);
    // Constants compiled into the program live outside the heap and never die
    if (_state.load(std::memory_order_acquire) == state::drain || !heap_layout::in_heap(tracked))
        return 0;
    auto _ = std::scoped_lock { _live_mtx };
    auto& e = _live.emplace_back();
    store_ref(e.tracked, tracked);
    store_root(e.cleanup, cleanup);
    store_root(e.arg, arg);
    e.id = _next_id++;
    e.external = external;
    _external.fetch_add(external, std::memory_order_relaxed);
    return e.id;
}

// _live is in id order. Leaves a tombstone, erasing would move the vector under the lock on every close
bool cleanups::remove(int64_t id) {
    auto _ = std::scoped_lock { _live_mtx };
    auto it = std::ranges::lower_bound(_live, id, {}, &entry::id);
    if (it == _live.end() || it->id != id || !read_ref(it->tracked)) return false;
    store_ref(it->tracked, nullptr);
    store_raw(it->cleanup, size_t(0));
    store_raw(it->arg, size_t(0));
    _external.fetch_sub(it->external, std::memory_order_relaxed);
    return true;
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
// Must keep _live in order, remove's search depends on it
void cleanups::queue_dead() {
    auto _ = std::scoped_lock { _live_mtx, _pending_mtx };
    _external.store(0, std::memory_order_relaxed);
    size_t kept = 0, queued = _pending.size();
    for (auto& e : _live) {
        auto obj = read_ref(e.tracked);
        if (!obj) continue;
        if (!pg_meta::head_from_ptr(obj)->is_marked(obj)) _pending.push_back(e);
        else _live[kept++] = e;
    }
    _live.resize(kept);
    if (_pending.size() == queued) return;
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
