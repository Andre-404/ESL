#pragma once
#include <algorithm>
#include <atomic>
#include <deque>
#include <generator>
#include <mutex>
#include <optional>
#include <thread>
#include <vector>

#include "managed.h"

namespace gc::detail {
    class collector;

    class cleanups {
        // tracked is a weak ref, cleanup and arg are treated as roots. A null tracked was removed
        struct entry { managed* tracked; size_t cleanup, arg; int64_t id; size_t external; };
        enum class state { none, queued, drain };

        std::mutex _live_mtx;
        std::vector<entry> _live;
        int64_t _next_id = 1;   // under _live_mtx

        std::mutex _pending_mtx;
        std::deque<entry> _pending;

        std::atomic<state> _state;
        std::thread _runner;
        std::atomic<int64_t> _external { 0 };

        void runner(collector& gc);
    public:
        cleanups() : _state(state::none) {}
        // The entry's id, 0 when the call is ignored
        int64_t add(managed* tracked, size_t cleanup, size_t arg, size_t external = 0);
        // False when id isn't in _live: 0, removed, or queued because its object died
        bool remove(int64_t id);
        size_t external() const { return std::max<int64_t>(0, _external.load(std::memory_order_relaxed)); }
        // cleanup and arg of each entry, they are treated as roots
        // Holds a lock while iterating
        std::generator<size_t&> keepalive_slots();
        void queue_dead();
        // (cleanup, arg) of the oldest queued entry, healed
        std::optional<std::pair<size_t, size_t>> pop();
        // (registered, queued)
        std::pair<size_t, size_t> count();

        void run(collector& gc);
        // Stops the runner once it has run everything queued, add ignores new entries from here on
        // Not callable from a mutator, a cleanup may need a pause
        void drain();
    };
}
