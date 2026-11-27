#pragma once
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
        // tracked is a weak ref, cleanup and arg are treated as roots
        struct entry { managed* tracked; size_t cleanup, arg; };
        enum class state { none, queued, drain };

        std::mutex _live_mtx;
        std::vector<entry> _live;

        std::mutex _pending_mtx;
        std::deque<entry> _pending;

        std::atomic<state> _state;
        std::thread _runner;

        void runner(collector& gc);
    public:
        cleanups() : _state(state::none) {}
        void add(managed* tracked, size_t cleanup, size_t arg);
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
