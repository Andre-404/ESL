#pragma once

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <mutex>
#include <span>

#include "../gc-config.h"
#include "../os/os-memory.h"

namespace gc::detail {

    // Bump allocator that assumes the virtual addr space has been reserved for it and only commits
    // Never decommits
    // Allocation unit is 1 word
    class bump_allocator {
        std::atomic<std::size_t> _cursor;
        std::atomic<std::size_t> _committed;
        std::mutex _mtx;
        uint8_t* _base;
        std::size_t _max_sz;

        static constexpr std::size_t round_up(std::size_t n, std::size_t to) {
            return (n + to - 1) / to * to;
        }

        bool ensure_committed(std::size_t end) {
            auto lk = std::lock_guard { _mtx };
            // Another thread may have covered this range while we waited for the lock.
            auto from = _committed.load(std::memory_order_relaxed);
            if (end <= from) return true;

            auto to = round_up(end, config::commit_syscall_sz);
            if (!os::commit(_base + from, to - from)) return false;

            // Release, paired with the acquire in bump(): a thread that skips the lock because it
            // sees this value must also see the range as backed.
            _committed.store(to, std::memory_order_release);
            return true;
        }
    public:
        bump_allocator(uint8_t* base, std::size_t max_sz) : _cursor(0), _committed(0), _base(base), _max_sz(max_sz) {
            assert((std::size_t)base % config::commit_syscall_sz == 0 && max_sz % config::commit_syscall_sz == 0
                && "commits are rounded up to commit_syscall_sz, so the region has to be made of whole ones");
        }
        bump_allocator(const bump_allocator&) = delete;
        bump_allocator& operator=(const bump_allocator&) = delete;

        // TODO: fail loudly here
        std::span<uint64_t> bump(std::size_t words, bool should_zero) {
            if (words == 0) return {};
            auto n = words * sizeof(uint64_t);

            auto off = _cursor.fetch_add(n, std::memory_order_relaxed);
            if (off + n > _max_sz) [[unlikely]] return {};

            if (off + n > _committed.load(std::memory_order_acquire)
                && !ensure_committed(off + n)) [[unlikely]] return {};

            auto p = _base + off;
            if (should_zero) memset(p, 0, n);
            return { (uint64_t*)p, words };
        }

        // Everything handed out so far is garbage after this, and nothing may bump concurrently
        void reset() { _cursor.store(0, std::memory_order_relaxed); }

        uint8_t* base() const { return _base; }
        std::size_t used() const { return std::min(_cursor.load(std::memory_order_relaxed), _max_sz); }
    };
}