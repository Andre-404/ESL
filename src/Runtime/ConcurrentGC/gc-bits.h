#pragma once
#include <atomic>
#include <cstdint>
#include <mutex>
#include <span>

#include "gc-config.h"

namespace gc::detail {
    //   live half    the alloc bitmaps the mutators are writing. A run created mid-cycle
    //                bumps its alloc bitmap from here, and it dies at the next flip like
    //                everything else in this half.
    //   spare half   this cycle's mark bitmaps, filled by the collector.
    //   flip()       the live half is now entirely garbage: reset its cursor and swap roles.
    //
    // The halves also self-compact. A run that dies simply stops asking for a bitmap, so each
    // cycle re-packs the survivors densely and in the order the sweep hands them out - which
    // is the order the next sweep will read them back in.
    class gc_bits {
        // When allocating a new page we request both alloc and mark bitmaps at the same time
        // For that reason i think its fine for both halves to share a cacheline
        struct half {
            std::atomic<std::size_t> cursor { 0 };
            std::atomic<std::size_t> committed { 0 };
        };

        uint8_t* _base;
        half _halves[2];
        // Card bitmaps are copies of mark bitmap, so no need to track cursor
        std::atomic<std::size_t> _card_committed;
        std::atomic<uint8_t> _live;
        std::mutex _mtx;

        uint8_t* half_base(uint8_t h) const { return _base + std::size_t(h) * config::bits_arena_sz; }
        uint8_t* card_base() const { return _base + config::bits_region_sz; }
        bool ensure_committed(std::atomic<std::size_t>& committed, uint8_t* base, std::size_t end);
        std::span<uint64_t> bump(half& h, uint8_t* base, std::size_t bits, bool should_zero, bool cards);

    public:
        gc_bits(uint8_t* base);
        gc_bits(const gc_bits&) = delete;
        gc_bits& operator=(const gc_bits&) = delete;

        std::span<uint64_t> alloc_bits(std::size_t bits) {
            auto h = _live.load(std::memory_order_acquire);
            return bump(_halves[h], half_base(h), bits, true, false);
        }

        std::span<uint64_t> mark_bits(std::size_t bits, bool should_zero = true) {
            auto h = _live.load(std::memory_order_acquire) ^ 1;
            return bump(_halves[h], half_base(h), bits, should_zero, true);
        }

        // Must run at a safepoint: it invalidates every pointer handed out of the alloc half
        void flip();
        // During the sweep phase each page allocates new mark bitmap
        // Instead of zeroing per page, record how much space has been taken up and then zero it once
        void clear_mark(uint64_t* watermark);

        std::size_t used() const;
        static constexpr std::size_t capacity() { return config::bits_arena_sz; }
    };
}
