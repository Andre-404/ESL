#pragma once
#include <atomic>
#include <cstdint>
#include <span>

#include "../gc-config.h"
#include "bump-allocator.h"

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
        bump_allocator _halves[2];
        std::atomic<uint8_t> _live;

        // Bitmaps are read and written a word at a time through atomic_ref<uint64_t>, so they are
        // handed out in whole words. The span's length is in words too
        static std::size_t words(std::size_t bits) { return (bits + 63) / 64; }

    public:
        gc_bits(uint8_t* base);
        gc_bits(const gc_bits&) = delete;
        gc_bits& operator=(const gc_bits&) = delete;

        std::span<uint64_t> alloc_bits(std::size_t bits) {
            auto h = _live.load(std::memory_order_acquire);
            return _halves[h].bump(words(bits), true);
        }

        std::span<uint64_t> mark_bits(std::size_t bits, bool should_zero = true) {
            auto h = _live.load(std::memory_order_acquire) ^ 1;
            return _halves[h].bump(words(bits), should_zero);
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
