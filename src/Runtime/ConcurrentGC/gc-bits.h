#pragma once
#include <atomic>
#include <cstdint>
#include <mutex>

#include "gc-config.h"

namespace gc::detail {
    // Side storage for the per-run alloc and mark bitmaps.
    //
    // Keeping bitmaps out of the heap is what lets a sweep avoid touching the heap at all.
    // A header read per page is a dependent load into a cold page and a TLB miss to go with
    // it; reading a dense array instead turns the same walk into a stream.
    //
    // The lifetime rule that shapes the allocator is that sweeping is `alloc = mark`. A cycle
    // marks into a fresh bitmap, and at the flip that mark bitmap *becomes* the alloc bitmap
    // with no copy, which leaves the previous alloc bitmap dead. Dead all at once, for every
    // run in the heap - so there is never an individual free, and a bump pointer is the whole
    // allocator:
    //
    //   live half    the alloc bitmaps the mutators are writing. A run created mid-cycle
    //                bumps its alloc bitmap from here, and it dies at the next flip like
    //                everything else in this half.
    //   spare half   this cycle's mark bitmaps, filled by the collector.
    //   flip()       the live half is now entirely garbage: reset its cursor and swap roles.
    //
    // The halves also self-compact. A run that dies simply stops asking for a bitmap, so each
    // cycle re-packs the survivors densely and in the order the sweep hands them out - which
    // is the order the next sweep will read them back in.
    //
    // Handing memory out zeroed is deliberate. Both an alloc bitmap for a new run and a mark
    // bitmap for a cycle have to start empty, and doing it here - a few hundred bytes against
    // a freshly bumped, sequential cursor - is strictly cheaper than clearing a half at reset,
    // which would put a memset proportional to the whole live heap inside the pause.
    class gc_bits {
        // When allocating a new page we request both alloc and mark bitmaps at the same time
        // For that reason i think its fine for both halves to share a cacheline
        struct half {
            std::atomic<std::size_t> cursor { 0 };
            std::atomic<std::size_t> committed { 0 };
        };

        uint8_t* _base;
        half _halves[2];
        std::atomic<uint8_t> _live;
        std::mutex _mtx;

        uint8_t* half_base(uint8_t h) const { return _base + std::size_t(h) * config::bits_arena_sz; }
        bool ensure_committed(half& h, uint8_t* base, std::size_t end);
        uint8_t* bump(half& h, uint8_t* base, std::size_t bits);

    public:
        gc_bits();
        ~gc_bits();
        gc_bits(const gc_bits&) = delete;
        gc_bits& operator=(const gc_bits&) = delete;

        uint8_t* alloc_bits(std::size_t bits) {
            auto h = _live.load(std::memory_order_acquire);
            return bump(_halves[h], half_base(h), bits);
        }

        uint8_t* mark_bits(std::size_t bits) {
            auto h = _live.load(std::memory_order_acquire) ^ 1;
            return bump(_halves[h], half_base(h), bits);
        }

        // Must run at a safepoint: it invalidates every pointer handed out of the alloc half
        void flip();

        // Bytes bumped in the fuller half, for pressure checks. Saturates at capacity.
        std::size_t used() const;
        static constexpr std::size_t capacity() { return config::bits_arena_sz; }
    };
}
