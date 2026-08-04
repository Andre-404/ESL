#pragma once
#include <cstddef>
#include <cstdint>

// Thin wrappers over the platform's virtual memory calls, kept free of any allocator
// policy so that anything needing a reservation can share them.
//
// The model is reserve/commit/release. A reservation is address space with nothing
// behind it; commit and decommit move physical pages in and out of a subrange without
// ever handing the range back, so an address stays valid for the whole lifetime of the
// reservation and only release unmaps. That split is what lets an allocator hand out
// stable pointers into a range far larger than it will ever back.
//
// On POSIX the split is mostly notional: an anonymous MAP_NORESERVE mapping already
// faults pages in on first touch, so commit is a no-op and decommit is MADV_DONTNEED.
// Windows requires the MEM_COMMIT/MEM_DECOMMIT pair to be explicit, which is what the
// interface is shaped around.
namespace gc::detail::os {
    // Address space only, nothing backing it. `align` must be a power of two and is
    // honoured on both platforms, though neither OS takes an alignment argument: both
    // paths over-reserve and give the excess back, so the returned range is exactly
    // `bytes` long and release still hands back one range.
    void* reserve(std::size_t bytes, std::size_t align);

    // Reserved and backed in one step. For small fixed structures that are touched
    // immediately and never grow, so there is nothing for a commit cursor to do.
    // Carries no alignment guarantee.
    void* map_rw(std::size_t bytes);

    // Both take a subrange of an existing reservation. Neither invalidates the address:
    // touching a decommitted page faults a fresh zero page back in.
    bool commit(void* addr, std::size_t bytes);
    bool decommit(void* addr, std::size_t bytes);

    // Gives the address space itself back. `p` must be what reserve or map_rw returned,
    // and `bytes` its full length (ignored on Windows, which tracks the size itself).
    void release(void* p, std::size_t bytes);

    // Cuts `n` T's off the front of a raw block and advances the cursor past them, for
    // laying several arrays out back to back inside a single mapping. The caller is
    // responsible for having sized the block to match, which is worth asserting: run
    // the cursor to the end and check it against the length that was mapped.
    template<typename T>
    T* carve(uint8_t*& cursor, std::size_t n) {
        auto p = reinterpret_cast<T*>(cursor);
        cursor += n * sizeof(T);
        return p;
    }
}
