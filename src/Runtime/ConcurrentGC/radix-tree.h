#pragma once
#include <cstdint>
#include <bit>
#include <cassert>
#include <span>
#include <algorithm>

namespace gc::detail {
    // A packed summary of a contiguous region of pages:
    //   start - length of the free run at the head of the region
    //   max   - length of the longest free run anywhere in the region
    //   end   - length of the free run at the tail of the region
    class summary {
    public:
        static constexpr uint32_t fanout     = 8;
        static constexpr uint32_t field_bits = 21;
        static constexpr uint32_t max_packed = 1u << field_bits;
        static constexpr uint32_t field_mask = max_packed - 1;
        static constexpr uint64_t full_bit   = 1ull << 63;

    private:
        uint64_t _data;

        static constexpr uint64_t pack(uint32_t start, uint32_t mx, uint32_t end) {
            assert(start <= max_packed && mx <= max_packed && end <= max_packed);
            // start/end reach max_packed only when the whole region is free, in
            // which case mx == max_packed too, so testing mx alone is enough.
            if (mx == max_packed) return full_bit;
            return  (uint64_t(start) & field_mask)
                 | ((uint64_t(mx)    & field_mask) << field_bits)
                 | ((uint64_t(end)   & field_mask) << (2 * field_bits));
        }

        constexpr uint32_t field(uint32_t shift) const {
            return is_full() ? max_packed : uint32_t((_data >> shift) & field_mask);
        }

        // Longest run of free (0) bits fully inside a word; edges are handled by
        // the caller's carry. Equivalent to the longest run of 1s in ~x.
        static constexpr uint32_t longest_free_run(uint64_t x) {
            uint64_t f = ~x;
            uint32_t best = 0;
            while (f) {
                f >>= std::countr_zero(f);            // skip allocated bits
                uint32_t run = std::countr_zero(~f);  // length of this free run (< 64)
                best = std::max(best, run);
                f >>= run;
            }
            return best;
        }

    public:
        constexpr summary() : _data(0) {}
        constexpr explicit summary(uint64_t data) : _data(data) {}
        constexpr summary(uint32_t start, uint32_t mx, uint32_t end) : _data(pack(start, mx, end)) {}

        constexpr bool     is_full() const { return _data & full_bit; }
        constexpr uint32_t start()   const { return field(0); }
        constexpr uint32_t max()     const { return field(field_bits); }
        constexpr uint32_t end()     const { return field(2 * field_bits); }
        constexpr uint64_t raw()     const { return _data; }

        constexpr bool operator==(const summary&) const = default;

        // Summary of a bitmap chunk. Convention: bit == 1 means allocated
        static constexpr summary summarize(std::span<const uint64_t> chunk) {
            constexpr uint32_t NOTSET = ~0u;
            uint32_t start = NOTSET, run = 0, mx = 0;
            for (uint64_t x : chunk) {
                if (x == 0) { run += 64; continue; }
                run += std::countr_zero(x); // free bits at the low end close the carried run
                if (start == NOTSET) start = run; // first allocated bit fixes the head run
                mx  = std::max({mx, run, longest_free_run(x)});
                run = std::countl_zero(x); // free bits at the high end carry forward
            }
            // no allocated bit: wholly free
            if (start == NOTSET) {
                uint32_t total = uint32_t(chunk.size()) * 64;
                return {total, total, total};
            }
            return {start, std::max(mx, run), run};
        }

        // Merge one parent's children. child_cover is the pages each child
        // covers; the parent covers fanout * child_cover.
        static constexpr summary merge(std::span<const summary, fanout> lower, uint32_t child_cover) {
            uint32_t start = 0, mx = 0, end = 0, covered = 0;
            for (summary s : lower) {
                // start grows while every prior child is wholly free
                if (start == covered) start += s.start();
                // max of: cur max, current end + this child start, this child interior max
                mx  = std::max({mx, end + s.start(), s.max()});
                // If the child is wholly free extend the end run, otherwise restart it
                end = (s.end() == child_cover) ? end + s.end() : s.end();
                covered += child_cover;
            }
            return {start, mx, end};
        }
    };

    class page_allocator {
        
    };
}