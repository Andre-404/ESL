#pragma once
#include <atomic>
#include <bit>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <new>
#include <utility>

#include "../gc-config.h"
#include "os-memory.h"

namespace gc::detail {
    // One entry per heap page saying where the survivors of a compacted page went. Belongs to
    // the address range, not the page: committed with the pg headers, never decommitted, and
    // zeroed once the marking after the copy has healed every pointer into the old pages
    class reloc_hdrs {
        // Writes are tracked one bit per run (a cache line of entries),
        // plus one summary bit per word of those so clear skips untouched ranges
        static constexpr std::size_t run_pgs = config::small_obj_pgs;
        static constexpr std::size_t run_words = config::total_pages / run_pgs / 64;
        static constexpr std::size_t sum_words = run_words / 64;
        static_assert(run_pgs * config::reloc_hdr_sz == 64);
        static_assert(config::total_pages % (run_pgs * 64 * 64) == 0);

        static constexpr std::size_t track_bytes = (run_words + sum_words) * sizeof(uint64_t);

        uint64_t* _entries;
        uint64_t* _runs;
        uint64_t* _sums;

    public:
        explicit reloc_hdrs(uint8_t* base)
            : _entries((uint64_t*)base), _runs((uint64_t*)os::map_rw(track_bytes)), _sums(_runs + run_words) {
            if (!_runs) throw std::bad_alloc{};
        }
        ~reloc_hdrs() { os::release(_runs, track_bytes); }
        reloc_hdrs(const reloc_hdrs&) = delete;
        reloc_hdrs& operator=(const reloc_hdrs&) = delete;

        bool commit(std::size_t from, std::size_t to) {
            return os::commit(_entries + from, (to - from) * config::reloc_hdr_sz);
        }

        uint64_t get(std::size_t pg) const { return _entries[pg]; }
        void set(std::size_t pg, uint64_t entry) {
            assert(entry && !_entries[pg] && "a page is compacted at most once per cycle");
            _entries[pg] = entry;

            auto run = pg / run_pgs;
            auto bit = 1ull << (run % 64);
            auto w = std::atomic_ref { _runs[run / 64] };
            if (w.load(std::memory_order_relaxed) & bit) return;
            if (w.fetch_or(bit, std::memory_order_relaxed) == 0)
                std::atomic_ref { _sums[run / 4096] }.fetch_or(1ull << (run / 64 % 64), std::memory_order_relaxed);
        }

        // A no-op unless the previous cycle copied. Must not race with set
        void clear() {
            for (std::size_t s = 0; s < sum_words; ++s) {
                for (auto sw = std::exchange(_sums[s], 0); sw; sw &= sw - 1) {
                    auto wi = s * 64 + std::countr_zero(sw);
                    for (auto rw = std::exchange(_runs[wi], 0); rw; rw &= rw - 1) {
                        auto run = wi * 64 + std::countr_zero(rw);
                        memset(_entries + run * run_pgs, 0, run_pgs * config::reloc_hdr_sz);
                    }
                }
            }
        }
    };
}
