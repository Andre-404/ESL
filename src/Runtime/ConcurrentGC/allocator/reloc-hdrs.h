#pragma once
#include <algorithm>
#include <atomic>
#include <bit>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <new>
#include <span>
#include <utility>

#include "../gc-config.h"
#include "bump-allocator.h"
#include "heap-layout.h"
#include "os-memory.h"

namespace gc::detail {
    // Covers 64 blocks of an old run, the same 64 as a mark word: which of them survived, and where
    // the first one went. The rest follow it in order, one block apart
    struct reloc_unit {
        uint64_t live;
        uint8_t* dest;
    };
    static_assert(sizeof(reloc_unit) == config::reloc_unit_sz);

    namespace reloc {
        constexpr std::size_t run_sz = config::small_obj_pgs * config::page_sz;
        constexpr std::size_t split_none = run_sz / 16;
        constexpr std::size_t run_units = split_none / 64;
        constexpr std::size_t max_table_units = run_units + 1;

        // A run's reloc hdr. table is its units' offset into the arena
        struct entry {
            uint32_t block_sz, split, table;
        };
        // Never zero, a block is at least 16 bytes
        constexpr uint64_t pack(entry e) { return e.block_sz | e.split << 12 | uint64_t(e.table) << 32; }
        constexpr entry unpack(uint64_t w) { return { uint32_t(w & 0xFFF), uint32_t(w >> 12 & 0xFFFFF), uint32_t(w >> 32) }; }
        static_assert(split_none <= 0xFFFFF && config::sz_classes[config::szclass_cnt - 1] <= 0xFFF);

        constexpr std::size_t unit_of(std::size_t pos, std::size_t split) { return pos / 64 + (pos >= split); }

        // p must be an object that survived. Compacted runs are aligned to their size, so masking
        // finds the run, whose entry sits at its first page
        constexpr uint8_t* forward(uint8_t* p) {
            auto off = uintptr_t(p - heap_layout::heap(p));
            auto e = ((const uint64_t*)heap_layout::relocs(p))[off / run_sz * config::small_obj_pgs];
            if (!e) return p;

            auto [size, split, ustart] = unpack(e);
            auto units = (const reloc_unit*)heap_layout::reloc_units(p) + ustart;
            auto pos = uint32_t(off % run_sz) / size;
            auto bit = pos & 63;
            auto u = units[unit_of(pos, split)];
            assert((u.live >> bit & 1) && "forwarding a pointer to an object that did not survive");
            return u.dest + std::popcount(u.live & ((1ull << bit) - 1)) * size;
        }
    }

    // One slot per heap page, a compacted run's first page holds the entry saying where its survivors
    // went. Belongs to the address range, not the page: committed with the pg headers, never
    // decommitted, and zeroed once the marking after the copy has healed every pointer into the old pages
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
        bump_allocator _units;

        // A table is stored reduced by the unit of its run's first survivor, up to run_units below where
        // it starts, so the arena never hands out its first run_units
        void reset_units() {
            _units.reset();
            _units.bump(reloc::run_units * config::reloc_unit_sz / sizeof(uint64_t), false);
        }
        std::span<reloc_unit> alloc_units(std::size_t n) {
            auto words = _units.bump(n * config::reloc_unit_sz / sizeof(uint64_t), false);
            return { (reloc_unit*)words.data(), words.size() * sizeof(uint64_t) / config::reloc_unit_sz };
        }
        void track(std::size_t pg) {
            auto run = pg / run_pgs;
            auto bit = 1ull << (run % 64);
            auto w = std::atomic_ref { _runs[run / 64] };
            if (w.load(std::memory_order_relaxed) & bit) return;
            if (w.fetch_or(bit, std::memory_order_relaxed) == 0)
                std::atomic_ref { _sums[run / 4096] }.fetch_or(1ull << (run / 64 % 64), std::memory_order_relaxed);
        }

    public:
        reloc_hdrs(uint8_t* base, uint8_t* units)
            : _entries((uint64_t*)base), _runs((uint64_t*)os::map_rw(track_bytes)), _sums(_runs + run_words),
            _units(units, config::reloc_units_sz) {
            if (!_runs) throw std::bad_alloc{};
            reset_units();
        }
        ~reloc_hdrs() { os::release(_runs, track_bytes); }
        reloc_hdrs(const reloc_hdrs&) = delete;
        reloc_hdrs& operator=(const reloc_hdrs&) = delete;

        bool commit(std::size_t from, std::size_t to) {
            return os::commit(_entries + from, (to - from) * config::reloc_hdr_sz);
        }

        uint64_t get(std::size_t pg) const { return _entries[pg]; }
        // Stores table[first, end), first being the unit of the run's first survivor
        void set(std::size_t pg, std::size_t block_sz, std::size_t split, std::span<const reloc_unit> table, std::size_t first) {
            assert(pg % run_pgs == 0 && "a run's entry sits at its first page");
            assert(!_entries[pg] && "a run is compacted at most once per cycle");
            auto stored = alloc_units(table.size() - first);
            assert(stored.size() == table.size() - first && "reloc units arena exhausted");
            std::copy(table.begin() + first, table.end(), stored.begin());

            auto idx = stored.data() - (const reloc_unit*)_units.base() - first;
            _entries[pg] = reloc::pack({ uint32_t(block_sz), uint32_t(split), uint32_t(idx) });
            track(pg);
        }
        // Must not race with set
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
            reset_units();
        }
    };
}
