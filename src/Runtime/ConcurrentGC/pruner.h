#pragma once

#include <algorithm>
#include <atomic>
#include <cmath>

#include "gc-config.h"
#include "pg-meta.h"
#include "gc-bits.h"


namespace gc::detail {
    // Prunes individual page lists and gathers fragmentation info
    class pruner {
        struct szclass_stats {
            std::atomic<double> frag_sum;
            std::atomic<size_t> pg_cnt;
        };
        std::atomic<size_t> _live_bytes;
        std::atomic<uint64_t*> _bitmap_watermark;
        std::array<szclass_stats, config::szclass_cnt> _szclass_stats;

        void record_page(pg_meta* pg, size_t live) {
            // TODO: is this inefficient?
            if (pg->szclass() != config::large_class) {
                auto& stats = _szclass_stats[pg->szclass()];
                stats.frag_sum.fetch_add(1.0 - live / (double)pg->block_cnt(), std::memory_order_relaxed);
                stats.pg_cnt.fetch_add(1, std::memory_order_relaxed);
            }
            _live_bytes.fetch_add(live * pg->block_sz(), std::memory_order_relaxed);
        }

        void record_watermark(uint64_t* end) {
            auto cur = _bitmap_watermark.load(std::memory_order_relaxed);
            while (cur < end && !_bitmap_watermark.compare_exchange_weak(cur, end, std::memory_order_relaxed)) {}
        }

    public:
        pruner() : _live_bytes(0), _bitmap_watermark(nullptr) {};

        [[nodiscard]] uint64_t* end_cycle() {
            for (auto& stats : _szclass_stats) {
                stats.frag_sum.store(0, std::memory_order_relaxed);
                stats.pg_cnt.store(0, std::memory_order_relaxed);
            }
            _live_bytes = 0;
            return _bitmap_watermark.exchange(nullptr, std::memory_order_relaxed);
        }

        size_t live_bytes() const { return _live_bytes; }

        template<typename F>
        pg_meta* prune(pg_meta* list, gc_bits& bits, F retire) {
            // Done to preserve the order in which pages were allocated
            // TODO: might be better to sort them by address?
            auto head = (pg_meta*)nullptr;
            auto tail = (pg_meta*)nullptr;
            uint64_t* watermark = nullptr;
            for (auto next = list; next;) {
                auto pg = next;
                next = next->next();
                pg->unlink();
                // The copier retires its source pages by marking them inactive, and an inactive
                // page has nothing worth counting, so it never pays for the popcount
                auto live_blks = pg->is_active() ? pg->compute_live() : 0;
                if (live_blks == 0) {
                    retire(pg);
                    continue;
                }
                record_page(pg, live_blks);
                // TODO: fail loudly when span is empty
                auto bitmap = bits.mark_bits(pg->block_cnt(), false);
                watermark = std::max(watermark, bitmap.data() + bitmap.size());
                pg->next_cycle(bitmap.data());
                if (tail) tail->link(pg); else head = pg;
                tail = pg;
            }
            if (watermark) record_watermark(watermark);
            return head;
        }

        struct evac_estimate { size_t gain_bytes = 0; size_t move_bytes = 0; };

        evac_estimate estimate_evacuation() const {
            evac_estimate e {};
            for (auto& stats : _szclass_stats) {
                auto pgs = stats.pg_cnt.load(std::memory_order_relaxed);
                if (pgs == 0) continue;
                // What the same live blocks would occupy once packed, and so what has to move
                auto live_pgs = pgs * (1.0 - stats.frag_sum.load(std::memory_order_relaxed) / pgs);
                e.gain_bytes += (pgs - (size_t)std::ceil(live_pgs)) * config::page_sz;
                e.move_bytes += (size_t)(live_pgs * config::page_sz);
            }
            return e;
        }
    };
}