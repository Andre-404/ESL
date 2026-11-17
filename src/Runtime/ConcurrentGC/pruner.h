#pragma once

#include <atomic>
#include <cmath>

#include "gc-config.h"
#include "pg-meta.h"
#include "allocator/gc-bits.h"


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

        void record_list(uint8_t szclass, double frag, size_t pgs, size_t live_bytes) {
            if (szclass != config::large_class) {
                auto& stats = _szclass_stats[szclass];
                stats.frag_sum.fetch_add(frag, std::memory_order_relaxed);
                stats.pg_cnt.fetch_add(pgs, std::memory_order_relaxed);
            }
            _live_bytes.fetch_add(live_bytes, std::memory_order_relaxed);
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
            double frag = 0;
            size_t pgs = 0, live_bytes = 0;
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
                frag += 1.0 - live_blks / (double)pg->block_cnt();
                ++pgs;
                live_bytes += live_blks * pg->block_sz();
                if (tail) tail->link(pg); else head = pg;
                tail = pg;
            }
            if (!head) return nullptr;
            record_list(head->szclass(), frag, pgs, live_bytes);

            auto words = (head->block_cnt() + 63) / 64;
            // TODO: fail loudly when span is empty
            auto bitmap = bits.mark_bits(pgs * words * 64, false);
            auto slot = bitmap.data();
            for (auto pg = head; pg; pg = pg->next(), slot += words)
                pg->next_cycle(slot);
            record_watermark(bitmap.data() + bitmap.size());
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
                constexpr auto run_sz = config::small_obj_pgs * config::page_sz;
                e.gain_bytes += (pgs - (size_t)std::ceil(live_pgs)) * run_sz;
                e.move_bytes += (size_t)(live_pgs * run_sz);
            }
            return e;
        }
    };
}
