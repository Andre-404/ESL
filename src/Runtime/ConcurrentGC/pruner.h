#pragma once

#include <atomic>

#include "gc-config.h"
#include "pg-meta.h"
#include "allocator/gc-bits.h"


namespace gc::detail {
    // Prunes individual page lists and gathers fragmentation info
    class pruner {
        std::atomic<size_t> _live_bytes;
        std::atomic<uint64_t*> _bitmap_watermark;
        // Per size class, summed over the copy candidates runs
        std::array<std::atomic<double>, config::szclass_cnt> _frag {};

        void record_list(uint8_t szclass, double frag, size_t live_bytes) {
            if (szclass != config::large_class) _frag[szclass].fetch_add(frag, std::memory_order_relaxed);
            _live_bytes.fetch_add(live_bytes, std::memory_order_relaxed);
        }

        void record_watermark(uint64_t* end) {
            auto cur = _bitmap_watermark.load(std::memory_order_relaxed);
            while (cur < end && !_bitmap_watermark.compare_exchange_weak(cur, end, std::memory_order_relaxed)) {}
        }

    public:
        pruner() : _live_bytes(0), _bitmap_watermark(nullptr) {};

        [[nodiscard]] uint64_t* end_cycle() {
            for (auto& frag : _frag) frag.store(0, std::memory_order_relaxed);
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
                if (pg->stranded() && live_blks < config::copy_evac_threshold * pg->block_cnt())
                    frag += 1.0 - live_blks / (double)pg->block_cnt();
                ++pgs;
                live_bytes += live_blks * pg->block_sz();
                if (tail) tail->link(pg); else head = pg;
                tail = pg;
            }
            if (!head) return nullptr;
            record_list(head->szclass(), frag, live_bytes);

            auto words = (head->block_cnt() + 63) / 64;
            // TODO: fail loudly when span is empty
            auto bitmap = bits.mark_bits(pgs * words * 64, false);
            auto slot = bitmap.data();
            for (auto pg = head; pg; pg = pg->next(), slot += words)
                pg->next_cycle(slot);
            record_watermark(bitmap.data() + bitmap.size());
            return head;
        }

        size_t evac_gain_bytes() const {
            constexpr auto run_sz = config::small_obj_pgs * config::page_sz;
            size_t runs = 0;
            for (auto& frag : _frag) runs += size_t(frag.load(std::memory_order_relaxed));
            return runs * run_sz;
        }
    };
}
