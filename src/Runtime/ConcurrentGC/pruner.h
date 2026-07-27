#pragma once

#include <cmath>

#include "gc-config.h"
#include "pg-meta.h"


namespace gc::detail {
    // Prunes individual page lists and gathers fragmentation info
    class pruner {
        struct sz_class_data {
            std::atomic<double> frag_total;
            std::atomic<size_t> pg_cnt;
        };
        std::atomic<size_t> _live_sz;
        std::array<sz_class_data, config::szclass_cnt> _per_class_frag;

        void add_stats(pg_meta* pg) {
            // TODO: is this inefficient?
            auto sz_class = config::sz_to_class(pg->block_sz());
            if (sz_class != config::large_class) {
                auto frag = 1.0 - pg->live_count() / (double)pg->block_cnt();
                _per_class_frag[sz_class].frag_total.fetch_add(frag, std::memory_order_relaxed);
                _per_class_frag[sz_class].pg_cnt.fetch_add(1, std::memory_order_relaxed);
            }
            _live_sz.fetch_add(pg->live_count() * pg->block_sz(), std::memory_order_relaxed);
        }

    public:
        pruner() {};

        void reset() {
            memset(_per_class_frag.data(), 0, _per_class_frag.size() * sizeof(sz_class_data));
            _live_sz = 0;
        }

        size_t get_live_size() const { return _live_sz; }

        template<typename F>
        pg_meta* prune(pg_meta* list, F on_empty) {
            // Done to preserve the order in which pages were allocated
            // TODO: might be better to sort them by address?
            auto in_use = (pg_meta*)nullptr;
            auto in_use_cur = (pg_meta*)nullptr;
            for (auto cur = list; cur;) {
                auto tmp = cur;
                cur = cur->next();
                tmp->unlink();
                tmp->compute_live();
                if (tmp->live_count() == 0) {
                    on_empty(tmp);
                } else {
                    add_stats(tmp);
                    tmp->reset_trackers();
                    if (!in_use_cur) {
                        in_use = tmp;
                        in_use_cur = tmp;
                    } else {
                        in_use_cur->link(tmp);
                        in_use_cur = tmp;
                    }
                }
            }
            return in_use;
        }

        struct evac_estimate { size_t gain_bytes = 0; size_t move_bytes = 0; };

        evac_estimate estimate_evacuation() const {
            evac_estimate e {};
            for (size_t i = 0; i < config::szclass_cnt; i++) {
                auto pgs = _per_class_frag[i].pg_cnt.load(std::memory_order_relaxed);
                if (pgs == 0) continue;
                auto avg_frag = _per_class_frag[i].frag_total.load(std::memory_order_relaxed) / pgs;
                auto after = (size_t)std::ceil(pgs * (1.0 - avg_frag));
                e.gain_bytes += (pgs - after) * config::page_sz;
                // Live bytes, i.e. what actually has to be copied. The old code used
                // pg_cnt*avg_frag*page_sz here, which is the *free* part of those pages.
                e.move_bytes += (size_t)(pgs * (1.0 - avg_frag) * config::page_sz);
            }
            return e;
        }
    };
}