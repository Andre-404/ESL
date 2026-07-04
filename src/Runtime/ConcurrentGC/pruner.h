#pragma once

#include "pg-meta.h"
#include <span>

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
            if (sz_class != -1) {
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

        std::span<sz_class_data> get_ppg_frag() { return { _per_class_frag }; }

        size_t get_live_size() const { return _live_sz; }

        std::pair<pg_meta*, pg_meta*> prune(pg_meta* list) {
            // Done to preserve the order in which pages were allocated
            // TODO: might be better to sort them by address?
            auto empty = (pg_meta*)nullptr;
            auto in_use = (pg_meta*)nullptr;
            auto in_use_cur = (pg_meta*)nullptr;
            for (auto cur = list; cur;) {
                auto tmp = cur;
                cur = cur->next();
                __builtin_prefetch(cur, 0, 0);
                tmp->unlink();
                tmp->compute_live();
                if (tmp->live_count() == 0) {
                    tmp->link(empty);
                    empty = tmp;
                } else {
                    add_stats(tmp);
                    if (!in_use_cur) {
                        in_use = tmp;
                        in_use_cur = tmp;
                    } else {
                        in_use_cur->link(tmp);
                        in_use_cur = tmp;
                    }
                }
                tmp->reset_trackers();
            }
            return { empty, in_use };
        }
    };
}