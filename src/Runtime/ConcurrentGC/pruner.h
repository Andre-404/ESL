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
        double _frag_threshold;
    public:
        pruner(double frag_threshold) : _frag_threshold(frag_threshold) {};

        void reset() {
            memset(_per_class_frag.data(), 0, _per_class_frag.size() * sizeof(sz_class_data));
            _live_sz = 0;
        }

        std::span<sz_class_data> get_ppg_frag() { return { _per_class_frag }; }

        size_t get_live_size() const { return _live_sz; }

        std::pair<pg_meta*, pg_meta*> prune(pg_meta* list) {
            auto empty = (pg_meta*)nullptr;
            // Done to preserve the order in which pages were allocated
            // TODO: might be better to sort them by address?
            auto in_use = (pg_meta*)nullptr;
            auto in_use_cur = (pg_meta*)nullptr;
            for (auto cur = list; cur;) {
                auto tmp = cur;
                cur = cur->next();
                if (tmp->live_count() == 0) {
                    tmp->link(empty);
                    empty = tmp;
                } else {
                    // TODO: is this inefficient?
                    auto sz_class = config::sz_to_class(tmp->block_sz());
                    auto frag = 1.0 - tmp->live_count() / (double)tmp->block_cnt();
                    // Don't count almost full pages into total, since they wouldn't be targets for copying anyway
                    if (sz_class != -1 && frag > _frag_threshold) {
                        _per_class_frag[sz_class].frag_total.fetch_add(frag, std::memory_order_relaxed);
                        _per_class_frag[sz_class].pg_cnt.fetch_add(1, std::memory_order_relaxed);
                    }
                    _live_sz.fetch_add(tmp->live_count() * tmp->block_sz(), std::memory_order_relaxed);
                    if (!in_use || !in_use_cur) {
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