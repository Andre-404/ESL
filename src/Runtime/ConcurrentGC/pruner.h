#pragma once

#include "pg-meta.h"

namespace gc::detail {
    // Prunes individual page lists and gathers fragmentation info
    class pruner {
        std::atomic<double> _frag_total;
        std::atomic<size_t> _pg_cnt;
        std::atomic<size_t> _live_sz;
    public:
        pruner();

        void reset() {
            _frag_total = 0;
            _pg_cnt = 0;
            _live_sz = 0;
        }

        double get_heap_frag() const {
            return _pg_cnt == 0 ? 0.0 : _frag_total / _pg_cnt;
        }

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
                    _frag_total.fetch_add(1.0 - (tmp->live_count() / (double)tmp->block_cnt()), std::memory_order_relaxed);
                    _pg_cnt.fetch_add(1, std::memory_order_relaxed);
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