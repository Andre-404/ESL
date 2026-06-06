#pragma once

#include <chrono>
#include <numeric>
#include <cmath>

#include "gc-config.h"

namespace gc::detail {
    class gc_heuristics {
        // all in B/ms
        double _alloc_rate;
        double _mark_rate = 200000.0;
        double _copy_rate = 500000.0;
        std::chrono::steady_clock::time_point _start;
        size_t _mark_ms;
        size_t _copy_ms;
        size_t _live_size;

        // Outputs
        size_t _trigger_sz;
        bool _should_copy;

        constexpr static size_t start_heap = 10 << 20;
        constexpr static size_t heap_max = 1ull << 40;
        constexpr static double head_room = 1.0;
        constexpr static double beta = 3;
        constexpr static double min_frag = 0.15;
        constexpr static double fixed_copy_cost_ms = 1;

        // returns: num of pages that would get freed by compaction, number of bytes that live in these partially filled pages
        auto calc_pgs_freed(auto& ppg_frag) {
            auto res = std::pair<size_t, size_t> {};
            for (auto& info : ppg_frag) {
                if (info.pg_cnt == 0) continue;
                auto f = info.frag_total / info.pg_cnt;
                auto pages_after  = ceil(info.pg_cnt * (1 - f));
                res.first += (info.pg_cnt - pages_after);
                res.second += info.pg_cnt * info.frag_total;
            }
            return res;
        }
    public:
        gc_heuristics() : _alloc_rate(0), _mark_rate(0), _copy_rate(0), _mark_ms(0), _copy_ms(0), _live_size(0),
                          _trigger_sz(start_heap), _should_copy(false) {
            _start = std::chrono::steady_clock::now();
        }

        void calc(auto ppg_frag, size_t alloc_sz, size_t live_sz) {
            auto tmp = _start;
            _live_size = live_sz;
            _start = std::chrono::steady_clock::now();
            auto diff = _start - tmp;
            _alloc_rate = (double)alloc_sz / std::chrono::duration_cast<std::chrono::milliseconds>(diff).count();
            auto mark_rate = (double)_live_size / _mark_ms;
            auto copy_rate = _copy_ms == 0 ? _copy_rate : (double)_live_size / _copy_ms;
            _mark_rate = 0.7 * _mark_rate + 0.3 * mark_rate;
            _copy_rate = 0.7 * _copy_rate + 0.3 * copy_rate;

            auto [pages_freed, copyable_bytes] = calc_pgs_freed(ppg_frag);
            auto bytes_saved = pages_freed * config::page_sz;

            // TODO: think of a better cap
            _trigger_sz = std::min(heap_max * 0.5, live_sz * (1 + head_room - _alloc_rate / _mark_rate));
            _should_copy = bytes_saved / _alloc_rate > fixed_copy_cost_ms + beta * copyable_bytes / _copy_rate;
        }

        bool should_copy() const {  return _should_copy; }
        size_t heap_trigger() const { return _trigger_sz; }

        size_t get_live_size() const { return _live_size; }

        void set_mark_ms(size_t ms) { _mark_ms = ms; }
        void set_copy_ms(size_t ms) { _copy_ms = ms; }
    };
}
