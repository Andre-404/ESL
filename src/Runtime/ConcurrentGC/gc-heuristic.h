#pragma once

#include <chrono>
#include <numeric>

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

        // Outputs
        size_t _trigger_sz;
        bool _should_copy;

        constexpr static size_t start_heap = 10 << 20;
        constexpr static double head_room = 1.0;
        constexpr static double beta = 3;
        constexpr static double min_frag = 0.15;
        constexpr static double fixed_copy_cost_ms = 1;

        // returns: num of pages that would get freed by compaction, number of bytes that live in these partially filled pages
        auto calc_pgs_freed(auto& ppg_frag) {
            return std::reduce(ppg_frag.begin(), ppg_frag.end(), std::pair<size_t, size_t> {}, [&](auto p, auto& s) {
                if (s.pg_cnt == 0) return p;
                auto f = s.frag_total / s.pg_cnt;
                auto pages_after  = ceil(s.pg_cnt * (1 - f));
                p.first += (s.pg_cnt - pages_after);
                p.second += s.pg_cnt * s.frag_total;
                return p;
            });
        }
    public:
        gc_heuristics() : _alloc_rate(0), _mark_rate(0), _copy_rate(0), _mark_ms(0), _copy_ms(0), _trigger_sz(start_heap),
                          _should_copy(false) {
            _start = std::chrono::steady_clock::now();
        }

        void calc(auto ppg_frag, size_t alloc_sz, size_t live_sz) {
            auto tmp = _start;
            _start = std::chrono::steady_clock::now();
            auto diff = _start - tmp;
            _alloc_rate = (double)alloc_sz / std::chrono::duration_cast<std::chrono::milliseconds>(diff).count();
            auto mark_rate = (double)live_sz / _mark_ms;
            auto copy_rate = _copy_ms == 0 ? _copy_rate : (double)live_sz / _copy_ms;
            _mark_rate = 0.7 * _mark_rate + 0.3 * mark_rate;
            _copy_rate = 0.7 * _copy_rate + 0.3 * copy_rate;

            auto [pages_freed, copyable_bytes] = calc_pgs_freed(ppg_frag);
            auto bytes_saved = pages_freed * config::page_sz;

            _trigger_sz = live_sz * (1 + head_room - _alloc_rate / _mark_rate);
            _should_copy = bytes_saved / _alloc_rate > fixed_copy_cost_ms + beta * copyable_bytes / _copy_rate;
        }

        bool should_copy() const {  return _should_copy; }
        size_t heap_trigger() const { return _trigger_sz; }

        void set_mark_ms(size_t ms) { _mark_ms = ms; }
        void set_copy_ms(size_t ms) { _copy_ms = ms; }
    };
}
