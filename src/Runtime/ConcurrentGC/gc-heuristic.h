#pragma once

#include <chrono>
#include <numeric>
#include <cmath>

#include "gc-config.h"

namespace gc::detail {
    class gc_heuristics {
        // all in B/ms
        double _alloc_rate;
        double _mark_rate;
        double _copy_rate;
        std::chrono::steady_clock::time_point _start;
        size_t _mark_ms;
        size_t _copy_ms;
        size_t _live_size;

        // Outputs
        size_t _trigger_sz;
        bool _should_copy;
        size_t _cycles;

        constexpr static size_t start_heap = 20 << 20;
        constexpr static size_t heap_max = 1ull << 40;
        constexpr static double head_room = 1.0;
        constexpr static double min_head_room_ratio = 0.25;
        constexpr static double beta = 3;
        constexpr static double fixed_copy_cost_ms = 1;

        // returns: num of pages that would get freed by compaction, number of bytes that live in these partially filled pages
        auto calc_pgs_freed(auto& ppg_frag) {
            auto res = std::pair<size_t, size_t> {};
            for (int i = 0; i < config::szclass_cnt; i++) {
                auto& info = ppg_frag[i];
                if (info.pg_cnt == 0) continue;
                auto avg_frag = info.frag_total / info.pg_cnt;
                auto pages_after  = ceil(info.pg_cnt * (1 - avg_frag));
                res.first += (info.pg_cnt - pages_after);
                // total pgs * avg frag per page * pg size, estimate because we don't take in slot fragmentation into consideration
                res.second += info.pg_cnt * avg_frag * config::page_sz;
            }
            return res;
        }
    public:
        gc_heuristics() : _alloc_rate(0), _mark_rate(200000.0), _copy_rate(500000.0), _mark_ms(0), _copy_ms(0),
                          _live_size(0),
                          _trigger_sz(start_heap), _should_copy(false), _cycles(0) {
            _start = std::chrono::steady_clock::now();
        }

        void calc(auto ppg_frag, size_t alloc_sz, size_t live_sz) {
            auto tmp = _start;
            _start = std::chrono::steady_clock::now();
            auto diff = _start - tmp;
            _live_size = live_sz;
            _alloc_rate = (double)alloc_sz / std::max(1l, std::chrono::duration_cast<std::chrono::milliseconds>(diff).count());
            auto mark_rate = _mark_ms == 0 ? _mark_rate : (double)_live_size / _mark_ms;
            auto copy_rate = _copy_ms == 0 ? _copy_rate : (double)_live_size / _copy_ms;
            _mark_rate = 0.7 * _mark_rate + 0.3 * mark_rate;
            _copy_rate = 0.7 * _copy_rate + 0.3 * copy_rate;

            auto [pages_freed, live_bytes] = calc_pgs_freed(ppg_frag);
            auto bytes_saved = pages_freed * config::page_sz;
            _should_copy = bytes_saved / _alloc_rate > fixed_copy_cost_ms + beta * live_bytes / _copy_rate;

            // TODO: think of a better cap and calculate the cap before stw finishes somehow?
            auto pressure = std::min(1.0, _alloc_rate / std::max(_mark_rate, 1.0));
            double headroom = std::max(min_head_room_ratio * head_room, head_room - pressure * head_room);
            double target = live_sz * (1.0 + headroom);
            double floor = std::max<double>(start_heap, live_sz);

            _trigger_sz = std::clamp(target, floor, heap_max / 2.0);
            _cycles++;
        }

        bool should_copy() const {  return _should_copy; }
        size_t heap_trigger() const { return _trigger_sz; }
        // TODO: think this through more carefully
        size_t stw_trigger() const { return _live_size * (1 + head_room*2); }

        size_t get_live_size() const { return _live_size; }

        void mark_start() {
            auto tmp = std::chrono::steady_clock::now().time_since_epoch();
            _mark_ms = std::chrono::duration_cast<std::chrono::milliseconds>(tmp).count();
        }
        void mark_end() {
            auto tmp = std::chrono::steady_clock::now().time_since_epoch();
            _mark_ms = std::chrono::duration_cast<std::chrono::milliseconds>(tmp).count() - _mark_ms;
        }
        void set_copy_ms(size_t ms) { _copy_ms = ms; }
    };
}
