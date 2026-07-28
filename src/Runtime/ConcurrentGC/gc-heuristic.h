#pragma once

#include <chrono>
#include <cmath>
#include <atomic>

#include "gc-config.h"

namespace gc::detail {
    using gc_clock = std::chrono::steady_clock;

    struct cycle_stats {
        std::chrono::nanoseconds mark_time { 0 };  // request accepted -> end of STW marking
        std::chrono::nanoseconds copy_time { 0 };  // zero when the cycle did not copy
        size_t allocated_bytes = 0;   // mutator allocation since the previous cycle
        size_t live_bytes      = 0;
        size_t evac_gain_bytes = 0;   // whole pages compaction would hand back
        size_t evac_move_bytes = 0;   // live bytes compaction would have to move
    };

    struct gc_tuning {
        size_t initial_heap = 20u << 20;
        size_t max_heap     = config::heap_max_sz;
        double max_headroom = 0.75;    // trigger = live * (1 + headroom)
        double min_headroom = 0.2625;
        // Half-life of the live-set peak the trigger is sized against
        double live_peak_halflife = 5.0;
        double copy_bias    = 3.0;     // >1 biases against compacting
        std::chrono::nanoseconds copy_fixed_cost = std::chrono::milliseconds(1);
        double rate_alpha        = 0.3;
        double initial_mark_rate = 200e6;   // replaced by the first real measurement
        double initial_copy_rate = 500e6;
    };

    class gc_heuristics {
        gc_tuning _cfg;
        double _alloc_rate, _mark_rate, _copy_rate;   // bytes/second, collector-private
        double _peak_live;                            // decaying max of recent live sets
        gc_clock::time_point _cycle_start;
        size_t _cycles = 0;

        // The only members a mutator may touch.
        std::atomic<size_t> _trigger;
        std::atomic<size_t> _live_size;
        std::atomic<bool>   _should_copy;

        static double secs(std::chrono::nanoseconds d) { return std::chrono::duration<double>(d).count(); }

        static double sample(size_t bytes, std::chrono::nanoseconds d, double fallback) {
            return (bytes == 0 || d <= std::chrono::nanoseconds::zero()) ? fallback : double(bytes) / secs(d);
        }
        void blend(double& est, double s) const { est = (1.0 - _cfg.rate_alpha) * est + _cfg.rate_alpha * s; }

        bool decide_copy(const cycle_stats& s) const {
            if (s.evac_gain_bytes == 0) return false;
            double bought = double(s.evac_gain_bytes) / std::max(_alloc_rate, 1.0);
            double cost   = secs(_cfg.copy_fixed_cost)
                          + _cfg.copy_bias * double(s.evac_move_bytes) / std::max(_copy_rate, 1.0);
            return bought > cost;
        }
        size_t next_trigger(size_t live) const {
            double pressure = std::clamp(_alloc_rate / std::max(_mark_rate, 1.0), 0.0, 1.0);
            double headroom = std::lerp(_cfg.max_headroom, _cfg.min_headroom, pressure);
            double floor    = double(std::max<size_t>(_cfg.initial_heap, live));
            double basis    = std::max(double(live), _peak_live);
            return size_t(std::clamp(basis * (1.0 + headroom), floor, _cfg.max_heap / 2.0));
        }
    public:
        explicit gc_heuristics(gc_tuning cfg = {}, gc_clock::time_point now = gc_clock::now())
            : _cfg(cfg), _alloc_rate(0), _mark_rate(cfg.initial_mark_rate), _copy_rate(cfg.initial_copy_rate),
              _peak_live(0), _cycle_start(now), _trigger(cfg.initial_heap), _live_size(0), _should_copy(false) {}

        size_t heap_trigger() const { return _trigger.load(std::memory_order_relaxed); }
        size_t live_size()    const { return _live_size.load(std::memory_order_relaxed); }
        bool   should_copy()  const { return _should_copy.load(std::memory_order_relaxed); }
        size_t cycles()       const { return _cycles; }

        void end_cycle(gc_clock::time_point now, const cycle_stats& s) {
            auto wall = now - _cycle_start;
            _cycle_start = now;
            blend(_alloc_rate, sample(s.allocated_bytes, wall,        _alloc_rate));
            blend(_mark_rate,  sample(s.live_bytes,      s.mark_time, _mark_rate));
            blend(_copy_rate,  sample(s.evac_move_bytes, s.copy_time, _copy_rate));

            _peak_live = std::max(
                double(s.live_bytes),
                _peak_live * std::exp2(-std::max(0.0, secs(wall)) / _cfg.live_peak_halflife)
            );

            _should_copy.store(decide_copy(s), std::memory_order_relaxed);
            _live_size.store(s.live_bytes, std::memory_order_relaxed);
            _trigger.store(next_trigger(s.live_bytes), std::memory_order_release);
            ++_cycles;
        }
    };
}
