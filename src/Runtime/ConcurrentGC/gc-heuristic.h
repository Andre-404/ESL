#pragma once

#include <algorithm>
#include <chrono>
#include <cmath>
#include <atomic>

#include "gc-config.h"

namespace gc::detail {
    using gc_clock = std::chrono::steady_clock;

    struct cycle_stats {
        std::chrono::nanoseconds mark_time { 0 };  // request accepted -> end of STW marking
        std::chrono::nanoseconds copy_time { 0 };  // zero when the cycle did not copy
        std::chrono::nanoseconds compact_time { 0 };  // the part of copy_time spent moving objects
        size_t copied_bytes    = 0;   // bytes the compaction moved
        size_t allocated_bytes = 0;   // mutator allocation since the previous cycle
        size_t live_bytes      = 0;
        size_t evac_gain_bytes = 0;   // whole pages compaction would hand back
        size_t evac_move_bytes = 0;   // live bytes compaction would have to move
        // Tracked so we don't give more headroom than there are actual live objects
        size_t black_bytes = 0;
    };

    struct gc_tuning {
        size_t initial_heap = 20u << 20;
        size_t max_heap     = config::heap_max_sz;
        double max_headroom = 0.75;    // trigger = live * (1 + headroom)
        double min_headroom = 0.2625;
        // Half-life of the live-set peak the trigger is sized against
        double live_peak_halflife = 5.0;
        double rate_alpha        = 0.3;
        double initial_mark_rate = 200e6;   // replaced by the first real measurement
        double initial_copy_rate = 500e6;
        // The whole copy phase of a pause aims to fit in this
        std::chrono::nanoseconds copy_budget = std::chrono::milliseconds(10);
        // Live bytes a second the rest of the copy phase gets through (gathering, the pointer
        // update), which visits every live object and so scales with the live set
        double initial_copy_overhead_rate = 4e9;
        // A copy has to hand back this fraction of the live set to be worth updating the pointers
        double min_copy_gain = 0.2;
    };

    class gc_heuristics {
        gc_tuning _cfg;
        double _alloc_rate, _mark_rate, _copy_rate;   // bytes/second, collector-private
        double _peak_live;                            // decaying max of recent live sets
        double _copy_overhead_rate;                   // see gc_tuning::initial_copy_overhead_rate
        size_t _evac_gain;                            // what the last cycle estimated a copy would free
        gc_clock::time_point _cycle_start;
        size_t _cycles = 0;

        // The only members a mutator may touch.
        std::atomic<size_t> _trigger;
        std::atomic<size_t> _live_size;

        static double secs(std::chrono::nanoseconds d) { return std::chrono::duration<double>(d).count(); }

        static double sample(size_t bytes, std::chrono::nanoseconds d, double fallback) {
            return (bytes == 0 || d <= std::chrono::nanoseconds::zero()) ? fallback : double(bytes) / secs(d);
        }
        void blend(double& est, double s) const { est = (1.0 - _cfg.rate_alpha) * est + _cfg.rate_alpha * s; }

        // According to the generational hypothesis most of the recently allocated objects will die
        // because of that we don't count black allocate objects into the headroom for the next trigger
        // (since they are most likely already dead or will be by the start of the next cycle)
        size_t next_trigger(size_t live, size_t occupancy, size_t black) const {
            double pressure = std::clamp(_alloc_rate / std::max(_mark_rate, 1.0), 0.0, 1.0);
            double headroom = std::lerp(_cfg.max_headroom, _cfg.min_headroom, pressure);
            double floor    = double(std::max<size_t>(_cfg.initial_heap, occupancy));
            double basis    = std::max(double(live), _peak_live);
            return size_t(std::min(std::max(basis * (1.0 + headroom) + double(black), floor),
                                   _cfg.max_heap / 2.0));
        }
    public:
        explicit gc_heuristics(gc_tuning cfg = {}, gc_clock::time_point now = gc_clock::now())
            : _cfg(cfg), _alloc_rate(0), _mark_rate(cfg.initial_mark_rate), _copy_rate(cfg.initial_copy_rate),
              _peak_live(0), _copy_overhead_rate(cfg.initial_copy_overhead_rate), _evac_gain(0), _cycle_start(now), _trigger(cfg.initial_heap), _live_size(0) {}

        size_t heap_trigger() const { return _trigger.load(std::memory_order_relaxed); }
        size_t live_size()    const { return _live_size.load(std::memory_order_relaxed); }
        size_t cycles()       const { return _cycles; }
        double copy_rate()    const { return _copy_rate; }
        size_t copy_budget_bytes() const {
            auto overhead = double(live_size()) / _copy_overhead_rate;
            return size_t(_copy_rate * std::max(0.0, secs(_cfg.copy_budget) - overhead));
        }
        size_t min_copy_gain() const {
            return std::max<size_t>(1, size_t(_cfg.min_copy_gain * double(live_size())));
        }
        // Decided before marking, since only a copying cycle pins what the stacks reach
        bool wants_copy() const { return _evac_gain >= min_copy_gain() && copy_budget_bytes() > 0; }

        void end_cycle(gc_clock::time_point now, const cycle_stats& s) {
            auto wall = now - _cycle_start;
            _cycle_start = now;
            blend(_alloc_rate, sample(s.allocated_bytes, wall,        _alloc_rate));
            blend(_mark_rate,  sample(s.live_bytes,      s.mark_time, _mark_rate));
            if (s.compact_time > std::chrono::nanoseconds::zero()) {
                blend(_copy_rate, sample(s.copied_bytes, s.compact_time, _copy_rate));
                // A slower rate is taken as is, the next pause would overshoot otherwise
                auto rate = sample(s.live_bytes, s.copy_time - s.compact_time, _copy_overhead_rate);
                if (rate < _copy_overhead_rate) _copy_overhead_rate = rate;
                else blend(_copy_overhead_rate, rate);
            }
            _evac_gain = s.evac_gain_bytes;

            auto live_set = s.live_bytes - std::min(s.black_bytes, s.live_bytes);
            _peak_live = std::max(
                double(live_set),
                _peak_live * std::exp2(-std::max(0.0, secs(wall)) / _cfg.live_peak_halflife)
            );

            _live_size.store(s.live_bytes, std::memory_order_relaxed);
            _trigger.store(next_trigger(live_set, s.live_bytes, s.black_bytes),
                           std::memory_order_release);
            ++_cycles;
        }
    };
}
