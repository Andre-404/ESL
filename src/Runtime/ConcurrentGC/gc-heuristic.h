#pragma once

#include <algorithm>
#include <chrono>
#include <cmath>
#include <atomic>

#include "gc-config.h"
#include "allocator/scavenger.h"

namespace gc {
    struct gc_tuning {
        size_t initial_heap = 20u << 20;
        size_t max_heap     = config::heap_max_sz;
        double max_headroom = 0.75;    // trigger = live * (1 + headroom)
        double min_headroom = 0.2625;
        // Half-life of the live-set peak the trigger is sized against
        double live_peak_halflife = 5.0;
        double rate_alpha        = 0.3;
        size_t initial_copy_bytes = 16u << 20;
        // Smaller copies are mostly fixed costs, so they don't teach the copy rate
        size_t min_rate_sample_bytes = 4u << 20;
        // Compaction in a pause aims to fit in this, the rest of the copy phase takes well under 1ms
        std::chrono::nanoseconds copy_budget = std::chrono::milliseconds(10);
        // (G1's G1HeapWastePercent)
        double min_copy_gain = 0.05;
        // Below this a copying cycle's fixed costs in the pause outweigh what it frees
        size_t min_copy_bytes = 1u << 20;
        scavenge_policy scavenge;
    };
}

namespace gc::detail {
    using gc_clock = std::chrono::steady_clock;

    struct cycle_stats {
        std::chrono::nanoseconds mark_time { 0 };  // request accepted -> end of STW marking
        std::chrono::nanoseconds compact_time { 0 };  // zero when the cycle did not copy
        size_t copied_bytes    = 0;   // bytes the compaction moved
        size_t allocated_bytes = 0;   // mutator allocation since the previous cycle
        size_t live_bytes      = 0;
        size_t evac_gain_bytes = 0;   // whole pages compaction would hand back
        // Tracked so we don't give more headroom than there are actual live objects
        size_t black_bytes = 0;
    };

    class gc_heuristics {
        gc_tuning _cfg;
        double _alloc_rate, _mark_rate, _copy_rate;   // bytes/second, collector-private, 0 until measured
        double _peak_live;                            // decaying max of recent live sets
        size_t _evac_gain;                            // what the last cycle estimated a copy would free
        gc_clock::time_point _cycle_start;

        // The only members a mutator may touch.
        std::atomic<size_t> _trigger;
        std::atomic<size_t> _live_size;

        static double secs(std::chrono::nanoseconds d) { return std::chrono::duration<double>(d).count(); }

        static double sample(size_t bytes, std::chrono::nanoseconds d, double fallback) {
            return (bytes == 0 || d <= std::chrono::nanoseconds::zero()) ? fallback : double(bytes) / secs(d);
        }
        void blend(double& est, double s) const { est = (1.0 - _cfg.rate_alpha) * est + _cfg.rate_alpha * s; }

        void learn(double& est, double s) const { if (est > 0) blend(est, s); else est = s; }

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
            : _cfg(cfg), _alloc_rate(0), _mark_rate(0), _copy_rate(0),
              _peak_live(0), _evac_gain(0), _cycle_start(now), _trigger(cfg.initial_heap), _live_size(0) {}

        size_t heap_trigger() const { return _trigger.load(std::memory_order_relaxed); }
        size_t live_size()    const { return _live_size.load(std::memory_order_relaxed); }
        size_t copy_budget_bytes() const {
            return _copy_rate > 0 ? size_t(_copy_rate * secs(_cfg.copy_budget)) : _cfg.initial_copy_bytes;
        }
        size_t min_copy_gain() const {
            return std::max(_cfg.min_copy_bytes, size_t(_cfg.min_copy_gain * double(live_size())));
        }
        size_t min_copy_bytes() const { return _cfg.min_copy_bytes; }
        // From the last cycle's estimate
        bool wants_copy() const { return _evac_gain >= min_copy_gain(); }

        void end_cycle(gc_clock::time_point now, const cycle_stats& s) {
            auto wall = now - _cycle_start;
            _cycle_start = now;
            learn(_alloc_rate, sample(s.allocated_bytes, wall,        _alloc_rate));
            learn(_mark_rate,  sample(s.live_bytes,      s.mark_time, _mark_rate));
            if (s.compact_time > std::chrono::nanoseconds::zero() && s.copied_bytes >= _cfg.min_rate_sample_bytes)
                learn(_copy_rate, sample(s.copied_bytes, s.compact_time, _copy_rate));
            _evac_gain = s.evac_gain_bytes;

            auto live_set = s.live_bytes - std::min(s.black_bytes, s.live_bytes);
            _peak_live = std::max(
                double(live_set),
                _peak_live * std::exp2(-std::max(0.0, secs(wall)) / _cfg.live_peak_halflife)
            );

            _live_size.store(s.live_bytes, std::memory_order_relaxed);
            _trigger.store(next_trigger(live_set, s.live_bytes, s.black_bytes),
                           std::memory_order_release);
        }
    };
}
