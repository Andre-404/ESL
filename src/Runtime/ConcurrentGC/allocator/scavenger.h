#pragma once
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <stop_token>
#include <thread>

namespace gc {
    struct scavenge_policy {
        double headroom      = 0.0625; // keep demand*(1+headroom) resident before scavenging
        double thrash_gain   = 0.5;    // widen the band by this * observed refault rate
        double decay_per_sec = std::log(2) / 5;  // demand-peak half-life ~5s
        double base_cpu      = 0.01;   // steady-state scavenger CPU budget
        double max_cpu       = 0.08;   // budget cap when far above goal
        double cpu_gain      = 0.07;   // extra budget per unit of excess/goal
        std::size_t min_retain = 64;   // never scavenge below this many resident granules
                                        // (64 granules is 4 MiB at 8K pages, 32 MiB at 64K)
        double quantum_ns    = 1e6;    // aim for ~1ms of decommit work per wake
        double ewma_alpha    = 0.1;    // smoothing for the measured ns-per-granule
        double init_ns_per_granule = 3000; // first-quantum cost guess
        std::chrono::nanoseconds idle_sleep{ std::chrono::milliseconds(50) };
        std::chrono::nanoseconds max_sleep{ std::chrono::seconds(1) };
    };
}

namespace gc::detail {
    class span_allocator;

    // Paces the background scavenger: on each wake it decides how many commit granules to
    // hand back to the OS, and how long to sleep afterwards
    class scavenge_heuristic {
        using clock = std::chrono::steady_clock;

        scavenge_policy _cfg;
        double _peak = 0.0;
        double _goal = 0.0;
        double _f    = 0.0;
        double _ns_per_granule;
        clock::time_point _last{};
        bool _started = false;
    public:

        struct observation {
            std::size_t in_use   = 0;   // granules currently allocated
            std::size_t resident = 0;   // granules physically backed (RSS proxy, not _committed)
            double      thrash   = 0.0; // recent decommit->refault rate in [0,1]; widens the band
        };

        explicit scavenge_heuristic(scavenge_policy cfg)
            : _cfg(cfg), _ns_per_granule(cfg.init_ns_per_granule) {}

        // Top of each wake. Advances the decaying demand peak using `now`, then returns how
        // many high-address free granules to decommit this quantum. 0 means resident memory
        // is at or below the goal - nothing to do.
        std::size_t plan(clock::time_point now, observation obs) {
            double dt = _started ? std::chrono::duration<double>(now - _last).count() : 0.0;
            _last = now;
            // A fresh peak seeds to current demand; after that it can only rise to meet demand
            // or decay slowly toward it, so a transient free trough does not trigger a scavenge.
            if (!_started) { _peak = double(obs.in_use); _started = true; }
            else _peak = std::max(double(obs.in_use), _peak * std::exp(-_cfg.decay_per_sec * dt));

            _goal = std::max(double(_cfg.min_retain),
                             _peak * (1.0 + _cfg.headroom + _cfg.thrash_gain * obs.thrash));

            double excess = double(obs.resident) - _goal;
            if (excess <= 0.0) { _f = _cfg.base_cpu; return 0; }

            // Work harder the further resident memory has drifted above the goal.
            _f = std::clamp(_cfg.base_cpu + _cfg.cpu_gain * (excess / _goal),
                            _cfg.base_cpu, _cfg.max_cpu);

            double quantum = _cfg.quantum_ns / std::max(1.0, _ns_per_granule);
            auto want = std::size_t(std::min(excess, quantum));
            return std::max<std::size_t>(want, 1); // make progress even on a sub-granule excess
        }

        // After the decommit. Feeds back the measured cost, refines the per-granule estimate,
        // and returns how long to sleep to hold the CPU fraction plan() just chose. A wake
        // that scavenged nothing sleeps the idle interval so the decaying goal is re-checked
        std::chrono::nanoseconds pace(std::size_t granules_done, std::chrono::nanoseconds work) {
            if (granules_done == 0 || work <= std::chrono::nanoseconds::zero())
                return _cfg.idle_sleep;

            double per = double(work.count()) / double(granules_done);
            _ns_per_granule = (1.0 - _cfg.ewma_alpha) * _ns_per_granule + _cfg.ewma_alpha * per;

            // sleep / (sleep + work) == 1 - f  ->  sleep = work * (1 - f) / f
            double sleep = double(work.count()) * (1.0 - _f) / _f;
            sleep = std::min(sleep, double(_cfg.max_sleep.count()));
            return std::chrono::nanoseconds(std::int64_t(sleep));
        }
    };

    // Hands free granules back to the OS from a background thread, paced by scavenge_heuristic.
    // Must be destroyed before the span_allocator it was given
    class scavenger {
        std::jthread _thd;
        static void loop(std::stop_token st, span_allocator& spans, scavenge_policy policy);
    public:
        scavenger(span_allocator& spans, scavenge_policy policy)
            : _thd([&spans, policy](std::stop_token st) { loop(st, spans, policy); }) {}
    };
}
