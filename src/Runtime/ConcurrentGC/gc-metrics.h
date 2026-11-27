#pragma once

#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <limits>
#include <mutex>

#include "gc-heuristic.h"
#include "os/os-time.h"

namespace gc {
    struct phase_stats { double last_ms = 0, max_ms = 0, avg_ms = 0; };
    // From live_bytes to cleanups_queued the numbers are current, the rest is as of the last cycle's end
    struct gc_stats {
        size_t cycles = 0, copying_cycles = 0;
        size_t live_bytes = 0, heap_trigger = 0;
        // Whole commit granules
        size_t heap_in_use_bytes = 0, heap_resident_bytes = 0;
        // The cleanup runner counts as a thread
        size_t threads = 0, cleanups_registered = 0, cleanups_queued = 0;
        size_t allocated_bytes = 0, copied_bytes = 0;
        // The worker thread's alone, pause work done on mutators is in the pause times
        double collector_cpu_ms = 0;
        phase_stats pause, ttsp, stw, mark, copy, sweep, concurrent_mark, compact;
    };
}

namespace gc::detail {
    class gc_metrics {
    public:
        enum class phase : uint8_t {
            pause = 0, 
            mark,
            copy,
            sweep,
            ttsp,
            stw,
            concurrent_mark,
            compact,
            count
        };

    private:
        using clock = std::chrono::steady_clock;
        using ns = std::chrono::nanoseconds;

        struct stat {
            ns last { 0 };
            ns max { 0 };
            ns total { 0 };
            size_t samples { 0 };

            void record(ns d) {
                last = d;
                if (d > max) max = d;
                total += d;
                ++samples;
            }
        };

        // Each participant publishes the interval it worked and the phase duration is the union of those intervals
        class span {
            using rep = clock::rep;
            static constexpr rep no_start = std::numeric_limits<rep>::max();
            static constexpr rep no_end   = std::numeric_limits<rep>::min();

            std::atomic<rep> _first { no_start };
            std::atomic<rep> _last  { no_end };

        public:
            void contribute(clock::time_point s, clock::time_point e) {
                auto st = s.time_since_epoch().count();
                auto en = e.time_since_epoch().count();
                auto cur = _first.load(std::memory_order_relaxed);
                while (st < cur && !_first.compare_exchange_weak(cur, st, std::memory_order_relaxed)) {}
                cur = _last.load(std::memory_order_relaxed);
                while (en > cur && !_last.compare_exchange_weak(cur, en, std::memory_order_relaxed)) {}
            }

            ns harvest() {
                auto st = _first.exchange(no_start, std::memory_order_relaxed);
                auto en = _last.exchange(no_end, std::memory_order_relaxed);
                return st == no_start ? ns { 0 } : ns { en - st };
            }
        };

        std::array<stat, (size_t)phase::count> _stats {};
        std::array<span, (size_t)phase::count> _spans {};
        std::mutex _published_mtx;
        gc_stats _published;

        const stat& get(phase p) const { return _stats[(size_t)p]; }
        static double to_ms(ns d) { return std::chrono::duration<double, std::milli>(d).count(); }

    public:
        class [[nodiscard]] scope {
            span* _s;
            clock::time_point _start;

        public:
            explicit scope(span& s) : _s(&s), _start(clock::now()) {}
            ~scope() { _s->contribute(_start, clock::now()); }
            scope(const scope&) = delete;
            scope& operator=(const scope&) = delete;
        };

        // Called by every thread taking part in the phase
        scope time(phase p) { return scope { _spans[(size_t)p] }; }

        // Called by the coordinator only, and only once it is ordered after every contributor
        ns collect(phase p) {
            auto d = _spans[(size_t)p].harvest();
            if (d.count()) _stats[(size_t)p].record(d);
            return d;
        }

        struct cycle_times { ns mark, compact; };
        cycle_times collect_cycle() {
            auto concurrent = collect(phase::concurrent_mark);
            auto pause = collect(phase::pause), stw = collect(phase::stw);
            _stats[(size_t)phase::ttsp].record(pause - stw);
            auto mark = collect(phase::mark);
            collect(phase::sweep);
            collect(phase::copy);
            return { concurrent + (pause - stw) + mark, collect(phase::compact) };
        }

        phase_stats summary(phase p) const {
            auto& s = get(p);
            return { to_ms(s.last), to_ms(s.max), s.samples ? to_ms(s.total) / s.samples : 0.0 };
        }
        // Worker only, once per cycle, after collect_cycle
        void publish(const cycle_stats& c) {
            using enum phase;
            auto _ = std::scoped_lock { _published_mtx };
            auto& s = _published;
            s.cycles = get(pause).samples;
            // Only a cycle that copied records a compact time
            s.copying_cycles = get(compact).samples;
            s.allocated_bytes += c.allocated_bytes;
            s.copied_bytes += c.copied_bytes;
            s.collector_cpu_ms = to_ms(os::thread_cpu_time());
            s.pause = summary(pause);
            s.ttsp = summary(ttsp);
            s.stw = summary(stw);
            s.mark = summary(mark);
            s.copy = summary(copy);
            s.sweep = summary(sweep);
            s.concurrent_mark = summary(concurrent_mark);
            s.compact = summary(compact);
        }
        // Any thread
        gc_stats snapshot() {
            auto _ = std::scoped_lock { _published_mtx };
            return _published;
        }
    };
}
