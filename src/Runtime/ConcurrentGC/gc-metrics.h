#pragma once

#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <limits>
#include <string>

#include "../../Includes/fmt/core.h"

namespace gc::detail {
    class gc_metrics {
    public:
        enum class phase : uint8_t {
            pause = 0, 
            mark,
            copy,
            sweep,
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

        double max_pause_ms() const { return to_ms(get(phase::pause).max); }
        double last_pause_ms() const { return to_ms(get(phase::pause).last); }
        double mark_pause_ms() const { return to_ms(get(phase::mark).last); }
        double copy_pause_ms() const { return to_ms(get(phase::copy).last); }
        double sweep_pause_ms() const { return to_ms(get(phase::sweep).last); }

        double max_ms(phase p) const { return to_ms(get(p).max); }
        double avg_ms(phase p) const {
            auto& s = get(p);
            return s.samples ? to_ms(s.total) / s.samples : 0.0;
        }
        ns last(phase p) const { return get(p).last; }
        size_t cycles() const { return get(phase::pause).samples; }

        std::string report() const {
            return fmt::format(
                "GC pause metrics over {} cycle(s) (last / max / avg ms):\n"
                "  pause  {:.3f} / {:.3f} / {:.3f}\n"
                "  mark   {:.3f} / {:.3f} / {:.3f}\n"
                "  copy   {:.3f} / {:.3f} / {:.3f}\n"
                "  sweep  {:.3f} / {:.3f} / {:.3f}\n",
                cycles(),
                last_pause_ms(), max_ms(phase::pause), avg_ms(phase::pause),
                mark_pause_ms(), max_ms(phase::mark), avg_ms(phase::mark),
                copy_pause_ms(), max_ms(phase::copy), avg_ms(phase::copy),
                sweep_pause_ms(), max_ms(phase::sweep), avg_ms(phase::sweep));
        }
    };
}
