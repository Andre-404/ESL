#pragma once

#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <limits>
#include <string>
#include <utility>

#include "../../Includes/fmt/core.h"

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

        double max_ms(phase p) const { return to_ms(get(p).max); }
        double avg_ms(phase p) const {
            auto& s = get(p);
            return s.samples ? to_ms(s.total) / s.samples : 0.0;
        }
        size_t cycles() const { return get(phase::pause).samples; }

        std::string report() const {
            static constexpr std::pair<phase, const char*> rows[] = {
                { phase::pause, "pause" }, { phase::ttsp, "ttsp" }, { phase::stw, "stw" },
                { phase::mark, "mark" }, { phase::copy, "copy" }, { phase::sweep, "sweep" },
            };
            auto out = fmt::format("GC pause metrics over {} cycle(s) (last / max / avg ms):\n", cycles());
            for (auto [p, name] : rows)
                out += fmt::format("  {:<6} {:.3f} / {:.3f} / {:.3f}\n", name, to_ms(get(p).last), max_ms(p), avg_ms(p));
            return out;
        }
    };
}
