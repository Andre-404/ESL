#pragma once

#include <array>
#include <chrono>
#include <cstdint>
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

        std::array<stat, (size_t)phase::count> _stats {};

        const stat& get(phase p) const { return _stats[(size_t)p]; }
        static double to_ms(ns d) { return std::chrono::duration<double, std::milli>(d).count(); }

    public:
        class [[nodiscard]] scope {
            gc_metrics* _owner;
            phase _p;
            clock::time_point _start;

        public:
            scope(gc_metrics* owner, phase p) : _owner(owner), _p(p), _start(clock::now()) {}
            ~scope() { if(_owner) _owner->_stats[(size_t)_p].record(clock::now() - _start); }
            scope(const scope&) = delete;
            scope& operator=(const scope&) = delete;
        };

        scope time(phase p) { return { this, p }; }
        scope maybe_time(phase p, bool time) { return { time ? this : nullptr, p }; }

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
