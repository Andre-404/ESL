#include <gtest/gtest.h>
#include <chrono>
#include <cmath>
#include "../gc-heuristic.h"
#include "../gc-config.h"

using namespace gc;
using namespace gc::detail;
using namespace std::chrono_literals;

// The heuristic is a pure function of the stats and the time point handed to end_cycle, so
// every test here drives a synthetic clock. Nothing sleeps: the previous version of this file
// had to spend the wall clock to dial in a mark duration, which made a 1000ms mark rate cost
// a literal second and put sub-millisecond phases out of reach entirely.
namespace {
    constexpr size_t MB = 1u << 20;

    class fake_clock {
        gc_clock::time_point _now { gc_clock::time_point {} + 1h };
    public:
        gc_clock::time_point now() const { return _now; }
        gc_clock::time_point advance(std::chrono::nanoseconds d) { _now += d; return _now; }
    };

    // A cycle that allocated `alloc` bytes over `wall`, marked `live` bytes in `mark`, and
    // found nothing worth compacting.
    cycle_stats plain_cycle(size_t alloc, size_t live, std::chrono::nanoseconds mark = 10ms) {
        return cycle_stats { .mark_time = mark, .allocated_bytes = alloc, .live_bytes = live };
    }

    // Runs `n` identical cycles so the EWMA rates settle before the assertion under test.
    void settle(gc_heuristics& h, fake_clock& clk, const cycle_stats& s,
                std::chrono::nanoseconds wall, int n = 30) {
        for (int i = 0; i < n; ++i) h.end_cycle(clk.advance(wall), s);
    }
}

// ---------------------------------------------------------------- defaults

TEST(GcHeuristicsTest, DefaultTriggerIsInitialHeap) {
    gc_heuristics h;
    EXPECT_EQ(h.heap_trigger(), 20ull << 20) << "a fresh heuristic hands back the configured initial_heap";
}

TEST(GcHeuristicsTest, InitialHeapIsConfigurable) {
    gc_heuristics h { gc_tuning { .initial_heap = 256u << 10 } };
    EXPECT_EQ(h.heap_trigger(), 256ull << 10);
}

TEST(GcHeuristicsTest, DefaultLiveSizeIsZero) {
    gc_heuristics h;
    EXPECT_EQ(h.live_size(), 0u);
    EXPECT_EQ(h.cycles(), 0u);
}

// ---------------------------------------------------------------- publication

TEST(GcHeuristicsTest, EndCyclePublishesLiveSize) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(20ms), plain_cycle(1000, 5000));
    EXPECT_EQ(h.live_size(), 5000u);
    EXPECT_EQ(h.cycles(), 1u);
}

TEST(GcHeuristicsTest, EndCycleRevisesTriggerAwayFromInitialHeap) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(20ms), plain_cycle(10'000, 50 * MB));
    EXPECT_NE(h.heap_trigger(), 20ull << 20) << "after a cycle the trigger is derived from live size";
}

// ---------------------------------------------------------------- trigger

TEST(GcHeuristicsTest, TriggerGrowsWithLiveSize) {
    fake_clock c1, c2;
    gc_heuristics h1 { {}, c1.now() };
    gc_heuristics h2 { {}, c2.now() };

    // Same allocation and mark cost either side, only live size differs.
    h1.end_cycle(c1.advance(100ms), plain_cycle(100, 20 * MB, 100ms));
    h2.end_cycle(c2.advance(100ms), plain_cycle(100, 40 * MB, 100ms));

    EXPECT_GT(h2.heap_trigger(), h1.heap_trigger());
    double ratio = (double)h2.heap_trigger() / h1.heap_trigger();
    EXPECT_GT(ratio, 1.5);
    EXPECT_LT(ratio, 2.5);
}

TEST(GcHeuristicsTest, TriggerShrinksAsAllocPressureIncreases) {
    constexpr size_t live = 30 * MB;
    fake_clock c_low, c_high;
    gc_heuristics h_low  { {}, c_low.now() };
    gc_heuristics h_high { {}, c_high.now() };

    // Identical live size and mark cost; the fast allocator gets the tighter trigger.
    settle(h_low,  c_low,  plain_cycle(100 * 1024, live, 100ms), 100ms);
    settle(h_high, c_high, plain_cycle(500 * MB,   live, 100ms), 100ms);

    EXPECT_LT(h_high.heap_trigger(), h_low.heap_trigger())
        << "a higher allocation rate must produce a tighter trigger";
}

TEST(GcHeuristicsTest, TriggerUsesFullHeadroomWhenAllocIsSlow) {
    constexpr size_t live = 30 * MB;
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    // Allocating a trickle against a fast marker: pressure ~= 0, so headroom is max_headroom.
    settle(h, clk, plain_cycle(1024, live, 1ms), 1s);
    EXPECT_NEAR((double)h.heap_trigger(), live * 1.75, live * 0.02);
}

TEST(GcHeuristicsTest, TriggerBottomsOutAtMinHeadroomUnderPressure) {
    constexpr size_t live = 30 * MB;
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    // Mutators far outrunning the marker: pressure clamps at 1, so headroom is min_headroom.
    settle(h, clk, plain_cycle(4096 * MB, live, 1s), 10ms);
    EXPECT_NEAR((double)h.heap_trigger(), live * 1.2625, live * 0.02);
}

TEST(GcHeuristicsTest, TriggerNeverDropsBelowLiveSize) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    // Enormous allocation pressure must not push the trigger under what is already live, or
    // the next allocation would re-trigger immediately.
    settle(h, clk, plain_cycle(4096 * MB, 100 * MB, 1s), 1ms);
    EXPECT_GE(h.heap_trigger(), 100 * MB);
}

TEST(GcHeuristicsTest, TriggerFlooredAtInitialHeap) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(20ms), plain_cycle(1000, 1024));
    EXPECT_GE(h.heap_trigger(), 20ull << 20) << "a tiny live set still gets the initial heap of room";
}

// ---------------------------------------------------------------- black allocation
//
// live_bytes is every mark the sweep found, which includes whatever the cycle allocated after the
// marking flag went live: black, so marked, so counted live whether or not it is reachable. That
// part is this cycle's garbage and the next sweep frees it, so granting headroom on top of it
// charges the heap twice for one allocation - and lets anything that lengthens a cycle, a longer
// pause above all, widen the heap on its own.

TEST(GcHeuristicsTest, BlackAllocationGetsNoHeadroom) {
    constexpr size_t live = 100 * MB, black = 60 * MB;
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(1024, live + black, 1ms);   // a trickle, so full headroom
    s.black_bytes = black;
    settle(h, clk, s, 1s);

    EXPECT_NEAR((double)h.heap_trigger(), live * 1.75 + black, live * 0.02)
        << "headroom on the live set, black bytes at face value";
    EXPECT_LT(h.heap_trigger(), (live + black) * 1.75)
        << "which is strictly less than granting headroom on the occupancy";
}

TEST(GcHeuristicsTest, LiveSizeStaysTheOccupancy) {
    constexpr size_t live = 40 * MB, black = 25 * MB;
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(1024, live + black, 1ms);
    s.black_bytes = black;
    h.end_cycle(clk.advance(20ms), s);

    EXPECT_EQ(h.live_size(), live + black)
        << "mutators add their own allocation to this and compare against the trigger, so it has "
           "to be what the heap holds, garbage and all";
    EXPECT_GE(h.heap_trigger(), live + black) << "and the trigger has to cover it";
}

TEST(GcHeuristicsTest, TriggerCappedAtHalfMaxHeap) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(1ms), plain_cycle(1, 1ull << 50, 1ms));
    EXPECT_LE(h.heap_trigger(), gc::config::heap_max_sz / 2);
}

// ---------------------------------------------------------------- smoothing

TEST(GcHeuristicsTest, EwmaConvergesAfterManyCycles) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(500 * 1024, 10 * MB);

    settle(h, clk, s, 15ms);
    auto stable = h.heap_trigger();
    h.end_cycle(clk.advance(15ms), s);

    double drift = std::abs((double)h.heap_trigger() - stable) / stable;
    EXPECT_LT(drift, 0.01) << "identical cycles should converge; drifted " << drift * 100 << "%";
}

TEST(GcHeuristicsTest, RateAlphaControlsResponsiveness) {
    fake_clock c_slow, c_fast;
    gc_heuristics h_slow { gc_tuning { .rate_alpha = 0.05 }, c_slow.now() };
    gc_heuristics h_fast { gc_tuning { .rate_alpha = 0.9  }, c_fast.now() };

    auto quiet = plain_cycle(1024, 30 * MB, 10ms);
    settle(h_slow, c_slow, quiet, 100ms);
    settle(h_fast, c_fast, quiet, 100ms);

    // One sudden burst of allocation. The eager EWMA must react harder than the sluggish one.
    auto burst = plain_cycle(1024 * MB, 30 * MB, 10ms);
    h_slow.end_cycle(c_slow.advance(100ms), burst);
    h_fast.end_cycle(c_fast.advance(100ms), burst);

    EXPECT_LT(h_fast.heap_trigger(), h_slow.heap_trigger());
}
