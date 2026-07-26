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

TEST(GcHeuristicsTest, DefaultShouldNotCopy) {
    gc_heuristics h;
    EXPECT_FALSE(h.should_copy());
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

TEST(GcHeuristicsTest, TriggerCappedAtHalfMaxHeap) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(1ms), plain_cycle(1, 1ull << 50, 1ms));
    EXPECT_LE(h.heap_trigger(), gc::config::heap_max_sz / 2);
}

// ---------------------------------------------------------------- copy decision

TEST(GcHeuristicsTest, NothingToGainMeansNoCopy) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    h.end_cycle(clk.advance(20ms), plain_cycle(1000, 10'000));
    EXPECT_FALSE(h.should_copy()) << "with nothing to reclaim, compaction is pure cost";
}

TEST(GcHeuristicsTest, LargeGainAgainstSlowAllocTriggersCopy) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(1024, 10'000);
    s.evac_gain_bytes = 80 * config::page_sz;   // ~80 pages come back
    s.evac_move_bytes = 20 * config::page_sz;
    h.end_cycle(clk.advance(20ms), s);
    EXPECT_TRUE(h.should_copy());
}

TEST(GcHeuristicsTest, SameGainAgainstFastAllocSuppressesCopy) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(512 * MB, 10'000);     // the reclaimed bytes are consumed instantly
    s.evac_gain_bytes = 80 * config::page_sz;
    s.evac_move_bytes = 20 * config::page_sz;
    settle(h, clk, s, 20ms);
    EXPECT_FALSE(h.should_copy())
        << "if the gain is consumed faster than the copy costs, skip the copy";
}

// The decision scales with what has to be *moved*. Two cycles reclaiming the same amount must
// disagree when one of them has far more live data to relocate.
TEST(GcHeuristicsTest, CopyDecisionScalesWithBytesToMove) {
    auto run = [](size_t move_bytes) {
        fake_clock clk;
        gc_heuristics h { {}, clk.now() };
        // 1 MB per 20ms, so the 64 reclaimed pages buy ~10ms - comfortably more than the
        // fixed copy cost, leaving the bytes-to-move term to decide.
        auto s = plain_cycle(1 * MB, 10'000);
        s.evac_gain_bytes = 64 * config::page_sz;
        s.evac_move_bytes = move_bytes;
        settle(h, clk, s, 20ms);
        return h.should_copy();
    };
    EXPECT_TRUE(run(1 * config::page_sz))   << "almost nothing to move: worth compacting";
    EXPECT_FALSE(run(4096 * config::page_sz)) << "a mountain to move for the same gain: not worth it";
}

// ---------------------------------------------------------------- degenerate inputs

// A cycle that allocated nothing used to divide by a zero alloc rate, giving +inf (or 0/0)
// for "how long the reclaimed bytes buy us". The rate is floored now, so the decision stays
// finite and stays driven by whether there is anything to gain at all.
TEST(GcHeuristicsTest, ZeroAllocRateProducesNoInfOrNan) {
    fake_clock clk;
    auto cycle_with_gain = [](size_t gain) {
        auto s = plain_cycle(0, 10'000);   // nothing allocated at all
        s.evac_gain_bytes = gain;
        s.evac_move_bytes = gain / 2;
        return s;
    };

    gc_heuristics idle { {}, clk.now() };
    idle.end_cycle(clk.advance(20ms), cycle_with_gain(0));
    EXPECT_LE(idle.heap_trigger(), gc::config::heap_max_sz / 2);
    EXPECT_GE(idle.heap_trigger(), 20ull << 20) << "an idle cycle still yields a sane, floored trigger";
    EXPECT_FALSE(idle.should_copy()) << "nothing to gain is still nothing to gain";

    // With something to gain and no allocation to consume it, compacting is free: the point
    // is that the answer is a definite bool arrived at by finite arithmetic.
    gc_heuristics gainful { {}, clk.now() };
    gainful.end_cycle(clk.advance(20ms), cycle_with_gain(8 * config::page_sz));
    EXPECT_TRUE(gainful.should_copy());
    EXPECT_LE(gainful.heap_trigger(), gc::config::heap_max_sz / 2);
}

TEST(GcHeuristicsTest, ZeroLengthCycleDoesNotDivideByZero) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    // Same time point twice: wall is zero, and so are both phase durations.
    auto now = clk.now();
    h.end_cycle(now, cycle_stats { .allocated_bytes = 1000, .live_bytes = 10'000 });
    EXPECT_TRUE(std::isfinite((double)h.heap_trigger()));
    EXPECT_GT(h.heap_trigger(), 0u);
}

// Whole-millisecond timing dropped any phase under 1ms on the floor, so a fast collector
// learned nothing at all about its own rates.
TEST(GcHeuristicsTest, SubMillisecondPhasesStillMoveTheRates) {
    constexpr size_t live = 40 * MB;
    fake_clock c_fast, c_slow;
    gc_heuristics h_fast { {}, c_fast.now() };
    gc_heuristics h_slow { {}, c_slow.now() };

    // 400us vs 900us of marking: both round to zero milliseconds, but they are not the same
    // marker, and the trigger has to reflect that.
    settle(h_fast, c_fast, plain_cycle(8 * MB, live, 400us), 50ms);
    settle(h_slow, c_slow, plain_cycle(8 * MB, live, 900us), 50ms);

    EXPECT_GT(h_fast.heap_trigger(), h_slow.heap_trigger())
        << "a marker twice as fast should be allowed a looser trigger";
}

// A phase that reports no elapsed time carries no information about the rate, so it must be
// skipped rather than folded in as "instant" (or as a divide by zero). alpha of 1.0 makes each
// measured cycle overwrite the estimate outright, so an ignored sample is exactly detectable:
// the trailing unmeasured cycle must leave the trigger where the measured one put it.
TEST(GcHeuristicsTest, ZeroLengthPhasesLeaveTheRatesAlone) {
    constexpr size_t live = 40 * MB;
    gc_tuning overwrite { .rate_alpha = 1.0 };
    fake_clock c_ref, c_gap;
    gc_heuristics h_ref { overwrite, c_ref.now() };
    gc_heuristics h_gap { overwrite, c_gap.now() };

    auto measured = plain_cycle(8 * MB, live, 2ms);
    measured.copy_time = 3ms;
    measured.evac_move_bytes = 4 * config::page_sz;
    auto unmeasured = measured;
    unmeasured.mark_time = 0ns;
    unmeasured.copy_time = 0ns;

    h_ref.end_cycle(c_ref.advance(50ms), measured);

    h_gap.end_cycle(c_gap.advance(50ms), measured);
    h_gap.end_cycle(c_gap.advance(50ms), unmeasured);

    EXPECT_EQ(h_ref.heap_trigger(), h_gap.heap_trigger());
    EXPECT_EQ(h_ref.should_copy(), h_gap.should_copy());
}

TEST(GcHeuristicsTest, SkippedCopyKeepsThePreviousCopyRate) {
    fake_clock clk;
    gc_heuristics h { {}, clk.now() };
    auto s = plain_cycle(4 * MB, 10'000);
    s.evac_gain_bytes = 40 * config::page_sz;
    s.evac_move_bytes = 8 * config::page_sz;
    s.copy_time = 2ms;

    settle(h, clk, s, 50ms);
    bool before = h.should_copy();

    // A cycle that did not copy reports no copy time; the smoothed rate must carry over
    // rather than being reset by the absence of a measurement.
    s.copy_time = 0ns;
    h.end_cycle(clk.advance(50ms), s);
    EXPECT_EQ(before, h.should_copy());
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
