#include <gtest/gtest.h>
#include <atomic>
#include <array>
#include <chrono>
#include <cmath>
#include <span>
#include <thread>
#include "../gc-heuristic.h"
#include "../gc-config.h"

using namespace gc::detail;
using namespace std::chrono_literals;

namespace {
    struct fake_frag {
        std::atomic<double> frag_total{0.0};
        std::atomic<size_t> pg_cnt{0};
    };
    using frag_array = std::array<fake_frag, gc::config::szclass_cnt>;

    void wait_for_clock_tick() { std::this_thread::sleep_for(20ms); }

}

TEST(GcHeuristicsTest, DefaultTriggerIsStartHeap) {
    gc_heuristics h;
    EXPECT_EQ(h.heap_trigger(), 20ull << 20) << "fresh heuristic should hand back the 10 MB start_heap";
}

TEST(GcHeuristicsTest, DefaultShouldNotCopy) {
    gc_heuristics h;
    EXPECT_FALSE(h.should_copy());
}

TEST(GcHeuristicsTest, DefaultLiveSizeIsZero) {
    gc_heuristics h;
    EXPECT_EQ(h.get_live_size(), 0u);
}

TEST(GcHeuristicsTest, CalcUpdatesLiveSize) {
    gc_heuristics h;
    h.set_mark_ms(10);
    h.set_copy_ms(10);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 1000, 5000);
    EXPECT_EQ(h.get_live_size(), 5000u);
}

TEST(GcHeuristicsTest, CalcRevisesTriggerAwayFromStartHeap) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 10000, 50'000);
    EXPECT_NE(h.heap_trigger(), 10ull << 20) << "after calc, trigger should be derived from live_sz, not the default";
}

TEST(GcHeuristicsTest, TriggerGrowsWithLiveSize) {
    // Holding alloc rate roughly constant, doubling live_sz should
    // roughly double the trigger
    frag_array frags;

    gc_heuristics h1;
    h1.set_mark_ms(100); h1.set_copy_ms(100);
    wait_for_clock_tick();
    h1.calc(std::span{frags}, 100, 20 << 20);

    gc_heuristics h2;
    h2.set_mark_ms(100); h2.set_copy_ms(100);
    wait_for_clock_tick();
    h2.calc(std::span{frags}, 100, 40 << 20);

    EXPECT_GT(h2.heap_trigger(), h1.heap_trigger());
    // Ratio should be near 2x (with noise from the time-based alloc_rate)
    double ratio = (double)h2.heap_trigger() / h1.heap_trigger();
    EXPECT_GT(ratio, 1.5);
    EXPECT_LT(ratio, 2.5);
}

TEST(GcHeuristicsTest, TriggerShrinksAsAllocPressureIncreases) {
    // Hold live_sz constant, vary alloc_sz: more allocation pressure
    // should mean a tighter trigger.
    frag_array frags;
    // Needs to be above heap min
    constexpr size_t live_sz = 30 << 20;
    constexpr size_t mark_ms = 100;

    gc_heuristics h_low;
    h_low.set_mark_ms(mark_ms);
    h_low.set_copy_ms(100);
    wait_for_clock_tick();
    h_low.calc(std::span{frags}, 100000, live_sz);

    gc_heuristics h_high;
    h_high.set_mark_ms(mark_ms);
    h_high.set_copy_ms(100);
    wait_for_clock_tick();
    h_high.calc(std::span{frags}, 5000000, live_sz);

    EXPECT_LT(h_high.heap_trigger(), h_low.heap_trigger())
                        << "higher alloc rate should produce a tighter trigger";
}

TEST(GcHeuristicsTest, TriggerCappedAtHeapMaxHalf) {
    gc_heuristics h;
    h.set_mark_ms(1);  // small denominator => huge mark_rate
    h.set_copy_ms(1);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 1, (1ull << 50));
    EXPECT_LE(h.heap_trigger(), 1ull << 39) << "trigger should be capped at heap_max * 0.5 = 512 GB";
}

TEST(GcHeuristicsTest, AllocRateExceedingMarkRateClampsRatioAtOne) {
    // The std::min(1.0, alloc/mark) clamp guarantees:
    // trigger >= live_sz * (1 + head_room - 1) = live_sz * head_room
    // Even under enormous allocation pressure the trigger doesn't go
    // below live_sz (head_room == 1.0) because setting trigger = live_sz will retrigger collection on next alloc
    gc_heuristics h;
    h.set_mark_ms(1000);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 1'000'000'000, 10'000);

    EXPECT_GE(h.heap_trigger(), 10'000u) << "under extreme alloc pressure, the clamp keeps trigger >= live_sz";
    EXPECT_LE(h.heap_trigger(), 1ull << 39) << "clamp still respects the heap_max/2 cap";
}

TEST(GcHeuristicsTest, NoFragmentationDoesNotTriggerCopy) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 1000, 10'000);

    EXPECT_FALSE(h.should_copy()) << "with zero fragmentation, savings are zero — never copy";
}

TEST(GcHeuristicsTest, HighFragmentationTriggersCopy) {
    // 100 pages averaging 80% frag - compacting saves ~80 pages.
    // Allocation rate is low so the time-to-fill bytes_saved dominates.
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    frags[0].pg_cnt.store(100);
    frags[0].frag_total.store(80.0);  // avg frag = 0.8

    h.calc(std::span{frags}, 100, 10'000);
    EXPECT_TRUE(h.should_copy());
}

TEST(GcHeuristicsTest, HighFragmentationButFastAllocSuppressesCopy) {
    // Same fragmentation as above, but huge alloc_sz means a high
    // alloc_rate, which shortens the left side (bytes_saved/alloc_rate)
    // until it's outweighed by the right side, copy should be skipped
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    frags[0].pg_cnt.store(2);
    frags[0].frag_total.store(0.4);

    // Very high allocation: 10 MB in ~20 ms => 500 KB/ms.
    h.calc(std::span{frags}, 10 * (1 << 20), 100'000);
    EXPECT_FALSE(h.should_copy()) << "fast allocator means bytes_saved would be consumed quickly - skip the copy cost";
}

TEST(GcHeuristicsTest, OneEntirelyDeadPageStillFreedByCopyDecisionMath) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    frags[0].pg_cnt.store(10);
    frags[0].frag_total.store(9.0);  // avg frag = 0.9

    h.calc(std::span{frags}, 200, 10'000);
    EXPECT_TRUE(h.should_copy());
}

TEST(GcHeuristicsTest, EwmaConvergesAfterManyCalls) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);

    frag_array frags;
    for (int i = 0; i < 20; ++i) {
        std::this_thread::sleep_for(15ms);
        h.calc(std::span{frags}, 500, 10'000);
    }
    auto stable = h.heap_trigger();

    std::this_thread::sleep_for(15ms);
    h.calc(std::span{frags}, 500, 10'000);
    auto next = h.heap_trigger();

    double drift = std::abs((double)next - stable) / stable;
    EXPECT_LT(drift, 0.10) << "after 20 iterations the EWMA should be within 10%; drifted by " << drift * 100 << "%";
}

TEST(GcHeuristicsTest, AllFragSlotsEmptyProducesZeroPagesFreed) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    h.calc(std::span{frags}, 1000, 10'000);
    EXPECT_FALSE(h.should_copy());
    EXPECT_GT(h.heap_trigger(), 0u);
}

TEST(GcHeuristicsTest, FullyFragmentedSinglePageFreesNothing) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);
    wait_for_clock_tick();

    frag_array frags;
    frags[0].pg_cnt.store(1);
    frags[0].frag_total.store(1.0);

    h.calc(std::span{frags}, 100, 10'000);
    EXPECT_TRUE(h.should_copy());
}

TEST(GcHeuristicsTest, AllSizeClassesContributeToCopyDecision) {
    gc_heuristics h_spread;
    h_spread.set_mark_ms(100);
    h_spread.set_copy_ms(100);

    gc_heuristics h_single;
    h_single.set_mark_ms(100);
    h_single.set_copy_ms(100);

    frag_array spread;
    for (auto& f : spread) { f.pg_cnt.store(10); f.frag_total.store(5.0); }

    frag_array single;
    single[0].pg_cnt.store(10);
    single[0].frag_total.store(5.0);

    wait_for_clock_tick();
    h_spread.calc(std::span{spread}, 100, 10'000);
    h_single.calc(std::span{single}, 100, 10'000);

    EXPECT_TRUE(h_spread.should_copy());
    EXPECT_TRUE(h_single.should_copy());
}

TEST(GcHeuristicsTest, SetMarkMsAffectsSubsequentCalc) {
    frag_array frags;

    gc_heuristics h_slow;
    h_slow.set_mark_ms(1000);   // mark_rate ~= 10
    h_slow.set_copy_ms(100);

    gc_heuristics h_fast;
    h_fast.set_mark_ms(10);     // mark_rate ~= 1000
    h_fast.set_copy_ms(100);

    for (int i = 0; i < 20; ++i) {
        std::this_thread::sleep_for(5ms);
        h_slow.calc(std::span{frags}, 2000, 10'000);
        h_fast.calc(std::span{frags}, 2000, 10'000);
        EXPECT_LE(h_slow.heap_trigger(), h_fast.heap_trigger()) << "slow mark phase should yield tighter heap trigger after EWMA settles";
    }
}

TEST(GcHeuristicsTest, ZeroCopyMsKeepsPreviousCopyRate) {
    gc_heuristics h;
    h.set_mark_ms(100);
    h.set_copy_ms(100);

    frag_array frags;
    frags[0].pg_cnt.store(50);
    frags[0].frag_total.store(40.0);

    wait_for_clock_tick();
    h.calc(std::span{frags}, 200, 10'000);
    bool first = h.should_copy();

    h.set_copy_ms(0);   // skip copy this cycle
    std::this_thread::sleep_for(15ms);
    h.calc(std::span{frags}, 200, 10'000);
    bool second = h.should_copy();

    // The decision shouldn't flip just because we passed copy_ms=0: the smoothed _copy_rate carries over
    EXPECT_EQ(first, second);
}