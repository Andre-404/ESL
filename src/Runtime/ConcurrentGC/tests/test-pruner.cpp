#include <gtest/gtest.h>
#include <cmath>
#include <cstring>
#include <memory>
#include <new>
#include <unordered_set>
#include <vector>
#include "../pruner.h"
#include "../page-allocator.h"
#include "../pg-meta.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

namespace {
    std::vector<pg_meta*> walk(pg_meta* head, int max = 32) {
        std::vector<pg_meta*> out;
        for (auto* p = head; p && (int)out.size() < max; p = p->next()) {
            out.push_back(p);
        }
        return out;
    }

    struct prune_result {
        pg_meta* empty;
        pg_meta* in_use;
    };

    // prune used to return { empty, in_use }; it now hands empty pages to a callback one at a
    // time and returns only the in_use chain. Prepending them here is exactly what prune did
    // internally, so both chains - and the reversed order of the empty one - stay as they were.
    prune_result prune(pruner& p, pg_meta* list) {
        auto empty = (pg_meta*)nullptr;
        auto in_use = p.prune(list, gc::test::bits(), [&](pg_meta* pg) {
            pg->link(empty);
            empty = pg;
        });
        return { empty, in_use };
    }
}

TEST(PrunerTest, PruneEmptyListReturnsNullPair) {
    pruner p;
    auto [empty, in_use] = prune(p, nullptr);
    EXPECT_EQ(empty,  nullptr);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(p.live_bytes(), 0);
}

TEST(PrunerTest, SingleEmptyPageEndsInEmptyChain) {
    pruner p;
    test_page tp{64};
    auto [empty, in_use] = prune(p, tp.pg());
    EXPECT_EQ(empty,  tp.pg());
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(empty->next(), nullptr);
}

TEST(PrunerTest, SingleInUsePageEndsInInUseChain) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);

    auto in_use = p.prune(tp.pg(), gc::test::bits(), [](pg_meta*) { EXPECT_TRUE(false); });
    EXPECT_EQ(in_use, tp.pg());
    EXPECT_EQ(p.live_bytes(), 5 * 64);
}

TEST(PrunerTest, ChainOfThreeAllEmptyEndsAsThreeEmptyPages) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg()->link(b.pg());
    b.pg()->link(c.pg());

    auto [empty, in_use] = prune(p, a.pg());
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(walk(empty).size(), 3);
}

TEST(PrunerTest, ChainOfThreeAllInUseEndsAsThreeInUsePagesPreservingOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.mark_n(1); b.mark_n(2); c.mark_n(3);
    a.pg()->link(b.pg()); b.pg()->link(c.pg());

    auto [empty, in_use] = prune(p, a.pg());
    EXPECT_EQ(empty, nullptr);

    auto chain = walk(in_use);
    ASSERT_EQ(chain.size(), 3u);
    EXPECT_EQ(chain[0], a.pg());
    EXPECT_EQ(chain[1], b.pg());
    EXPECT_EQ(chain[2], c.pg());
}

TEST(PrunerTest, InUseChainLastPointerMustNotLeakIntoEmptyChain) {
    pruner p;
    test_page a{64}, b{64};
    a.mark_n(1);   // in_use
    // b is empty
    a.pg()->link(b.pg());

    auto [empty, in_use] = prune(p, a.pg());
    ASSERT_EQ(in_use, a.pg());
    EXPECT_EQ(in_use->next(), nullptr)<<"in_use should not be connected to empty in any way";
}

TEST(PrunerTest, AlternatingInUseAndEmptyProducesCleanChains) {
    pruner p;
    test_page a{64}, b{64}, c{64}, d{64};
    a.mark_n(1);  // in_use
    // b empty
    c.mark_n(1);  // in_use
    // d empty
    a.pg()->link(b.pg()); b.pg()->link(c.pg()); c.pg()->link(d.pg());

    auto [empty, in_use] = prune(p, a.pg());

// Expected: in_use is exactly {a, c}, empty is exactly {b, d} (any order).
    auto in_use_chain = walk(in_use);
    auto empty_chain  = walk(empty);

    std::unordered_set<pg_meta*> in_use_set(in_use_chain.begin(), in_use_chain.end());
    std::unordered_set<pg_meta*> empty_set (empty_chain.begin(),  empty_chain.end());

    EXPECT_EQ(in_use_chain.size(), 2);
    EXPECT_TRUE(in_use_set.count(a.pg()));
    EXPECT_TRUE(in_use_set.count(c.pg()));
    EXPECT_FALSE(in_use_set.count(b.pg())) << "b should not appear in in_use chain";
    EXPECT_FALSE(in_use_set.count(d.pg())) << "d should not appear in in_use chain";

    EXPECT_EQ(empty_chain.size(), 2);
    EXPECT_TRUE(empty_set.count(b.pg()));
    EXPECT_TRUE(empty_set.count(d.pg()));
}

// The per size class fragmentation table is private now; estimate_evacuation is the only way
// out of the pruner, so the fragmentation it accumulates is asserted through what compaction
// would gain and what it would have to move.
TEST(PrunerTest, HalfFullPagesYieldHalfTheirBytesBack) {
    pruner p;
    // Four half full pages pack into two, so two pages worth comes back and the live half
    // of all four has to move.
    test_page a{64}, b{64}, c{64}, d{64};
    auto cap  = a.pg()->block_cnt();
    auto half = cap / 2;
    for (auto* tp : { &a, &b, &c, &d }) tp->mark_n(half);
    a.pg()->link(b.pg()); b.pg()->link(c.pg()); c.pg()->link(d.pg());

    prune(p, a.pg());

    auto live_frac = (double)half / cap;
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, (4 - (size_t)std::ceil(4 * live_frac)) * config::page_sz);
    EXPECT_EQ(e.move_bytes, (size_t)(4 * live_frac * config::page_sz));
}

// The estimate used to charge compaction for the *free* part of a page, which made the copy
// decision most pessimistic exactly where compaction pays best. Sparse pages must report far
// less to move than they hand back.
TEST(PrunerTest, MovableBytesAreTheLivePartNotTheFreePart) {
    pruner p;
    std::vector<std::unique_ptr<test_page>> pages;
    for (int i = 0; i < 10; ++i) pages.push_back(std::make_unique<test_page>(64));
    auto cap = pages[0]->pg()->block_cnt();
    // ~10% live, so ~90% fragmented.
    for (auto& tp : pages) tp->mark_n(cap / 10);
    for (size_t i = 0; i + 1 < pages.size(); ++i) pages[i]->pg()->link(pages[i + 1]->pg());

    prune(p, pages[0]->pg());

    auto e = p.estimate_evacuation();
    EXPECT_GT(e.gain_bytes, 0u);
    EXPECT_LT(e.move_bytes, e.gain_bytes)
        << "at ~90% fragmentation there is far less live data to move than there is to reclaim";
    // The live tenth of ten pages, not the dead nine tenths.
    EXPECT_LT(e.move_bytes, 2 * config::page_sz);
}

TEST(PrunerTest, FullyMarkedPageYieldsNothingToReclaim) {
    pruner p;
    test_page tp{64};
    tp.mark_n(tp.pg()->block_cnt());

    prune(p, tp.pg());
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u) << "a full page cannot be compacted away";
    EXPECT_EQ(e.move_bytes, config::page_sz);
}

TEST(PrunerTest, EmptyPagesDoNotContributeToFragmentationStats) {
    pruner p;
    test_page tp{64};
    prune(p, tp.pg());

    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u) << "an empty page is freed outright, it is not compaction's business";
}

TEST(PrunerTest, LargePagesContributeToLiveSizeButNotFrag) {
    pruner p;
    test_page big{4000};  // unknown size -> large, block_cnt=1
    big.construct(0);
    big.mark(0);

    prune(p, big.pg());

    // A large page's block is its whole run, so the run is what it costs
    EXPECT_EQ(p.live_bytes(), config::page_sz);
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u) << "large pages are never evacuation candidates";
}

TEST(PrunerTest, FragStatsAccumulateAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{64};
    auto cap = a.pg()->block_cnt();
    a.mark_n(cap / 4);
    b.mark_n(cap / 4);

    prune(p, a.pg());
    auto after_one = p.estimate_evacuation();
    prune(p, b.pg());
    auto after_two = p.estimate_evacuation();

    // Two quarter full pages pack into one, which a single page on its own cannot do.
    EXPECT_EQ(after_one.gain_bytes, 0u);
    EXPECT_EQ(after_two.gain_bytes, config::page_sz);
    EXPECT_GT(after_two.move_bytes, after_one.move_bytes) << "the second page's live data must be counted too";
}

TEST(PrunerTest, LiveSizeAccumulatesAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{32};
    a.mark_n(5);   // 5 * 64 = 320
    b.mark_n(10);  // 10 * 32 = 320

    prune(p, a.pg());
    EXPECT_EQ(p.live_bytes(), 320);
    prune(p, b.pg());
    EXPECT_EQ(p.live_bytes(), 640);
}

TEST(PrunerTest, EndCycleZeroesLiveSizeAndAllFragSlots) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);
    prune(p, tp.pg());
    ASSERT_GT(p.live_bytes(), 0);
    ASSERT_GT(p.estimate_evacuation().move_bytes, 0u);

    (void)p.end_cycle();

    EXPECT_EQ(p.live_bytes(), 0u);
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u);
}

TEST(PrunerTest, PostPruneAllPagesHaveNoLiveBlocks) {
    pruner p;
    test_page a{64}, b{64};
    a.mark_n(7);   // in_use, will become "previously alive"
    // b empty
    a.pg()->link(b.pg());

    prune(p, a.pg());

    EXPECT_EQ(a.pg()->compute_live(), 0);
    EXPECT_EQ(b.pg()->compute_live(), 0);
}

TEST(PrunerTest, PruneFlipsBitmapMakingMarksTheNewAllocBitmap) {
    pruner p;
    test_page tp{64};
    tp.mark_n(3);

    size_t pre_alloc  = tp.pg()->load_alloc_word(0);
    prune(p, tp.pg());
    size_t post_alloc = tp.pg()->load_alloc_word(0);

    EXPECT_NE(pre_alloc, post_alloc);
    EXPECT_EQ(post_alloc & 0b111ull, 0b111ull)<<"after flip, alloc bitmap takes on the prior mark bitmap values";
}

TEST(PrunerTest, EmptyChainTerminatesAtNullptr) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg()->link(b.pg()); b.pg()->link(c.pg());

    auto [empty, in_use] = prune(p, a.pg());
    auto chain = walk(empty);
    ASSERT_EQ(chain.size(), 3);
    EXPECT_EQ(chain.back()->next(), nullptr);
}

TEST(PrunerTest, EmptyChainOrderIsReverseOfInputOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg()->link(b.pg()); b.pg()->link(c.pg());

    auto [empty, in_use] = prune(p, a.pg());
    auto chain = walk(empty);
    ASSERT_EQ(chain.size(), 3u);
    EXPECT_EQ(chain[0], c.pg());
    EXPECT_EQ(chain[1], b.pg());
    EXPECT_EQ(chain[2], a.pg());
}

TEST(PrunerTest, MixedLargeAndSmallPagesAreSeparatedCorrectly) {
    pruner p;
    test_page small{64};   // empty small
    test_page big{4000};   // in_use big
    big.construct(0);
    big.mark(0);

    small.pg()->link(big.pg());
    auto [empty, in_use] = prune(p, small.pg());

    EXPECT_EQ(empty, small.pg());
    EXPECT_EQ(in_use, big.pg());
    EXPECT_EQ(empty->next(), nullptr);

    EXPECT_EQ(p.live_bytes(), config::page_sz);
}
// ---------------------------------------------------------------------------------------------
// Liveness history and the survival rates that will feed role nomination.
//
// The pair being measured is deliberately the two numbers the collector uses itself: what a page
// held when the cycle started (recorded by observe_pages, outside the pause) against what the
// sweep finds live at the end of it.
// ---------------------------------------------------------------------------------------------

TEST(PrunerHistoryTest, ObserveOccupancyRecordsWhatThePageHolds) {
    pruner p;
    test_page tp{64};
    for (uint16_t i = 0; i < 12; i++) tp.allocate(i);

    p.observe_pages(tp.pg(), false);
    EXPECT_EQ(tp->history().occ(), 12u);
    EXPECT_EQ(tp->history().age(), 0u);
}

TEST(PrunerHistoryTest, ObserveOccupancyKeepsTheAge) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 4, 5 });
    for (uint16_t i = 0; i < 12; i++) tp.allocate(i);

    p.observe_pages(tp.pg(), false);
    EXPECT_EQ(tp->history().occ(), 12u);
    EXPECT_EQ(tp->history().age(), 5u) << "only the sweep advances the age";
}

TEST(PrunerHistoryTest, ObserveOccupancySkipsRetiredPages) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 7, 2 });
    tp->mark_inactive();

    p.observe_pages(tp.pg(), false);
    EXPECT_EQ(tp->history().occ(), 7u) << "an inactive page has been evacuated or freed";
}

TEST(PrunerHistoryTest, TheSweepRecordsTheExactLiveCount) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 20, 0 });
    tp.mark_n(5);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().occ(), 5u) << "what the sweep found becomes the next cycle's base";
}

TEST(PrunerHistoryTest, KeepingMostOfWhatItHeldAgesThePage) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 2 });
    tp.mark_n(6);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().age(), 3u);
}

TEST(PrunerHistoryTest, LosingMostOfWhatItHeldStartsTheAgeOver) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 6 });
    tp.mark_n(3);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().age(), 0u) << "its population is about a cycle old, whatever the "
                                          "page's own history";
}

// The allocator fills holes in pages worth keeping, and those pages must not be read as
// nurseries. What protects them is the retention test itself, not a special case for having been
// allocated into: a page that kept most of what it held ages whoever put the objects there.
TEST(PrunerHistoryTest, ARefilledPageThatKeptItsBlocksStillAges) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 4 });
    for (uint16_t i = 0; i < 20; i++) tp.allocate(i);
    tp.mark_n(8);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().age(), 5u);
}

TEST(PrunerHistoryTest, ARefilledPageThatLostMostOfItStartsOver) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 4 });
    for (uint16_t i = 0; i < 20; i++) tp.allocate(i);
    tp.mark_n(3);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().age(), 0u);
    EXPECT_EQ(tp->history().occ(), 3u) << "the count itself is still exact and worth recording";
}

TEST(PrunerHistoryTest, AgeSaturatesAtTheTop) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 5, pg_history::age_max });
    tp.mark_n(5);

    prune(p, tp.pg());
    EXPECT_EQ(tp->history().age(), pg_history::age_max);
}

TEST(PrunerHistoryTest, AFirstCyclePageIsRecordedButNotScored) {
    pruner p;
    test_page tp{64};
    tp.mark_n(2);                  // no observe_pages ran, so nothing predicted this page

    prune(p, tp.pg());
    p.end_cycle();

    EXPECT_EQ(tp->history().occ(), 0u) << "the sweep has nothing to score against, and the next "
                                          "cycle's occupancy pass gives it a base";
    for (uint8_t age = 0; age < config::pg_age_cnt; age++)
        EXPECT_EQ(p.survival().survival(age), config::survival_seed) << "age " << int(age);
}

// ---------------------------------------------------------------------------------------------
// Nomination: pred_live = occ * survival(age), nominate when pred_live < threshold * cap.
// The pass is a superset generator, so what matters is that it separates pages whose age says
// their contents stick from pages whose age says they do not, at identical occupancy.
// ---------------------------------------------------------------------------------------------

namespace {
    // Drives one age bucket's rate to `rate` by handing the sweep pages that kept exactly that
    // fraction of what they held. The EWMA closes (1 - alpha)^n of the gap to the 0.5 seed, so
    // 24 cycles lands within 2e-4 of the target
    void train_rate(pruner& p, uint8_t age, double rate, int cycles = 24) {
        test_page tp{64};
        auto cap  = tp->block_cnt();
        auto live = int(cap * rate);
        for (int c = 0; c < cycles; c++) {
            tp->set_history({ cap, age });
            tp.mark_n(live);
            prune(p, tp.pg());
            p.end_cycle();
        }
        tp->unlink();
    }
}

TEST(PrunerNominationTest, ANonCopyingCycleNominatesNothing) {
    pruner p;
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);

    p.observe_pages(tp.pg(), false);
    EXPECT_FALSE(tp->is_source()) << "role bits are only meaningful on a copying cycle";
    EXPECT_EQ(tp->history().occ(), 4u) << "the occupancy record is unconditional";
}

TEST(PrunerNominationTest, ASparsePageIsNominated) {
    pruner p;
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);

    p.observe_pages(tp.pg(), true);
    EXPECT_TRUE(tp->is_source());
}

// The case the whole predictor exists for: two pages with every block allocated, told apart by
// the rate their age bucket has measured. Occupancy alone cannot distinguish them
TEST(PrunerNominationTest, AgeDecidesBetweenTwoEquallyFullPages) {
    pruner p;
    train_rate(p, 0, 0.25);
    train_rate(p, 5, 1.0);

    test_page young{64}, old{64};
    auto cap = young->block_cnt();
    for (uint16_t i = 0; i < cap; i++) { young.allocate(i); old.allocate(i); }
    young->set_history({ cap, 0 });
    old->set_history({ cap, 5 });

    p.observe_pages(young.pg(), true);
    p.observe_pages(old.pg(), true);

    EXPECT_TRUE(young->is_source()) << "occ * 0.25 is far under the threshold";
    EXPECT_FALSE(old->is_source()) << "occ * 1.0 is over it, so the page keeps its contents";
}

// With every rate at 1.0 the expression collapses to occ < threshold * cap, i.e. today's
// occupancy test. Measurement can only improve on that, never do worse than it
TEST(PrunerNominationTest, RatesAtOneDegenerateToTheOccupancyTest) {
    pruner p;
    train_rate(p, 3, 1.0);

    test_page full{64}, partial{64};
    auto cap = full->block_cnt();
    for (uint16_t i = 0; i < cap; i++) full.allocate(i);
    for (uint16_t i = 0; i < cap / 2; i++) partial.allocate(i);
    full->set_history({ cap, 3 });
    partial->set_history({ uint16_t(cap / 2), 3 });

    p.observe_pages(full.pg(), true);
    p.observe_pages(partial.pg(), true);

    EXPECT_FALSE(full->is_source());
    EXPECT_TRUE(partial->is_source());
}

// A page with no alloc bits is either empty, and about to be retired, or the one the allocator
// is filling right now. Nominating it would cost the allocator a page and buy nothing
TEST(PrunerNominationTest, AnEmptyPageIsNotNominated) {
    pruner p;
    test_page tp{64};

    p.observe_pages(tp.pg(), true);
    EXPECT_FALSE(tp->is_source());
}

TEST(PrunerNominationTest, ALargeObjectPageIsNotNominated) {
    pruner p;
    test_page big{ config::page_sz * 2 };
    big.allocate(0);

    p.observe_pages(big.pg(), true);
    EXPECT_FALSE(big->is_source()) << "the copier never evacuates large objects";
}

// Marking runs concurrently with this pass, so the page may already be pinned when it is
// scored. Roles only relax source -> target, never back
TEST(PrunerNominationTest, APinnedPageIsNotNominated) {
    pruner p;
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);
    tp.mark(0, true);
    ASSERT_TRUE(tp->has_pinned());

    p.observe_pages(tp.pg(), true);
    EXPECT_FALSE(tp->is_source());
}

TEST(PrunerNominationTest, AnInactivePageIsSkippedEntirely) {
    pruner p;
    test_page tp{64};
    tp.allocate(0);
    tp->mark_inactive();

    p.observe_pages(tp.pg(), true);
    EXPECT_FALSE(tp->is_source());
}

TEST(PrunerSurvivalTest, TheSampleIsWhatTheCycleKeptOfWhatItStartedWith) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 3 });
    tp.mark_n(2);                  // a quarter of what it held is still live

    prune(p, tp.pg());
    p.end_cycle();

    auto expected = (1.0 - config::survival_alpha) * config::survival_seed
                  + config::survival_alpha * 0.25;
    EXPECT_NEAR(p.survival().survival(3), expected, 1e-12);
    EXPECT_EQ(p.survival().survival(2), config::survival_seed);
}

// A refilled page is still sampled: what the copier will find in it is exactly what the rate has
// to predict, and refill is part of that. Only the age comparison is held back.
TEST(PrunerSurvivalTest, ARefilledPageStillContributesItsSample) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 8, 3 });
    tp.mark_n(8);

    prune(p, tp.pg());
    p.end_cycle();

    auto expected = (1.0 - config::survival_alpha) * config::survival_seed
                  + config::survival_alpha * 1.0;
    EXPECT_NEAR(p.survival().survival(3), expected, 1e-12);
}

// One rate per age over all the blocks of that age, not an average of per-page ratios: a page
// holding four survivors has to count for four times one holding one.
TEST(PrunerSurvivalTest, SamplesAccumulateOverBlocksAcrossPagesAndBatches) {
    pruner p;
    test_page a{64}, b{64};
    a->set_history({ 4, 1 });
    b->set_history({ 4, 1 });
    a.mark_n(4);
    b.mark_n(2);

    prune(p, a.pg());        // separate calls, so separate batches
    prune(p, b.pg());
    p.end_cycle();

    auto expected = (1.0 - config::survival_alpha) * config::survival_seed
                  + config::survival_alpha * (6.0 / 8.0);
    EXPECT_NEAR(p.survival().survival(1), expected, 1e-12);
}

// An empty page is retired before any of this: it has no record to update and nothing to sample,
// and the blocks it lost are accounted for by the page leaving the heap.
TEST(PrunerSurvivalTest, RetiredPagesContributeNoSample) {
    pruner p;
    test_page tp{64};
    tp->set_history({ 4, 1 });

    prune(p, tp.pg());
    p.end_cycle();
    EXPECT_EQ(p.survival().survival(1), config::survival_seed);
}
