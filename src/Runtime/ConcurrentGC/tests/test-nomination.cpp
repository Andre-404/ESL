#include <gtest/gtest.h>

#include "../nomination.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

namespace {
    // Drives one age bucket to `rate`. The EWMA closes (1 - alpha) of the gap to the 0.5 seed
    // per cycle, so 24 of them land within 2e-4 of the target
    void train(survival_model& m, uint8_t age, double rate, int cycles = 24) {
        for (int c = 0; c < cycles; c++) {
            { auto b = m.start_batch(); b.add_retained(age, 1000, uint64_t(1000 * rate)); }
            m.end_cycle();
        }
    }
    const survival_model& cold() {
        static auto m = survival_model {};
        return m;
    }
}

// ---------------------------------------------------------------------------------------------
// The occupancy record, which is unconditional: nomination is what the copy decision gates.
// ---------------------------------------------------------------------------------------------

TEST(NominationTest, TheOccupancyRecordIsWhatThePageHolds) {
    test_page tp{64};
    for (uint16_t i = 0; i < 12; i++) tp.allocate(i);

    observe_pages(tp.pg(), cold(), false);
    EXPECT_EQ(tp->history().occ(), 12u);
    EXPECT_EQ(tp->history().age(), 0u);
}

TEST(NominationTest, TheOccupancyRecordKeepsTheAge) {
    test_page tp{64};
    tp->set_history({ 4, 5 });
    for (uint16_t i = 0; i < 12; i++) tp.allocate(i);

    observe_pages(tp.pg(), cold(), false);
    EXPECT_EQ(tp->history().occ(), 12u);
    EXPECT_EQ(tp->history().age(), 5u) << "only the sweep advances the age";
}

TEST(NominationTest, RetiredPagesAreSkippedEntirely) {
    test_page tp{64};
    tp->set_history({ 7, 2 });
    tp.allocate(0);
    tp->mark_inactive();

    observe_pages(tp.pg(), cold(), true);
    EXPECT_EQ(tp->history().occ(), 7u) << "an inactive page has been evacuated or freed";
    EXPECT_FALSE(tp->is_source());
}

TEST(NominationTest, ANonCopyingCycleNominatesNothing) {
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);

    observe_pages(tp.pg(), cold(), false);
    EXPECT_FALSE(tp->is_source()) << "role bits are only meaningful on a copying cycle";
    EXPECT_EQ(tp->history().occ(), 4u) << "the occupancy record is unconditional";
}

// ---------------------------------------------------------------------------------------------
// pred_live = occ * survival(age) against nominate_threshold * cap.
// ---------------------------------------------------------------------------------------------

TEST(NominationTest, ASparsePageIsNominated) {
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);

    observe_pages(tp.pg(), cold(), true);
    EXPECT_TRUE(tp->is_source());
}

// The case the predictor exists for: two pages with every block allocated, told apart by the
// rate their age bucket has measured. Occupancy alone cannot distinguish them
TEST(NominationTest, AgeDecidesBetweenTwoEquallyFullPages) {
    auto young_rates = survival_model {};
    auto old_rates   = survival_model {};
    train(young_rates, 0, 0.25);
    train(old_rates, 5, 1.0);

    test_page young{64}, old{64};
    auto cap = young->block_cnt();
    for (uint16_t i = 0; i < cap; i++) { young.allocate(i); old.allocate(i); }
    young->set_history({ cap, 0 });
    old->set_history({ cap, 5 });

    observe_pages(young.pg(), young_rates, true);
    observe_pages(old.pg(), old_rates, true);

    EXPECT_TRUE(young->is_source()) << "occ * 0.25 is far under the threshold";
    EXPECT_FALSE(old->is_source()) << "occ * 1.0 is over it, so the page keeps its contents";
}

// With every rate at 1.0 the expression collapses to occ < threshold * cap, i.e. today's
// occupancy test. Measurement can only improve on that, never do worse than it
TEST(NominationTest, RatesAtOneDegenerateToTheOccupancyTest) {
    auto rates = survival_model {};
    train(rates, 3, 1.0);

    test_page full{64}, partial{64};
    auto cap = full->block_cnt();
    for (uint16_t i = 0; i < cap; i++) full.allocate(i);
    for (uint16_t i = 0; i < cap / 2; i++) partial.allocate(i);
    full->set_history({ cap, 3 });
    partial->set_history({ uint16_t(cap / 2), 3 });

    observe_pages(full.pg(), rates, true);
    observe_pages(partial.pg(), rates, true);

    EXPECT_FALSE(full->is_source());
    EXPECT_TRUE(partial->is_source());
}

// A page with no alloc bits is either empty, and about to be retired, or the one the allocator
// is filling right now. Nominating it would cost the allocator a page and buy nothing
TEST(NominationTest, AnEmptyPageIsNotNominated) {
    test_page tp{64};

    observe_pages(tp.pg(), cold(), true);
    EXPECT_FALSE(tp->is_source());
}

TEST(NominationTest, ALargeObjectPageIsNotNominated) {
    test_page big{ config::page_sz * 2 };
    big.allocate(0);

    observe_pages(big.pg(), cold(), true);
    EXPECT_FALSE(big->is_source()) << "the copier never evacuates large objects";
}

// Marking runs concurrently with this pass, so a page can already be pinned when it is scored
TEST(NominationTest, APinnedPageIsNotNominated) {
    test_page tp{64};
    for (uint16_t i = 0; i < 4; i++) tp.allocate(i);
    tp.mark(0, true);
    ASSERT_TRUE(tp->has_pinned());

    observe_pages(tp.pg(), cold(), true);
    EXPECT_FALSE(tp->is_source());
}

TEST(NominationTest, AWholeListIsWalked) {
    test_page a{64}, b{64}, c{64};
    for (uint16_t i = 0; i < 4; i++) { a.allocate(i); b.allocate(i); c.allocate(i); }
    a->link(b.pg());
    b->link(c.pg());

    observe_pages(a.pg(), cold(), true);
    EXPECT_TRUE(a->is_source());
    EXPECT_TRUE(b->is_source());
    EXPECT_TRUE(c->is_source());
    a->unlink();
    b->unlink();
}
