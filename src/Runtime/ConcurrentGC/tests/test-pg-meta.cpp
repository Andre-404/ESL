#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include "../pg-meta.h"
#include "../obj-allocator.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

TEST(PgMetaTest, ConstructorRecordsBlockSize) {
    test_page p{64};
    EXPECT_EQ(p->block_sz(), 64u);
}

TEST(PgMetaTest, UnknownSizeProducesLargePage) {
    test_page p{ config::sz_classes[config::szclass_cnt-1] + 1 };
    EXPECT_EQ(p->block_cnt(), 1);
    EXPECT_EQ(p->szclass(), config::large_class);
}

TEST(PgMetaTest, SlotRangeFitsInsideOnePage) {
    for (size_t sz : config::sz_classes) {
        test_page p{sz};
        size_t used = p->block_cnt() * sz;
        EXPECT_LE(used, config::page_sz) << "sz=" << sz << " needs " << used << " bytes of page";
    }
}

TEST(PgMetaTest, DataStartsAtAPageBoundary) {
    test_page p{64};
    EXPECT_EQ(reinterpret_cast<size_t>(p->get_data()) % config::page_sz, 0u);
}

TEST(PgMetaTest, AllocWordStartsZeroAndIsWritable) {
    test_page p{64};
    EXPECT_EQ(p->load_alloc_word(0), 0u);
    p->store_alloc_word(0, 0xDEADBEEFCAFEBABEull);
    EXPECT_EQ(p->load_alloc_word(0), 0xDEADBEEFCAFEBABEull);
}

// next_cycle is the sweep: this cycle's marks become the next cycle's alloc bitmap, and the
// page is handed a fresh mark bitmap to fill.
TEST(PgMetaTest, NextCycleAdoptsTheMarkBitmapAsAlloc) {
    test_page p{64};
    p.construct(1);
    p.mark(1);
    p->store_alloc_word(0, 0xAAAAAAAAAAAAAAAAull);

    p.next_cycle();
    EXPECT_EQ(p->load_alloc_word(0), 0b10ull) << "alloc is now what the mark bitmap held";
    EXPECT_EQ(p->compute_live(), 0u) << "and the mark bitmap it got is empty";
    EXPECT_FALSE(p->has_pinned());
}

// The four per-cycle bits share one byte with each other and with nothing else, which is what
// keeps the header at 16 bytes. They are independent and all start clear.
TEST(PgFlagsTest, FlagsAreIndependentAndStartClear) {
    test_page p{64};
    EXPECT_FALSE(p->has_pinned());
    EXPECT_FALSE(p->is_source());
    EXPECT_FALSE(p->any_dirty());
    EXPECT_FALSE(p->has_pinned()) << "flags sharing a byte must not bleed into each other";
    EXPECT_FALSE(p->is_source());
    EXPECT_FALSE(p->any_dirty());

    p->set_dirty();
    p->nominate();
    p.mark(0, true);
    EXPECT_TRUE(p->any_dirty());
    EXPECT_TRUE(p->has_pinned());
}

// The shape bits (first-in-run, has-continuation) now live in the same byte as the cycle bits,
// so the sweep's reset has to leave them alone. Losing first-in-run would make head_from_ptr
// walk off a live page; losing has-continuation would make a multi-page run report one page.
TEST(PgFlagsTest, NextCycleKeepsTheShapeBits) {
    test_page p{ config::page_sz * 3 };
    ASSERT_EQ(p->num_pages(), 3u);
    ASSERT_EQ(pg_meta::head_from_ptr(p->get_data() + config::page_sz * 2 + 8), p.pg());

    p->nominate();
    p.mark(0, true);
    p.next_cycle();

    EXPECT_EQ(p->num_pages(), 3u);
    EXPECT_EQ(pg_meta::head_from_ptr(p->get_data() + config::page_sz * 2 + 8), p.pg());
    EXPECT_EQ(p->block_sz(), config::page_sz * 3);
    EXPECT_FALSE(p->has_pinned());
    EXPECT_FALSE(p->is_source());
}

// A single page has no continuation bit set, and the cycle bits must not fabricate one
TEST(PgFlagsTest, CycleFlagsDoNotCreateAContinuation) {
    test_page p{64};
    p->nominate();
    p->set_dirty();
    p.mark(0, true);
    EXPECT_EQ(p->num_pages(), 1u);
    EXPECT_EQ(p->block_sz(), 64u);
}

TEST(PgFlagsTest, SetSourceRoundTrips) {
    test_page p{64};
    p->nominate();
    EXPECT_TRUE(p->is_source());
    p->demote();
    EXPECT_FALSE(p->is_source());
    p->nominate();
    EXPECT_TRUE(p->is_source());
}

// Every one of these is a statement about this cycle only, so the sweep has to wipe all of them
// along with the mark bitmap. A bit that survived would silently describe the wrong cycle.
TEST(PgFlagsTest, NextCycleClearsEveryPerCycleFlag) {
    test_page p{64};
    p->set_dirty();
    p->nominate();
    p.mark(0, true);
    ASSERT_TRUE(p->has_pinned());

    p.next_cycle();
    EXPECT_FALSE(p->has_pinned());
    EXPECT_FALSE(p->is_source());
    EXPECT_FALSE(p->any_dirty());
}

// The history entry lives in a side array, not in the header, so these check both the packing
// and that a page can find its own entry.
TEST(PgHistoryTest, PacksLiveAndAgeIntoTwoBytes) {
    auto h = pg_history { 4096, 5 };
    EXPECT_EQ(h.occ(), 4096u);
    EXPECT_EQ(h.age(), 5u);
    EXPECT_TRUE(h.known());
}

TEST(PgHistoryTest, DefaultMeansNothingRecordedYet) {
    pg_history h;
    EXPECT_EQ(h.occ(), 0u);
    EXPECT_EQ(h.age(), 0u);
    EXPECT_FALSE(h.known()) << "an empty page is freed rather than recorded, so zero can only "
                               "mean no cycle has seen this page";
}

TEST(PgHistoryTest, SaturatesInsteadOfWrapping) {
    auto live = pg_history { pg_history::live_max + 1, 0 };
    EXPECT_EQ(live.occ(), pg_history::live_max) << "a wrapped count would read as unknown";
    auto age = pg_history { 1, pg_history::age_max + 1 };
    EXPECT_EQ(age.age(), pg_history::age_max);
    EXPECT_EQ(age.occ(), 1u) << "age must not spill into the live count";
}

TEST(PgHistoryTest, EveryCountAndAgeRoundTrips) {
    for (uint16_t live = 0; live <= pg_history::live_max; live++) {
        for (uint8_t age = 0; age <= pg_history::age_max; age++) {
            auto h = pg_history { live, age };
            ASSERT_EQ(h.occ(), live) << "live=" << live << " age=" << int(age);
            ASSERT_EQ(h.age(), age) << "live=" << live << " age=" << int(age);
        }
    }
}

TEST(PgHistoryTest, AFreshPageHasNoHistory) {
    test_page p{64};
    EXPECT_FALSE(p->history().known());
}

TEST(PgHistoryTest, PageHistoryRoundTripsThroughTheSideArray) {
    test_page p{64};
    p->set_history({ 17, 3 });
    EXPECT_EQ(p->history().occ(), 17u);
    EXPECT_EQ(p->history().age(), 3u);

    // Neighbouring pages are neighbouring entries, so an off-by-one in the addressing shows up
    // as one page reading another's record
    test_page q{64};
    EXPECT_FALSE(q->history().known());
    q->set_history({ 5, 1 });
    EXPECT_EQ(p->history().occ(), 17u);
    EXPECT_EQ(q->history().occ(), 5u);
}

TEST(PgHistoryTest, HistorySurvivesTheSweepThatWritesIt) {
    test_page p{64};
    p->set_history({ 9, 2 });
    p.next_cycle();
    EXPECT_EQ(p->history().occ(), 9u) << "next_cycle clears the per-cycle flags, not the record "
                                         "the sweep just wrote";
    EXPECT_EQ(p->history().age(), 2u);
}

TEST(ComputeAllocTest, CountsEveryPublishedAllocBit) {
    test_page p{64};
    EXPECT_EQ(p->compute_alloc(), 0u);
    p.allocate(0);
    p.allocate(63);
    p.allocate(64);
    p.allocate(p->block_cnt() - 1);
    EXPECT_EQ(p->compute_alloc(), 4u);
}

TEST(PgSlotsIterTest, IteratesExactlyBlockCntSlots) {
    test_page p{64};
    pg_meta::pg_slots_iter it{p.pg(), 0};

    int seen = 0;
    while (!it.at_end()) {
        ASSERT_NE(it.get(), nullptr);
        it.next();
        ++seen;
    }
    EXPECT_EQ(seen, p->block_cnt());
    EXPECT_EQ(it.get(), nullptr); // at_end returns nullptr
}

TEST(PgSlotsIterTest, ConstructedAtEndReportsAtEnd) {
    test_page p{64};
    pg_meta::pg_slots_iter it{p.pg(), p->block_cnt()};
    EXPECT_TRUE(it.at_end());
    EXPECT_EQ(it.get(), nullptr);
}

TEST(PgSlotsIterTest, GetReturnsPointersAtExpectedSlotOffsets) {
    test_page p{64};
    pg_meta::pg_slots_iter it{p.pg(), 0};

    for (uint16_t i = 0; i < p->block_cnt(); ++i) {
        ASSERT_FALSE(it.at_end());
        EXPECT_EQ(reinterpret_cast<uint8_t*>(it.get()), p.slot_addr(i)) << "slot " << i;
        it.next();
    }
    EXPECT_TRUE(it.at_end());
}

TEST(PgSlotsIterTest, EverySlotPointsIntoSamePage) {
    for (size_t sz : config::sz_classes) {
        test_page p{sz};
        pg_meta::pg_slots_iter it{p.pg(), 0};
        while (!it.at_end()) {
            EXPECT_EQ(pg_meta::head_from_ptr(it.get()), p.pg()) << "sz=" << sz;
            it.next();
        }
    }
}

// Headers live in a dense array beside the heap, so the mapping is index arithmetic on the
// page's address rather than a mask of the object pointer.
TEST(PgFromPtr, EveryAddressInThePageMapsToItsHeader) {
    test_page p{64};
    auto* data = p->get_data();

    EXPECT_EQ(pg_meta::head_from_ptr(data), p.pg());
    EXPECT_EQ(pg_meta::head_from_ptr(data + 100), p.pg());
    EXPECT_EQ(pg_meta::head_from_ptr(data + config::page_sz - 1), p.pg());
}

TEST(PgFromPtr, EveryPageOfAMultiPageRunMapsToTheRunHead) {
    test_page p{config::page_sz * 2};   // large, spans a run
    ASSERT_EQ(p->num_pages(), 2u);

    auto* data = p->get_data();
    EXPECT_EQ(pg_meta::head_from_ptr(data), p.pg());
    EXPECT_EQ(pg_meta::head_from_ptr(data + config::page_sz), p.pg())
        << "a continuation header must resolve back to the head";
}

TEST(RecordMarkTest, FirstMarkReturnsTrueAndCountsOne) {
    test_page p{64};
    p.construct(0);
    EXPECT_TRUE(p.mark(0));
    EXPECT_EQ(p->compute_live(), 1u);
    EXPECT_FALSE(p->has_pinned());
}

TEST(RecordMarkTest, DuplicateMarkReturnsFalseAndLeavesLiveUnchanged) {
    test_page p{64};
    ASSERT_TRUE(p.mark(3));
    EXPECT_FALSE(p.mark(3)) << "marking the same slot twice must not re-count it";
    EXPECT_EQ(p->compute_live(), 1u);
}

TEST(RecordMarkTest, IsPinnedTrueSetsHasPinned) {
    test_page p{64};
    p.mark(0, true);
    EXPECT_TRUE(p->has_pinned());
}

// An object that cannot move vetoes the page's evacuation, whatever nominated it
TEST(RecordMarkTest, PinningOverridesTheSourceRole) {
    test_page p{64};
    p->nominate();
    ASSERT_TRUE(p->is_source());

    p.mark(0, true);
    EXPECT_TRUE(p->has_pinned());
    EXPECT_FALSE(p->is_source()) << "a pinned page cannot be evacuated";
}

// Nomination races marking, so it has to lose against a pin either way round
TEST(RecordMarkTest, NominationLosesToAnAlreadyPinnedPage) {
    test_page p{64};
    p.mark(0, true);
    ASSERT_TRUE(p->has_pinned());

    EXPECT_FALSE(p->nominate());
    EXPECT_FALSE(p->is_source()) << "a pinned page must not be nominated back into a source";
}

// f_pinned masks f_source rather than clearing it, so the masked bit must not survive the
// sweep - a page that comes back with it set and no pin would be a source nobody nominated
TEST(RecordMarkTest, NextCycleDropsTheMaskedSourceBit) {
    test_page p{64};
    p.mark(0, true);
    p->nominate();
    ASSERT_FALSE(p->is_source());

    p.next_cycle();
    EXPECT_FALSE(p->has_pinned());
    EXPECT_FALSE(p->is_source());
    EXPECT_TRUE(p->nominate()) << "and the page is a candidate again";
    EXPECT_TRUE(p->is_source());
}

TEST(RecordMarkTest, UnpinnedMarkLeavesTheSourceRoleAlone) {
    test_page p{64};
    p->nominate();
    p.mark(0, false);
    EXPECT_TRUE(p->is_source());
    EXPECT_FALSE(p->has_pinned());
}

TEST(RecordMarkTest, IsPinnedFalseDoesNotClearPreviouslySetPinned) {
    test_page p{64};
    p.mark(0, true);
    p.mark(1, false);
    EXPECT_TRUE(p->has_pinned());
}

TEST(RecordMarkTest, MarkingAllSlotsBringsLiveToBlockCnt) {
    test_page p{64};
    for (uint16_t i = 0; i < p->block_cnt(); ++i) ASSERT_TRUE(p.mark(i));
    EXPECT_EQ(p->compute_live(), p->block_cnt());
}

TEST(RecordMarkTest, IsMarkedReflectsMarks) {
    test_page p{64};
    p.mark(5);
    p.mark(20);

    pg_meta::pg_slots_iter it{p.pg(), 0};
    for (uint16_t i = 0; i < p->block_cnt(); ++i) {
        EXPECT_EQ(it.is_marked(), (i == 5 || i == 20)) << "slot " << i;
        it.next();
    }
}

TEST(RecordMarkTest, WorksAtTheLastSlotIndex) {
    test_page p{32};
    uint16_t last = p->block_cnt() - 1;
    EXPECT_TRUE(p.mark(last));
    EXPECT_EQ(p->compute_live(), 1u);

    pg_meta::pg_slots_iter it{p.pg(), last};
    EXPECT_TRUE(it.is_marked());
}

TEST(RecordMarkTest, NextCycleDropsTheMarksItJustAdopted) {
    test_page p{64};
    p.mark(1);
    ASSERT_EQ(p->compute_live(), 1u);

    p.next_cycle();
    EXPECT_EQ(p->compute_live(), 0u);

    pg_meta::pg_slots_iter it{p.pg(), 0};
    for (uint16_t i = 0; i < p->block_cnt(); ++i) {
        EXPECT_FALSE(it.is_marked()) << "slot " << i;
        it.next();
    }
    EXPECT_EQ(p->load_alloc_word(0), 0b10);
}

TEST(FromInteriorTest, PointerPastSlotAreaReturnsNullptr) {
    test_page p{64};
    auto past = p->get_data() + p->block_cnt() * p->block_sz();
    EXPECT_EQ(p->from_interior(past),     nullptr);
    EXPECT_EQ(p->from_interior(past + 8), nullptr);
}

TEST(FromInteriorTest, AllocatedSlotPointerRoundsToSlotStart) {
    test_page p{64};
    // Allocate a few slots through obj_allocator and flush so the alloc bitmap reflects them.
    // Then any interior pointer in those slots must round down to the slot's start.
    obj_allocator a{*p.pg()};
    auto* s0 = reinterpret_cast<uint8_t*>(a.allocate());
    auto* s1 = reinterpret_cast<uint8_t*>(a.allocate());
    auto* s2 = reinterpret_cast<uint8_t*>(a.allocate());
    a.flush_cache();

    EXPECT_EQ(p->from_interior(s0), reinterpret_cast<managed*>(s0));
    EXPECT_EQ(p->from_interior(s1 + 7),  reinterpret_cast<managed*>(s1));
    EXPECT_EQ(p->from_interior(s2 + 63), reinterpret_cast<managed*>(s2));
}

TEST(FromInteriorTest, UnallocatedSlotReturnsNullptr) {
    test_page p{64};
    obj_allocator a{*p.pg()};
    auto* s0 = reinterpret_cast<uint8_t*>(a.allocate());
    a.flush_cache();

    // Slot 1 was never allocated - its alloc bit is 0, so from_interior must reject pointers
    // into it.
    EXPECT_EQ(p->from_interior(p.slot_addr(0) + 1), (managed*)s0);
    EXPECT_EQ(p->from_interior(p.slot_addr(1)),      nullptr);
    EXPECT_EQ(p->from_interior(p.slot_addr(1) + 30), nullptr);
}

TEST(FromInteriorTest, WorksAtFirstAndLastSlot) {
    test_page p{32};
    obj_allocator a{*p.pg()};
    // Drain the page so every alloc bit is set.
    while (a.allocate()) { }
    a.flush_cache();

    auto* first = p.slot_addr(0);
    auto* last  = p.slot_addr(p->block_cnt() - 1);

    EXPECT_EQ(p->from_interior(first),      reinterpret_cast<managed*>(first));
    EXPECT_EQ(p->from_interior(first + 5),  reinterpret_cast<managed*>(first));
    EXPECT_EQ(p->from_interior(last),       reinterpret_cast<managed*>(last));
    EXPECT_EQ(p->from_interior(last + 31),  reinterpret_cast<managed*>(last));
}

// compute_live used to add to a cached live count instead of setting it. A copying cycle
// computes it twice on the same page - once while the copier splits pages into sources and
// targets, and again while the pruner walks them - so every surviving page reported double its
// live count, which fed straight into the fragmentation stats and the heap trigger.
TEST(RecordMarkTest, ComputeLiveIsIdempotent) {
    test_page p{64};
    p.mark(0);
    p.mark(1);
    p.mark(2);

    ASSERT_EQ(p->compute_live(), 3u);
    EXPECT_EQ(p->compute_live(), 3u) << "computing the live count twice must not double it";
    EXPECT_EQ(p->compute_live(), 3u);
}

// The alloc bitmap is the boundary between an allocating thread and the collector's
// conservative stack scan. An object whose alloc word has not been flushed yet is deliberately
// invisible: obj_allocator only publishes a word once every object in it is fully constructed,
// so a scan that found one earlier could trace a half-built object.
TEST(FromInteriorTest, UnflushedAllocationIsDeliberatelyInvisible) {
    test_page p{64};
    obj_allocator a{*p.pg()};
    auto* slot = reinterpret_cast<uint8_t*>(a.allocate());
    ASSERT_NE(slot, nullptr);

    EXPECT_EQ(p->from_interior(slot), nullptr)
        << "before the cache flush the slot is not yet published to the collector";

    a.flush_cache();
    EXPECT_EQ(p->from_interior(slot), reinterpret_cast<managed*>(slot))
        << "the flush is what makes the object findable";
}

TEST(FromInteriorTest, AllocWordAccessorsRoundTripEveryBit) {
    test_page p{64};
    // load/store_alloc_word index by bit and address the containing word, so a bit in the
    // second word must not disturb the first.
    ASSERT_GT(p->block_cnt(), 64u) << "need a page with more than one alloc word";
    p->store_alloc_word(0, 0b1011ull);
    p->store_alloc_word(64, 0b1ull);

    EXPECT_EQ(p->load_alloc_word(0),  0b1011ull);
    EXPECT_EQ(p->load_alloc_word(63), 0b1011ull) << "bit 63 lives in the same word as bit 0";
    EXPECT_EQ(p->load_alloc_word(64), 0b1ull);
}
