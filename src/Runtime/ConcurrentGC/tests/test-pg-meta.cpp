#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <new>
#include <unordered_set>
#include "../pg-meta.h"
#include "../obj-allocator.h"

using namespace gc;
using namespace gc::detail;

namespace {
    struct page_buf {
        void*    mem = nullptr;
        pg_meta* pg  = nullptr;

        explicit page_buf(size_t block_sz) {
            mem = std::aligned_alloc(config::page_sz, config::page_sz);
            std::memset(mem, 0, config::page_sz);
            pg = new (mem) pg_meta(block_sz);
        }
        ~page_buf() {
            if (pg) pg->~pg_meta();
            std::free(mem);
        }
        page_buf(const page_buf&) = delete;
        page_buf& operator=(const page_buf&) = delete;

        uint8_t* slot_addr(uint16_t i) const {
            return static_cast<uint8_t*>(mem) + pg->start_off() + i * pg->block_sz();
        }
    };

} // namespace

TEST(PgMetaTest, ConstructorRecordsBlockSize) {
    page_buf p{64};
    EXPECT_EQ(p.pg->block_sz(), 64u);
}

TEST(PgMetaTest, UnknownSizeProducesLargePage) {
    page_buf p{config::sz_classes.back() + 1};
    EXPECT_EQ(p.pg->block_cnt(), 1);
}

TEST(PgMetaTest, StartOffMatchesHeaderPlusTwoBitmaps) {
    for (size_t sz : config::sz_classes) {
        page_buf p{sz};
        auto bitmap_sz = (p.pg->block_cnt() + 63) / 64 * 8; // 8 byte aligned
        EXPECT_EQ(p.pg->start_off(), sizeof(pg_meta) + 2 * bitmap_sz);
    }
}

TEST(PgMetaTest, SlotRangeFitsInsideOnePage) {
    for (size_t sz : config::sz_classes) {
        page_buf p{sz};
        size_t last_byte = p.pg->start_off() + (p.pg->block_cnt() - 1) * sz + (sz - 1);
        EXPECT_LT(last_byte, config::page_sz) << "sz=" << sz << " last slot byte at " << last_byte;
    }
}

TEST(PgMetaTest, BitmapsAlignedTo8Bytes) {
    for (size_t sz : config::sz_classes) {
        page_buf p{sz};
        auto bitmap_sz = (p.pg->block_cnt() + 63) / 64 * 8; // 8 byte aligned

        EXPECT_EQ(p.pg->alloc_word(0) % 8, 0);
        p.pg->reset_trackers(); // To flip the bitmaps
        EXPECT_EQ(p.pg->alloc_word(0) % 8, 0);
    }
}

TEST(PgMetaTest, AllocWordStartsZeroAndIsWritable) {
    page_buf p{64};
    EXPECT_EQ(p.pg->alloc_word(0), 0u);
    p.pg->alloc_word(0) = 0xDEADBEEFCAFEBABEull;
    EXPECT_EQ(p.pg->alloc_word(0), 0xDEADBEEFCAFEBABEull);
}

TEST(PgMetaTest, RecycleClearsMarkBitmapAndCounters) {
    page_buf p{64};
    auto* base = reinterpret_cast<uint8_t*>(p.pg);
    constexpr size_t bytes_after_meta = config::page_sz - sizeof(pg_meta);
    std::memset(base + sizeof(pg_meta), 0xFF, bytes_after_meta);

    p.pg->recycle();
    EXPECT_EQ(p.pg->live_count(), 0u);
    EXPECT_FALSE(p.pg->has_pinned());
    // Recycle clears everything
    EXPECT_EQ(p.pg->alloc_word(0), 0u);
}

TEST(PgMetaTest, ResetTrackersZerosCountersAndFlipsBitmap) {
    page_buf p{64};
    // Write through the current alloc word; after reset_trackers, the bitmap
    // halves swap roles, so alloc_word now reads the other (zero) half.
    p.pg->alloc_word(0) = 0xAAAAAAAAAAAAAAAAull;
    p.pg->reset_trackers();
    EXPECT_EQ(p.pg->live_count(), 0u);
    EXPECT_FALSE(p.pg->has_pinned());
    EXPECT_EQ(p.pg->alloc_word(0), 0u);
}

TEST(PgMetaTest, FlipIsItsOwnInverse) {
    page_buf p{64};
    p.pg->alloc_word(0) = 0x1111ull;
    p.pg->reset_trackers();
    p.pg->reset_trackers();
    EXPECT_EQ(p.pg->alloc_word(0), 0x1111ull);
}

TEST(PgSlotsIterTest, IteratesExactlyBlockCntSlots) {
    page_buf p{64};
    pg_meta::pg_slots_iter it{p.pg, 0};

    int seen = 0;
    while (!it.at_end()) {
        ASSERT_NE(it.get(), nullptr);
        it.next();
        ++seen;
    }
    EXPECT_EQ(seen, p.pg->block_cnt());
    EXPECT_EQ(it.get(), nullptr); // at_end returns nullptr
}

TEST(PgSlotsIterTest, ConstructedAtEndReportsAtEnd) {
    page_buf p{64};
    pg_meta::pg_slots_iter it{p.pg, p.pg->block_cnt()};
    EXPECT_TRUE(it.at_end());
    EXPECT_EQ(it.get(), nullptr);
}

TEST(PgSlotsIterTest, GetReturnsPointersAtExpectedSlotOffsets) {
    page_buf p{64};
    pg_meta::pg_slots_iter it{p.pg, 0};

    for (uint16_t i = 0; i < p.pg->block_cnt(); ++i) {
        ASSERT_FALSE(it.at_end());
        auto* expected = p.slot_addr(i);
        EXPECT_EQ(reinterpret_cast<uint8_t*>(it.get()), expected) << "slot " << i;
        it.next();
    }
    EXPECT_TRUE(it.at_end());
}

TEST(PgSlotsIterTest, EverySlotPointsIntoSamePage) {
    for (size_t sz : config::sz_classes) {
        page_buf p{sz};
        pg_meta::pg_slots_iter it{p.pg, 0};
        while (!it.at_end()) {
            EXPECT_EQ(pg_from_obj(it.get()), p.pg) << "sz=" << sz << "what: "<<p.pg->block_cnt();
            it.next();
        }
    }
}

TEST(PgFromObj, RoundsPointerDownToPageBoundary) {
    page_buf p{64};
    auto pg_addr = reinterpret_cast<size_t>(p.pg);
    ASSERT_EQ(pg_addr & (config::page_sz - 1), 0u);

    EXPECT_EQ(pg_from_obj(reinterpret_cast<managed*>(pg_addr + p.pg->start_off())), p.pg);
    EXPECT_EQ(pg_from_obj(reinterpret_cast<managed*>(pg_addr + 100)), p.pg);
    EXPECT_EQ(pg_from_obj(reinterpret_cast<managed*>(pg_addr + config::page_sz - 1)), p.pg);
}

TEST(LargePgHelpers, SizeFormulaAndClassification) {
    EXPECT_EQ(large_pg_sz(100), sizeof(pg_meta) + 2*8 + 100);

    page_buf small{64};
    page_buf large{config::sz_classes.back() + 1};
    EXPECT_FALSE(is_large_pg(small.pg));
    EXPECT_TRUE(is_large_pg(large.pg));
}


namespace {
    managed* slot_of(page_buf& p, uint16_t i) {
        return reinterpret_cast<managed*>(
                reinterpret_cast<uint8_t*>(p.pg) + p.pg->start_off() + i * p.pg->block_sz());
    }
}

TEST(RecordMarkTest, FirstMarkReturnsTrueAndIncrementsLive) {
    page_buf p{64};
    EXPECT_TRUE(p.pg->record_mark(slot_of(p, 0), false));
    EXPECT_EQ(p.pg->live_count(), 1u);
    EXPECT_FALSE(p.pg->has_pinned());
}

TEST(RecordMarkTest, DuplicateMarkReturnsFalseAndLeavesLiveUnchanged) {
    page_buf p{64};
    ASSERT_TRUE(p.pg->record_mark(slot_of(p, 3), false));
    EXPECT_FALSE(p.pg->record_mark(slot_of(p, 3), false)) << "marking the same slot twice must not re-count it";
    EXPECT_EQ(p.pg->live_count(), 1u);
}

TEST(RecordMarkTest, IsPinnedTrueSetsHasPinned) {
    page_buf p{64};
    p.pg->record_mark(slot_of(p, 0), true);
    EXPECT_TRUE(p.pg->has_pinned());
}

TEST(RecordMarkTest, IsPinnedFalseDoesNotClearPreviouslySetPinned) {
    page_buf p{64};
    p.pg->record_mark(slot_of(p, 0), true);
    p.pg->record_mark(slot_of(p, 1), false);
    EXPECT_TRUE(p.pg->has_pinned());
}

TEST(RecordMarkTest, MarkingAllSlotsBringsLiveToBlockCnt) {
    page_buf p{64};
    for (uint16_t i = 0; i < p.pg->block_cnt(); ++i) {
        ASSERT_TRUE(p.pg->record_mark(slot_of(p, i), false));
    }
    EXPECT_EQ(p.pg->live_count(), p.pg->block_cnt());
}

TEST(RecordMarkTest, IsMarkedReflectsMarks) {
    page_buf p{64};
    p.pg->record_mark(slot_of(p, 5), false);
    p.pg->record_mark(slot_of(p, 20), false);

    pg_meta::pg_slots_iter it{p.pg, 0};
    for (uint16_t i = 0; i < p.pg->block_cnt(); ++i) {
        bool expected = (i == 5 || i == 20);
        EXPECT_EQ(it.is_marked(), expected) << "slot " << i;
        it.next();
    }
}

TEST(RecordMarkTest, RecycleClearsMarksAndCounters) {
    page_buf p{64};
    p.pg->record_mark(slot_of(p, 1), true);
    p.pg->record_mark(slot_of(p, 2), false);
    ASSERT_EQ(p.pg->live_count(), 2u);
    ASSERT_TRUE(p.pg->has_pinned());

    p.pg->recycle();
    EXPECT_EQ(p.pg->live_count(), 0u);
    EXPECT_FALSE(p.pg->has_pinned());

    pg_meta::pg_slots_iter it{p.pg, 0};
    for (uint16_t i = 0; i < p.pg->block_cnt(); ++i) {
        EXPECT_FALSE(it.is_marked()) << "slot " << i;
        it.next();
    }
}

TEST(RecordMarkTest, WorksAtTheLastSlotIndex) {
    page_buf p{32};
    uint16_t last = p.pg->block_cnt() - 1;
    EXPECT_TRUE(p.pg->record_mark(slot_of(p, last), false));
    EXPECT_EQ(p.pg->live_count(), 1u);

    pg_meta::pg_slots_iter it{p.pg, last};
    EXPECT_TRUE(it.is_marked());
}

TEST(RecordMarkTest, ResetFlipsBitmaps) {
    page_buf p{64};
    p.pg->record_mark(slot_of(p, 1), false);
    ASSERT_EQ(p.pg->live_count(), 1u);

    p.pg->reset_trackers();
    EXPECT_EQ(p.pg->live_count(), 0u);

    pg_meta::pg_slots_iter it{p.pg, 0};
    for (uint16_t i = 0; i < p.pg->block_cnt(); ++i) {
        EXPECT_FALSE(it.is_marked()) << "slot " << i;
        it.next();
    }
    EXPECT_EQ(p.pg->alloc_word(0), 0b10);
}

TEST(FromInteriorTest, PointerBeforeSlotAreaReturnsNullptr) {
    page_buf p{64};
    auto* base = reinterpret_cast<uint8_t*>(p.pg);
    EXPECT_EQ(p.pg->from_interior(base + 4), nullptr); // inside header
    EXPECT_EQ(p.pg->from_interior(base + p.pg->start_off() - 1), nullptr);
}

TEST(FromInteriorTest, PointerPastSlotAreaReturnsNullptr) {
    page_buf p{64};
    auto* base = reinterpret_cast<uint8_t*>(p.pg);
    auto past = p.pg->start_off() + p.pg->block_cnt() * p.pg->block_sz();
    EXPECT_EQ(p.pg->from_interior(base + past),     nullptr);
    EXPECT_EQ(p.pg->from_interior(base + past + 8), nullptr);
}

TEST(FromInteriorTest, AllocatedSlotPointerRoundsToSlotStart) {
    page_buf p{64};
    // Allocate a few slots through obj_allocator and flush so the alloc
    // bitmap reflects them. Then any interior pointer in those slots must
    // round down to the slot's start.
    obj_allocator a{*p.pg};
    auto* s0 = reinterpret_cast<uint8_t*>(a.allocate());
    auto* s1 = reinterpret_cast<uint8_t*>(a.allocate());
    auto* s2 = reinterpret_cast<uint8_t*>(a.allocate());
    a.flush_cache();

    EXPECT_EQ(p.pg->from_interior(s0), reinterpret_cast<managed*>(s0));
    EXPECT_EQ(p.pg->from_interior(s1 + 7),  reinterpret_cast<managed*>(s1));
    EXPECT_EQ(p.pg->from_interior(s2 + 63), reinterpret_cast<managed*>(s2));
}

TEST(FromInteriorTest, UnallocatedSlotReturnsNullptr) {
    page_buf p{64};
    obj_allocator a{*p.pg};
    auto* s0 = reinterpret_cast<uint8_t*>(a.allocate());
    a.flush_cache();

    // Slot 1 was never allocated - its alloc bit is 0, so from_interior
    // must reject pointers into it.
    auto* base = reinterpret_cast<uint8_t*>(p.pg);
    auto* slot1 = base + p.pg->start_off() + 1 * p.pg->block_sz();
    EXPECT_EQ(p.pg->from_interior(base + p.pg->start_off() + 1), (managed*)s0);
    EXPECT_EQ(p.pg->from_interior(slot1),      nullptr);
    EXPECT_EQ(p.pg->from_interior(slot1 + 30), nullptr);
}

TEST(FromInteriorTest, WorksAtFirstAndLastSlot) {
    page_buf p{32};
    obj_allocator a{*p.pg};
    // Drain the page so every alloc bit is set.
    while (a.allocate()) { }
    a.flush_cache();

    auto* base  = reinterpret_cast<uint8_t*>(p.pg);
    auto* first = base + p.pg->start_off();
    auto* last  = base + p.pg->start_off() + (p.pg->block_cnt() - 1) * p.pg->block_sz();

    EXPECT_EQ(p.pg->from_interior(first),      reinterpret_cast<managed*>(first));
    EXPECT_EQ(p.pg->from_interior(first + 5),  reinterpret_cast<managed*>(first));
    EXPECT_EQ(p.pg->from_interior(last),       reinterpret_cast<managed*>(last));
    EXPECT_EQ(p.pg->from_interior(last + 31),  reinterpret_cast<managed*>(last));
}
