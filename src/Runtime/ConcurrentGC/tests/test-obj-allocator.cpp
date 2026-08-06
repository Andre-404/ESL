#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <new>
#include <unordered_set>
#include "../obj-allocator.h"
#include "../pg-meta.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;

using gc::test::test_page;

TEST(ObjAllocatorTest, DefaultConstructedHasNoPage) {
    obj_allocator a;
    EXPECT_EQ(a.get_pg(), nullptr);
}

TEST(ObjAllocatorTest, GetPgReturnsTheBoundPage) {
    test_page p{64};
    obj_allocator a{*p.pg()};
    EXPECT_EQ(a.get_pg(), p.pg());
}

TEST(ObjAllocatorTest, FirstAllocationLandsAtPageStartOff) {
    for (size_t sz: config::sz_classes) {
        test_page p{sz};
        obj_allocator a{*p.pg()};
        auto *m = a.allocate();
        ASSERT_NE(m, nullptr);
        EXPECT_EQ(reinterpret_cast<uint8_t *>(m), p.slot_addr(0));
    }
}

TEST(ObjAllocatorTest, ConsecutiveAllocationsAreSpacedByBlockSize) {
    for (size_t sz: config::sz_classes) {
        test_page p{sz};
        obj_allocator a{*p.pg()};
        auto* first  = reinterpret_cast<uint8_t*>(a.allocate());
        auto* second = reinterpret_cast<uint8_t*>(a.allocate());
        ASSERT_NE(first,  nullptr);
        ASSERT_NE(second, nullptr);
        EXPECT_EQ(second - first, sz);
    }
}

TEST(ObjAllocatorTest, AllReturnedPointersStayInsideTheSamePage) {
    for (size_t sz: config::sz_classes) {
        test_page p{sz};
        obj_allocator a{*p.pg()};
        while (auto* m = a.allocate()) {
            EXPECT_EQ(pg_meta::head_from_ptr(m), p.pg());
        }
    }
}

TEST(ObjAllocatorTest, AllocatedPointersCoverContiguousSlots) {
    test_page p{64};
    obj_allocator a{*p.pg()};

    std::unordered_set<size_t> got;
    while (auto* m = a.allocate()) {
        got.insert(reinterpret_cast<size_t>(m));
    }
    ASSERT_EQ((int)got.size(), p.pg()->block_cnt());

    for (uint16_t i = 0; i < p.pg()->block_cnt(); ++i)
        EXPECT_TRUE(got.count((size_t)p.slot_addr(i))) << "missing slot " << i;
}

TEST(ObjAllocatorTest, FlushCacheWritesBitmapToPage) {
    test_page p{64};
    obj_allocator a{*p.pg()};

    // Three allocations -> bits 0,1,2 in the cache (not yet flushed).
    ASSERT_NE(a.allocate(), nullptr);
    ASSERT_NE(a.allocate(), nullptr);
    ASSERT_NE(a.allocate(), nullptr);
    EXPECT_EQ(p.pg()->load_alloc_word(0), 0u);

    a.flush_cache();
    EXPECT_EQ(p.pg()->load_alloc_word(0), 0b111ull);
}

TEST(ObjAllocatorTest, RebindToSamePagePicksUpAlreadyAllocatedSlots) {
    test_page p{64};

    {
        obj_allocator a{*p.pg()};
        for (int i = 0; i < 5; ++i) ASSERT_NE(a.allocate(), nullptr);
        a.flush_cache();
    }

    // Second allocator on the same page must skip the first 5 and hand out
    // the remaining block_cnt - 5 distinct slots before exhausting.
    obj_allocator b{*p.pg()};
    std::unordered_set<managed*> seen;
    for (int i = 0; i < p.pg()->block_cnt() - 5; ++i) {
        managed* m = b.allocate();
        ASSERT_NE(m, nullptr) << "alloc #" << i << " after rebind";
        seen.insert(m);
    }
    EXPECT_EQ(b.allocate(), nullptr);
    EXPECT_EQ((int)seen.size(), p.pg()->block_cnt() - 5);
}