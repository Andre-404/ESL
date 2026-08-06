#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <new>
#include <unordered_set>
#include "../szclass-allocator.h"
#include "../pg-meta.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;

using gc::test::test_page;

TEST(SzclassAllocatorTest, DefaultConstructedReturnsNullptr) {
    szclass_allocator a;
    EXPECT_EQ(a.alloc(), nullptr);
}

TEST(SzclassAllocatorTest, AllocsFromSinglePushedPageUntilExhaustion) {
    for (size_t sz : config::sz_classes) {
        test_page p{sz};
        szclass_allocator a;
        a.push_pg(p.pg());

        std::unordered_set<managed*> seen;
        for (int i = 0; i < p.pg()->block_cnt(); ++i) {
            managed* m = a.alloc();
            ASSERT_NE(m, nullptr) << "alloc #" << i;
            ASSERT_TRUE(seen.insert(m).second);
        }
        EXPECT_EQ(a.alloc(), nullptr) << "page should be exhausted";
        EXPECT_EQ((int)seen.size(), p.pg()->block_cnt());
    }
}

TEST(SzclassAllocatorTest, AllAllocationsBelongToTheirOriginatingPage) {
    test_page p{64};
    szclass_allocator a;
    a.push_pg(p.pg());

    while (auto* m = a.alloc()) {
        EXPECT_EQ(pg_meta::head_from_ptr(m), p.pg());
    }
}

TEST(SzclassAllocatorTest, PushPgSwitchesActivePageToNewest) {
    test_page a_pg{64};
    test_page b_pg{64};

    szclass_allocator s;
    s.push_pg(a_pg.pg());
    managed* in_a = s.alloc();
    ASSERT_NE(in_a, nullptr);
    EXPECT_EQ(pg_meta::head_from_ptr(in_a), a_pg.pg());

    s.push_pg(b_pg.pg());
    managed* in_b = s.alloc();
    ASSERT_NE(in_b, nullptr);
    EXPECT_EQ(pg_meta::head_from_ptr(in_b), b_pg.pg());
}

TEST(SzclassAllocatorTest, FlushAllocCacheIsSafeOnEmpty) {
    szclass_allocator s;
    s.flush_alloc_cache();
    SUCCEED();
}

TEST(SzclassAllocatorTest, FlushAllocCachePropagatesToCurrentPage) {
    test_page p{64};
    szclass_allocator s;
    s.push_pg(p.pg());

    ASSERT_NE(s.alloc(), nullptr);
    ASSERT_NE(s.alloc(), nullptr);
    s.flush_alloc_cache();
    EXPECT_EQ(p.pg()->load_alloc_word(0), 0b11ull);
}

TEST(SzclassAllocatorTest, MutateReplacesStartChain) {
    test_page a_pg{64};
    test_page b_pg{64};

    szclass_allocator s;
    s.push_pg(a_pg.pg());
    s.mutate([&](pg_meta* ) -> pg_meta* { return b_pg.pg(); });

    managed* m = s.alloc();
    ASSERT_NE(m, nullptr);
    EXPECT_EQ(pg_meta::head_from_ptr(m), b_pg.pg());
}

TEST(SzclassAllocatorTest, MutateToNullptrRemovesAllocator) {
    test_page p{64};
    szclass_allocator s;
    s.push_pg(p.pg());

    s.mutate([](pg_meta*) -> pg_meta* { return nullptr; });

    auto* m = s.alloc();
    ASSERT_EQ(m, nullptr);
}

TEST(SzclassAllocatorTest, MutateReceivesCurrentStart) {
    test_page p{64};
    szclass_allocator s;
    s.push_pg(p.pg());

    pg_meta* seen = nullptr;
    s.mutate([&](pg_meta* start) -> pg_meta* { seen = start; return start; });
    EXPECT_EQ(seen, p.pg());
}

namespace {
    managed* alloc_and_mark(szclass_allocator& s) {
        managed* m = s.alloc();
        if (m) pg_meta::head_from_ptr(m)->record_mark(m, false);
        return m;
    }
}

TEST(SzclassAllocatorTest, FullPageIsSkippedToNextPage) {
    test_page a_pg{64};
    test_page b_pg{64};

    szclass_allocator s;
    s.push_pg(a_pg.pg());   // active page = A
    s.push_pg(b_pg.pg());   // active page = B; A linked before B in the chain

    s.mutate([&](pg_meta*) -> pg_meta* { return a_pg.pg(); });

    for (int i = 0; i < a_pg.pg()->block_cnt(); ++i) {
        managed* m = s.alloc();
        ASSERT_NE(m, nullptr) << "alloc #" << i;
        EXPECT_EQ(pg_meta::head_from_ptr(m), a_pg.pg());
        a_pg.pg()->record_mark(m, false);
    }
    ASSERT_EQ(a_pg.pg()->compute_live(), a_pg.pg()->block_cnt());

    managed* in_b = s.alloc();
    ASSERT_NE(in_b, nullptr);
    EXPECT_EQ(pg_meta::head_from_ptr(in_b), b_pg.pg());
}

TEST(SzclassAllocatorTest, AllocatesAcrossTwoPagesEndToEnd) {
    test_page a_pg{64};
    test_page b_pg{64};

    szclass_allocator s;
    s.push_pg(a_pg.pg());

    int a_count = 0;
    while (managed* m = alloc_and_mark(s)) {
        EXPECT_EQ(pg_meta::head_from_ptr(m), a_pg.pg());
        ++a_count;
    }
    EXPECT_EQ(a_count, a_pg.pg()->block_cnt());

    s.push_pg(b_pg.pg());
    int b_count = 0;
    while (managed* m = alloc_and_mark(s)) {
        EXPECT_EQ(pg_meta::head_from_ptr(m), b_pg.pg());
        ++b_count;
    }
    EXPECT_EQ(b_count, b_pg.pg()->block_cnt());

    EXPECT_EQ(s.alloc(), nullptr);
}
