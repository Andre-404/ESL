#include <gtest/gtest.h>
#include <cstring>
#include <unordered_set>
#include <vector>
#include "../arena.h"
#include "../pg-manager.h"
#include "../pg-meta.h"

using namespace gc;
using namespace gc::detail;

TEST(ArenaTest, RemoveDebtCanGoNegativeIfMoreRemovedThanAllocated) {
    arena a;
    a.remove_debt(100);
    EXPECT_EQ(a.get_debt(), -100);
}

TEST(ArenaTest, AllocOfSizeInSizeClassPlacesObjectOnAPageOfThatSize) {
    arena a;
    pg_manager m;
    for (size_t sz : config::sz_classes) {
        auto* obj = a.alloc(sz, m);
        ASSERT_NE(obj, nullptr) << "sz=" << sz;
        EXPECT_EQ(pg_from_obj(obj)->block_sz(), sz) << "sz=" << sz;
    }
}

TEST(ArenaTest, AllocOfUnknownSizeRoutesToBigPage) {
    arena a;
    pg_manager m;
    // 4000 isn't a configured size class.
    auto* obj = a.alloc(4000, m);
    ASSERT_NE(obj, nullptr);
    EXPECT_TRUE(pg_from_obj(obj)->is_large());
}

TEST(ArenaTest, ConsecutiveSmallAllocsShareAPageUntilItFills) {
    arena a;
    pg_manager m;
    auto* a1 = a.alloc(64, m);
    auto* a2 = a.alloc(64, m);
    ASSERT_NE(a1, nullptr);
    ASSERT_NE(a2, nullptr);
    EXPECT_NE(a1, a2);
    EXPECT_EQ(pg_from_obj(a1), pg_from_obj(a2)) << "both allocations should come from the same just-fetched page";
}

TEST(ArenaTest, FirstAllocLandsAtPageStartOff) {
    arena a;
    pg_manager m;
    auto* obj = a.alloc(64, m);
    ASSERT_NE(obj, nullptr);
    auto* pg = pg_from_obj(obj);
    auto pg_addr = reinterpret_cast<uintptr_t>(pg);
    EXPECT_EQ(reinterpret_cast<uintptr_t>(obj), pg_addr + pg->start_off());
}

TEST(ArenaTest, ConsecutiveSmallAllocsAreSpacedByBlockSize) {
    arena a;
    pg_manager m;
    auto* a1 = reinterpret_cast<uint8_t*>(a.alloc(63, m));
    auto* a2 = reinterpret_cast<uint8_t*>(a.alloc(63, m));
    ASSERT_NE(a1, nullptr);
    ASSERT_NE(a2, nullptr);
    EXPECT_EQ(a2 - a1, static_cast<std::ptrdiff_t>(64));
}

TEST(ArenaTest, DistinctSizeClassesUseDistinctPages) {
    arena a;
    pg_manager m;
    auto* o32 = a.alloc(32, m);
    auto* o64 = a.alloc(64, m);
    ASSERT_NE(o32, nullptr);
    ASSERT_NE(o64, nullptr);
    EXPECT_NE(pg_from_obj(o32), pg_from_obj(o64));
    EXPECT_EQ(pg_from_obj(o32)->block_sz(), 32);
    EXPECT_EQ(pg_from_obj(o64)->block_sz(), 64);
}

TEST(ArenaTest, BigAllocsAccumulateOnTheBigChain) {
    arena a;
    pg_manager m;
    auto* o1 = a.alloc(4000, m);
    auto* o2 = a.alloc(4000, m);
    auto* o3 = a.alloc(4000, m);
    ASSERT_NE(o1, nullptr);
    ASSERT_NE(o2, nullptr);
    ASSERT_NE(o3, nullptr);

    std::unordered_set<pg_meta*> seen;
    a.mutate_owned([&](pg_meta* head) {
        if (head && head->is_large()) {
            for (auto* p = head; p; p = p->next()) seen.insert(p);
        }
        return head;
    });
    EXPECT_EQ(seen.size(), 3);
    EXPECT_TRUE(seen.count(pg_from_obj(o1)));
    EXPECT_TRUE(seen.count(pg_from_obj(o2)));
    EXPECT_TRUE(seen.count(pg_from_obj(o3)));
}

TEST(ArenaTest, BigAllocChainPrependsNewestFirst) {
    arena a;
    pg_manager m;
    auto* o1 = a.alloc(4000, m);
    auto* o2 = a.alloc(4000, m);
    auto* o3 = a.alloc(4000, m);

    pg_meta* head = nullptr;
    a.mutate_owned([&](pg_meta* h) {
        if (h && h->is_large()) head = h;
        return h;
    });
    ASSERT_NE(head, nullptr);
    EXPECT_EQ(head, pg_from_obj(o3)) << "newest allocation should be at head";
    EXPECT_EQ(head->next(), pg_from_obj(o2));
    EXPECT_EQ(head->next()->next(), pg_from_obj(o1));
}

TEST(ArenaTest, MutateOwnedVisitsEverySizeClassAndBigChain) {
    arena a;
    int calls = 0;
    a.mutate_owned([&](pg_meta* head) {
        ++calls;
        return head;
    });
    EXPECT_EQ(calls, config::szclass_cnt + 1) << "mutator should be called once per size class plus once for big";
}

TEST(ArenaTest, FlushAllocCachesIsSafeOnFreshArena) {
    arena a;
    a.flush_alloc_caches();
    SUCCEED();
}

TEST(ArenaTest, FlushAllocCachesPropagatesBitsToTheActivePage) {
    arena a;
    pg_manager m;
    auto* o = a.alloc(64, m);
    a.alloc(64, m);
    a.alloc(64, m);

    EXPECT_EQ(pg_from_obj(o)->load_alloc_word(0), 0u);

    a.flush_alloc_caches();
    EXPECT_EQ(pg_from_obj(o)->load_alloc_word(0) & 0b111ull, 0b111ull);
}

TEST(ArenaTest, AllocBigReturnsPointerInsideTheAllocatedPage) {
    arena a;
    pg_manager m;
    auto* obj = a.alloc(4000, m);
    ASSERT_NE(obj, nullptr);
    auto* pg = pg_from_obj(obj);
    auto base = reinterpret_cast<uintptr_t>(pg);
    EXPECT_GE(reinterpret_cast<uintptr_t>(obj), base + pg->start_off());
    EXPECT_LT(reinterpret_cast<uintptr_t>(obj), base + config::page_sz);
}

TEST(ArenaTest, AllocAcrossManySmallSizesAccumulatesDebtCorrectly) {
    arena a;
    pg_manager m;
    int64_t expected = 0;
    for (size_t sz : { size_t{32}, size_t{64}, size_t{128}, size_t{32}, size_t{64} }) {
        a.alloc(sz, m);
        expected += sz;
    }
    EXPECT_EQ(a.get_debt(), expected);
}

TEST(ArenaTest, PartialsTest) {
    pg_manager m;
    std::vector<managed*> objs;
    pg_meta* a_start = nullptr;
    {
        arena a;
        // Over allocates into second page, we don't care about that since first page will be popped into arena B
        for (int i = 0; i < config::page_sz / 64; ++i) {
            auto ptr = a.alloc(64, m);
            objs.push_back(ptr);
            ASSERT_NE(ptr, nullptr);
        }
        a.flush_alloc_caches();
        a.mutate_owned([&](pg_meta* start) {
            if (start) a_start = start;
            m.transfer_ownership(start);
            return nullptr;
        });
    }
    arena b;
    auto ptr = b.alloc(64, m);
    pg_meta* b_start = nullptr;
    b.mutate_owned([&](pg_meta* start) {
        if (start) b_start = start;
        return start;
    });

    EXPECT_EQ(pg_from_obj(objs.back()), pg_from_obj(ptr)) << "b uses second partial page";
    EXPECT_EQ(a_start, b_start) << "b reuses pages from a";
}