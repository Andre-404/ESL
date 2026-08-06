#include <gtest/gtest.h>

#include <memory>
#include <vector>

#include "../pg-manager.h"
#include "../pg-meta.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;

namespace {
    using raw_page = gc::test::test_page;

    std::vector<std::unique_ptr<raw_page>> make_pages(size_t n) {
        auto out = std::vector<std::unique_ptr<raw_page>> {};
        for (size_t i = 0; i < n; ++i) out.push_back(std::make_unique<raw_page>(64));
        return out;
    }

    // Chains pages into one list and hands back the head, the way an arena's size class list
    // arrives at transfer_ownership.
    pg_meta* chain(const std::vector<std::unique_ptr<raw_page>>& pages, size_t from, size_t to) {
        for (size_t i = from; i + 1 < to; ++i) pages[i]->pg()->link(pages[i + 1]->pg());
        pages[to - 1]->pg()->unlink();
        return pages[from]->pg();
    }

    size_t drain(pg_list& list) {
        size_t n = 0;
        while (list.pop()) ++n;
        return n;
    }
}

TEST(PgListTest, PopOnFreshListIsNull) {
    pg_list list;
    EXPECT_EQ(list.pop(), nullptr);
}

TEST(PgListTest, PushThenPopReturnsThePage) {
    pg_list list;
    raw_page p{64};
    list.push(p.pg());
    EXPECT_EQ(list.pop(), p.pg());
    EXPECT_EQ(list.pop(), nullptr);
}

TEST(PgListTest, PoppedPageIsUnlinked) {
    pg_list list;
    auto pages = make_pages(3);
    list.push(chain(pages, 0, 3));

    auto* first = list.pop();
    ASSERT_NE(first, nullptr);
    EXPECT_EQ(first->next(), nullptr) << "a popped page must not still point at the rest of the list";
}

TEST(PgListTest, PushOfAChainMakesEveryPageAvailable) {
    pg_list list;
    auto pages = make_pages(8);
    list.push(chain(pages, 0, 8));
    EXPECT_EQ(drain(list), 8u);
}

TEST(PgListTest, RepeatedPushesAccumulate) {
    pg_list list;
    auto pages = make_pages(6);
    list.push(chain(pages, 0, 3));
    list.push(chain(pages, 3, 6));
    EXPECT_EQ(drain(list), 6u);
}

TEST(PgListTest, MutateToEmptyMakesPopReturnNull) {
    pg_list list;
    auto pages = make_pages(4);
    list.push(chain(pages, 0, 4));

    list.mutate([](pg_meta*) { return (pg_meta*)nullptr; });
    EXPECT_EQ(list.pop(), nullptr);
}

TEST(PgListTest, MutateToNonEmptyLeavesPagesReachable) {
    pg_list list;
    auto pages = make_pages(4);
    list.push(chain(pages, 0, 4));

    // Identity mutation, which is what a prune that frees nothing amounts to.
    list.mutate([](pg_meta* head) { return head; });
    EXPECT_EQ(drain(list), 4u) << "a list that survived a mutation must still hand its pages out";
}

TEST(PgListTest, MutateOnEmptyListStaysEmpty) {
    pg_list list;
    bool called = false;
    list.mutate([&](pg_meta* head) { called = true; EXPECT_EQ(head, nullptr); return head; });
    EXPECT_TRUE(called);
    EXPECT_EQ(list.pop(), nullptr);
}

TEST(PgListTest, PagesRemainReachableAfterAMutateThatDropsSome) {
    pg_list list;
    auto pages = make_pages(4);
    list.push(chain(pages, 0, 4));

    // Keep only the head, as a prune dropping three empty pages would.
    list.mutate([](pg_meta* head) { head->unlink(); return head; });
    EXPECT_EQ(drain(list), 1u);
}
