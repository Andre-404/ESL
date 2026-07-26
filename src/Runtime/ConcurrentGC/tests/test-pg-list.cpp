#include <gtest/gtest.h>

#include <atomic>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <new>
#include <chrono>
#include <semaphore>
#include <thread>
#include <unordered_set>
#include <vector>

#include "../pg-manager.h"
#include "../pg-meta.h"

using namespace gc;
using namespace gc::detail;

namespace {
    // pg_list only ever touches a page's list link, so a bare pg_meta in page aligned storage
    // is all these need - no page allocator, and no 1 TiB reservation per page.
    class raw_page {
    public:
        explicit raw_page(size_t block_sz = 64) {
            _mem = std::aligned_alloc(config::page_sz, config::page_sz);
            std::memset(_mem, 0, config::page_sz);
            _pg = new (_mem) pg_meta(block_sz);
        }
        ~raw_page() {
            if (_pg) _pg->~pg_meta();
            std::free(_mem);
        }
        raw_page(const raw_page&) = delete;
        raw_page& operator=(const raw_page&) = delete;

        pg_meta* pg() const { return _pg; }
    private:
        void*    _mem = nullptr;
        pg_meta* _pg  = nullptr;
    };

    std::vector<std::unique_ptr<raw_page>> make_pages(size_t n) {
        auto out = std::vector<std::unique_ptr<raw_page>> {};
        for (size_t i = 0; i < n; ++i) out.push_back(std::make_unique<raw_page>());
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
    raw_page p;
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

// The fast path hint used to be a count that push raised *before* taking the lock, while
// mutate zeroed it and recounted the list under the lock. A push that reached its increment
// and then blocked on the mutex had that increment erased by the recount, so its page sat in
// the list behind a hint reading "empty" - unreachable until some later mutate happened to
// count it again.
//
// This drives that interleaving directly: the mutator is parked inside mutate holding the
// lock, the pusher is parked on the mutex just past the point where it would have bumped the
// count, and only then is the mutator let go to recount an empty list.
TEST(PgListTest, PushDuringAnInFlightMutateIsNotLost) {
    pg_list list;
    raw_page p;

    auto mutator_holds_lock = std::binary_semaphore { 0 };
    auto release_mutator    = std::binary_semaphore { 0 };

    auto mutator = std::thread { [&] {
        list.mutate([&](pg_meta* head) {
            mutator_holds_lock.release();
            release_mutator.acquire();
            return head;      // identity, as a prune that frees nothing would be
        });
    } };

    mutator_holds_lock.acquire();   // mutate is now inside the lock, over an empty list

    auto pusher = std::thread { [&] { list.push(p.pg()); } };
    // Long enough for the pusher to be blocked on the mutex, which is past the point where
    // the old implementation had already raised its count.
    std::this_thread::sleep_for(std::chrono::milliseconds(50));

    release_mutator.release();
    mutator.join();
    pusher.join();

    EXPECT_EQ(list.pop(), p.pg())
        << "a page pushed while a mutate was in flight must still be reachable through pop";
}

// The same race left to chance across many pushes. Weaker than the test above - a mutate that
// lands after the last push repairs the count - but it covers interleavings the staged version
// cannot reach. The preloaded list is long on purpose: the old recount was O(list), so it kept
// the mutator holding the lock exactly when a pusher wanted it.
TEST(PgListTest, ConcurrentPushAndMutateStrandNoPages) {
    constexpr size_t kPreloaded = 512;
    constexpr size_t kPushers = 4;
    constexpr size_t kPerPusher = 64;

    pg_list list;
    auto preloaded = make_pages(kPreloaded);
    list.push(chain(preloaded, 0, kPreloaded));

    auto pushed = std::vector<std::vector<std::unique_ptr<raw_page>>> {};
    for (size_t t = 0; t < kPushers; ++t) pushed.push_back(make_pages(kPerPusher));

    auto stop = std::atomic<bool> { false };
    auto mutator = std::thread { [&] {
        while (!stop.load(std::memory_order_relaxed))
            list.mutate([](pg_meta* head) { return head; });
    } };

    auto threads = std::vector<std::thread> {};
    for (size_t t = 0; t < kPushers; ++t) {
        threads.emplace_back([&, t] {
            for (size_t i = 0; i < kPerPusher; ++i) list.push(pushed[t][i]->pg());
        });
    }
    for (auto& th : threads) th.join();
    stop.store(true, std::memory_order_relaxed);
    mutator.join();

    // Drain with pop alone: another mutate here would recount and paper over the loss.
    EXPECT_EQ(drain(list), kPreloaded + kPushers * kPerPusher)
        << "every pushed page must still be reachable through pop";
}
