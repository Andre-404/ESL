#include <gtest/gtest.h>
#include <atomic>
#include <thread>
#include <vector>
#include <unordered_set>
#include "../tstack.h"

using namespace gc::detail;

namespace {

    struct Item : public tnode<Item> {
        int value{0};
        Item() = default;
        explicit Item(int v) : value(v) {}
    };

}

TEST(TnodeTest, LinkAndUnlink) {
    Item a, b;
    a.link(&b);
    EXPECT_EQ(a.next(), &b);
    a.unlink();
    EXPECT_EQ(a.next(), nullptr);
}

TEST(TstackTest, EmptyPopReturnsNullptr) {
    tstack<Item> s;
    EXPECT_EQ(s.lfpop(), nullptr);
}

TEST(TstackTest, LfPushPopIsLifo) {
    tstack<Item> s;
    Item a{1}, b{2}, c{3};
    s.lfpush(&a);
    s.lfpush(&b);
    s.lfpush(&c);

    EXPECT_EQ(s.lfpop(), &c);
    EXPECT_EQ(s.lfpop(), &b);
    EXPECT_EQ(s.lfpop(), &a);
    EXPECT_EQ(s.lfpop(), nullptr);
}

TEST(TstackTest, LfPopUnlinksPoppedNode) {
    tstack<Item> s;
    Item a{1}, b{2};
    s.lfpush(&a);
    s.lfpush(&b);
    Item* top = s.lfpop();
    ASSERT_EQ(top, &b);
    EXPECT_EQ(top->next(), nullptr);
}

TEST(TstackTest, ConcurrentPushPreservesAllItems) {
    constexpr int kThreads = 8;
    constexpr int kPerThread = 1000;
    constexpr int kTotal = kThreads * kPerThread;

    std::vector<Item> items(kTotal);
    for (int i = 0; i < kTotal; ++i) items[i].value = i;

    tstack<Item> s;
    std::vector<std::thread> threads;
    threads.reserve(kThreads);
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&, t] {
            for (int i = 0; i < kPerThread; ++i) {
                s.lfpush(&items[t * kPerThread + i]);
            }
        });
    }
    for (auto& th : threads) th.join();

    std::unordered_set<int> seen;
    seen.reserve(kTotal);
    while (Item* n = s.lfpop()) {
        ASSERT_TRUE(seen.insert(n->value).second) << "duplicate pop: " << n->value;
    }
    EXPECT_EQ((int)seen.size(), kTotal);
    for (int i = 0; i < kTotal; ++i) {
        EXPECT_TRUE(seen.count(i)) << "missing item " << i;
    }
}

TEST(TstackTest, ConcurrentPushPopPreservesAllItems) {
    constexpr int kProducers = 4;
    constexpr int kConsumers = 4;
    constexpr int kPerProducer = 2000;
    constexpr int kTotal = kProducers * kPerProducer;

    std::vector<Item> items(kTotal);
    for (int i = 0; i < kTotal; ++i) items[i].value = i;

    tstack<Item> s;
    std::atomic<int> producers_done{0};
    std::vector<std::vector<int>> per_consumer(kConsumers);

    std::vector<std::thread> producers;
    for (int p = 0; p < kProducers; ++p) {
        producers.emplace_back([&, p] {
            for (int i = 0; i < kPerProducer; ++i) {
                s.lfpush(&items[p * kPerProducer + i]);
            }
            producers_done.fetch_add(1, std::memory_order_release);
        }  );
    }

    std::vector<std::thread> consumers;
    for (int c = 0; c < kConsumers; ++c) {
        consumers.emplace_back([&, c] {
            for (;;) {
                if (Item* n = s.lfpop()) {
                    per_consumer[c].push_back(n->value);
                } else if (producers_done.load(std::memory_order_acquire) == kProducers) {
                    if (Item* n2 = s.lfpop()) {
                        per_consumer[c].push_back(n2->value);
                    } else {
                        return;
                    }
                }
            }
        });
    }

    for (auto& th : producers) th.join();
    for (auto& th : consumers) th.join();

    std::unordered_set<int> seen;
    seen.reserve(kTotal);
    for (auto& v : per_consumer)
        for (int x : v)
            ASSERT_TRUE(seen.insert(x).second) << "duplicate pop: " << x;

    EXPECT_EQ((int)seen.size(), kTotal);
    EXPECT_EQ(s.lfpop(), nullptr);
}