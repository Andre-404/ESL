#include <gtest/gtest.h>
#include <atomic>
#include <thread>
#include <vector>
#include <unordered_set>
#include "../TCB-registry.h"
#include "../TCB.h"

using namespace gc::detail;

TEST(TcbRegistryTest, AddInsertsAndUnderLockCallbackFires) {
    tcb_registry reg;
    tcb t{nullptr, 0};

    bool callback_ran = false;
    reg.add(&t, [&] { callback_ran = true; });
    EXPECT_TRUE(callback_ran);

    reg.with_snapshot([&](auto& set) {
        EXPECT_EQ(set.size(), 1u);
        EXPECT_TRUE(set.count(&t));
    });
}

TEST(TcbRegistryTest, RemoveErases) {
    tcb_registry reg;
    tcb t{nullptr, 0};
    reg.add(&t, []{});
    reg.remove(&t);

    reg.with_snapshot([&](auto& set) {
        EXPECT_EQ(set.size(), 0u);
        EXPECT_FALSE(set.count(&t));
    });
}

TEST(TcbRegistryTest, WithSnapshotSeesAllRegisteredTcbs) {
    tcb_registry reg;
    std::vector<tcb*> tcbs;
    for (int i = 0; i < 16; ++i) tcbs.push_back(new tcb { nullptr, 0 });
    for (auto& t : tcbs) reg.add(t, []{});

    reg.with_snapshot([&](auto& set) {
        EXPECT_EQ(set.size(), tcbs.size());
        for (auto& t : tcbs) {
            EXPECT_TRUE(set.contains(t)) << "missing tcb " << &t;
            delete t;
        }
    });
}

TEST(TcbRegistryTest, ConcurrentAddRemoveDoesNotCorruptRegistry) {
    tcb_registry reg;
    constexpr int kThreads = 8;
    constexpr int kPerThread = 200;

    std::vector<std::vector<tcb*>> per_thread(kThreads);
    for (auto& v : per_thread) {
        for (int i = 0; i < kPerThread; ++i) v.push_back(new tcb { nullptr, 0 });
    }

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&, t] {
            for (auto& tt : per_thread[t]) reg.add(tt, []{});
            for (auto& tt : per_thread[t]) {
                reg.remove(tt);
                delete tt;
            }
        });
    }
    for (auto& th : threads) th.join();

    reg.with_snapshot([&](auto& set) {
        EXPECT_EQ(set.size(), 0u);
    });
}