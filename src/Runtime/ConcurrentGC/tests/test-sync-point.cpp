#include <gtest/gtest.h>
#include <atomic>
#include <chrono>
#include <thread>
#include <vector>
#include "../sync-point.h"

using namespace gc::detail;
using namespace std::chrono_literals;

namespace {
    template <class Pred>
    bool wait_until(Pred pred, std::chrono::milliseconds timeout = 2s) {
        auto deadline = std::chrono::steady_clock::now() + timeout;
        while (std::chrono::steady_clock::now() < deadline) {
            if (pred()) return true;
            std::this_thread::sleep_for(1ms);
        }
        return pred();
    }

}

TEST(SyncPointTest, ArriveAndWaitReleasesWhenAllArrive) {
    auto sp = sync_point {};
    constexpr int N = 4;
    for (int i = 0; i < N; ++i) sp.register_waiter();

    std::atomic<int> past_barrier{0};
    std::vector<std::thread> threads;
    for (int i = 0; i < N; ++i) {
        threads.emplace_back([&] {
            sp.arrive_and_wait();
            past_barrier.fetch_add(1, std::memory_order_release);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(past_barrier.load(), N);
}

TEST(SyncPointTest, ArriveAndWaitBlocksUntilLastArrival) {
    auto sp = sync_point {};
    constexpr int N = 3;
    for (int i = 0; i < N; ++i) sp.register_waiter();

    std::atomic<int> past_barrier{0};
    std::vector<std::thread> threads;
    for (int i = 0; i < N - 1; ++i) {
        threads.emplace_back([&] {
            sp.arrive_and_wait();
            past_barrier.fetch_add(1, std::memory_order_release);
        });
    }
    std::this_thread::sleep_for(50ms);
    EXPECT_EQ(past_barrier.load(), 0) << "no thread may pass before the last arrives";

    threads.emplace_back([&] {
        sp.arrive_and_wait();
        past_barrier.fetch_add(1, std::memory_order_release);
    });
    ASSERT_TRUE(wait_until([&] { return past_barrier.load() == N; })) << "barrier did not release; only " << past_barrier.load() << "/" << N << " passed";
    for (auto& th : threads) th.join();
}

TEST(SyncPointTest, BarrierIsReusableAcrossPhases) {
    auto sp = sync_point {};
    constexpr int N = 4;
    constexpr int kPhases = 5;
    for (int i = 0; i < N; ++i) sp.register_waiter();

    std::atomic<int> phase_counts[kPhases] = {};
    std::vector<std::thread> threads;
    for (int i = 0; i < N; ++i) {
        threads.emplace_back([&] {
            for (int p = 0; p < kPhases; ++p) {
                sp.arrive_and_wait();
                phase_counts[p].fetch_add(1, std::memory_order_relaxed);
            }
        });
    }
    for (auto& th : threads) th.join();
    for (int p = 0; p < kPhases; ++p) {
        EXPECT_EQ(phase_counts[p].load(), N) << "phase " << p;
    }
}

TEST(SyncPointTest, ArriveIsNonBlocking) {
    auto sp = sync_point {};
    sp.register_waiter();
    sp.register_waiter();

    std::atomic<bool> done{false};
    std::thread t([&] {
        sp.arrive();
        done.store(true, std::memory_order_release);
    });
    ASSERT_TRUE(wait_until([&] { return done.load(); })) << "arrive() must not block";
    t.join();

    sp.arrive_and_wait();
    ASSERT_TRUE(true) << "arrive_and_wait() must not block because this is the last thread";
}

TEST(SyncPointTest, ArriveCanCompletePhaseForArriveAndWaitWaiters) {
    auto sp = sync_point {};
    constexpr int N = 3;
    for (int i = 0; i < N; ++i) sp.register_waiter();

    std::atomic<int> past{0};
    std::thread t1([&]{ sp.arrive_and_wait(); past.fetch_add(1); });
    std::thread t2([&]{ sp.arrive_and_wait(); past.fetch_add(1); });
    std::this_thread::sleep_for(50ms);
    ASSERT_EQ(past.load(), 0);

    sp.arrive();
    t1.join(); t2.join();
    ASSERT_TRUE(past.load() == 2);
}

TEST(SyncPointTest, DeregisterReleasesWhenItMakesCountsMatch) {
    // expected = 3, two arrive_and_wait callers are blocked; deregistering
    // the third should drop expected to 2 and release everyone.
    auto sp = sync_point {};
    sp.register_waiter();
    sp.register_waiter();
    sp.register_waiter();

    std::atomic<int> past{0};
    std::thread t1([&]{ sp.arrive_and_wait(); past.fetch_add(1); });
    std::thread t2([&]{ sp.arrive_and_wait(); past.fetch_add(1); });

    ASSERT_TRUE(wait_until([&]{
        std::this_thread::sleep_for(20ms);
        return true;
    }));
    EXPECT_EQ(past.load(), 0);

    sp.deregister_waiter();
    t1.join(); t2.join();
    ASSERT_TRUE(past.load() == 2) << "deregister did not release blocked waiters";
}

TEST(SyncPointTest, DeregisterReducesExpectedForFuturePhases) {
    auto sp = sync_point {};
    for (int i = 0; i < 4; ++i) sp.register_waiter();

    std::atomic<int> past_phase0{0};
    std::thread w1([&]{ sp.arrive_and_wait();   past_phase0.fetch_add(1); });
    std::thread w2([&]{ sp.arrive_and_wait();   past_phase0.fetch_add(1); });
    std::thread d1([&]{ sp.deregister_waiter(); past_phase0.fetch_add(1); });
    std::thread d2([&]{ sp.deregister_waiter(); past_phase0.fetch_add(1); });
    w1.join(); w2.join(); d1.join(); d2.join();
    EXPECT_EQ(past_phase0.load(), 4);

    // Expected is now 2. Phase 1 should complete with two arrivals.
    std::atomic<int> past_phase1{0};
    std::thread t1([&]{ sp.arrive_and_wait(); past_phase1.fetch_add(1); });
    std::thread t2([&]{ sp.arrive_and_wait(); past_phase1.fetch_add(1); });
    ASSERT_TRUE(wait_until([&]{ return past_phase1.load() == 2; })) << "phase 1 did not complete with reduced expected count";
    t1.join(); t2.join();
}

TEST(SyncPointTest, DeregisterAloneCompletesPhaseWhenItMakesMathMatch) {
    auto sp = sync_point {};
    sp.register_waiter();

    std::atomic<bool> returned{false};
    std::thread t([&]{ sp.deregister_waiter(); returned.store(true); });
    ASSERT_TRUE(wait_until([&]{ return returned.load(); })) << "deregister did not return when it should have completed the phase alone";
    t.join();
}

TEST(SyncPointTest, ManyThreadsManyPhasesStress) {
    auto sp = sync_point {};
    constexpr int N = 16;
    constexpr int kPhases = 50;
    for (int i = 0; i < N; ++i) sp.register_waiter();

    std::atomic<int> total{0};
    std::vector<std::thread> threads;
    for (int i = 0; i < N; ++i) {
        threads.emplace_back([&] {
            for (int p = 0; p < kPhases; ++p) {
                sp.arrive_and_wait();
                total.fetch_add(1, std::memory_order_relaxed);
            }
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(total.load(), N * kPhases);
}
