#include <gtest/gtest.h>
#include <atomic>
#include <chrono>
#include <thread>
#include <vector>
#include "../transition-manager.h"
#include "../TCB.h"

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
    std::vector<std::unique_ptr<tcb>> make_tcbs(int n) {
        std::vector<std::unique_ptr<tcb>> v;
        v.reserve(n);
        for (int i = 0; i < n; ++i) v.emplace_back(std::make_unique<tcb>(nullptr, 0));
        return v;
    }

    std::vector<tcb*> as_ptrs(std::vector<std::unique_ptr<tcb>>& owners) {
        std::vector<tcb*> v;
        v.reserve(owners.size());
        for (auto& up : owners) v.push_back(up.get());
        return v;
    }

}

TEST(PostManagerTest, PostOnRunningThreadsSetsOpcodeAndPending) {
    auto owners = make_tcbs(4);
    auto ptrs   = as_ptrs(owners);

    post_manager pm;
    auto vec = pm.post(ptrs, 42);

    EXPECT_TRUE(vec.empty()) << "running threads aren't immediately handshaked";
    for (auto* t : ptrs) {
        EXPECT_EQ(t->load_state(), thd_state::has_pending);
        EXPECT_EQ(t->get_opcode(), 42u);
    }
}

TEST(PostManagerTest, PostOnBlockedThreadsReturnsThemAsHandshaking) {
    auto owners = make_tcbs(4);
    auto ptrs   = as_ptrs(owners);
    for (auto* t : ptrs) t->transition(thd_state::blocked);

    post_manager pm;
    auto vec = pm.post(ptrs, /*opcode=*/7);

    EXPECT_EQ(vec.size(), ptrs.size());
    for (auto* t : ptrs) EXPECT_EQ(t->load_state(), thd_state::handshaking);
}

TEST(PostManagerTest, PostOnDeadThreadsAcksImmediately) {
    auto owners = make_tcbs(3);
    auto ptrs   = as_ptrs(owners);
    for (auto* t : ptrs) t->transition(thd_state::dead);

    post_manager pm;
    auto vec = pm.post(ptrs, 1);

    EXPECT_TRUE(vec.empty());
    auto returned = std::atomic<bool> { false };
    std::thread t([&]{ pm.wait_on_all_ack(); returned.store(true); });
    EXPECT_TRUE(wait_until([&]{ return returned.load(); }));
    t.join();
}

TEST(PostManagerTest, PostMixedStates) {
    auto owners = make_tcbs(6);
    auto ptrs   = as_ptrs(owners);
    // 0,1 running ; 2,3 blocked ; 4,5 dead
    ptrs[2]->transition(thd_state::blocked);
    ptrs[3]->transition(thd_state::blocked);
    ptrs[4]->transition(thd_state::dead);
    ptrs[5]->transition(thd_state::dead);

    post_manager pm;
    auto handshaked = pm.post(ptrs, /*opcode=*/9);

    EXPECT_EQ(handshaked.size(), 2u);
    EXPECT_EQ(ptrs[0]->load_state(), thd_state::has_pending);
    EXPECT_EQ(ptrs[1]->load_state(), thd_state::has_pending);
    EXPECT_EQ(ptrs[2]->load_state(), thd_state::handshaking);
    EXPECT_EQ(ptrs[3]->load_state(), thd_state::handshaking);
    EXPECT_EQ(ptrs[4]->load_state(), thd_state::dead);
    EXPECT_EQ(ptrs[5]->load_state(), thd_state::dead);
}


TEST(PostManagerTest, CompleteHandshakeTransitionsAndAcksHandshakedThreads) {
    auto owners = make_tcbs(3);
    auto ptrs   = as_ptrs(owners);
    for (auto* t : ptrs) t->transition(thd_state::blocked);

    post_manager pm;
    auto handshaked = pm.post(ptrs, 1);
    ASSERT_EQ(handshaked.size(), 3u);

    pm.complete_handshake(handshaked);
    for (auto* t : ptrs) EXPECT_EQ(t->load_state(), thd_state::blocked);

    // All three were the only threads in flight - wait_on_all_ack must return.
    std::atomic<bool> returned{false};
    std::thread t([&]{ pm.wait_on_all_ack(); returned.store(true); });
    EXPECT_TRUE(wait_until([&]{ return returned.load(); }));
    t.join();
}

TEST(PostManagerTest, ExecutePendingRunsLambdaResetsOpcodeAndStateRunning) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();

    post_manager pm;
    pm.post(std::vector<tcb*>{t}, 77);
    ASSERT_EQ(t->load_state(), thd_state::has_pending);
    ASSERT_EQ(t->get_opcode(), 77u);

    uint8_t observed_opcode = 0;
    pm.execute_pending(t, [&](tcb* who) {
        EXPECT_EQ(who, t);
        observed_opcode = who->get_opcode();
    });

    EXPECT_EQ(observed_opcode, 77u);
    EXPECT_EQ(t->get_opcode(), 0u);
    EXPECT_EQ(t->load_state(), thd_state::running);
}

TEST(PostManagerTest, EnterBlockedFromRunningGoesStraightToBlocked) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    post_manager pm;
    bool exec_called = false;

    pm.enter_blocked(t, [&](tcb*) { exec_called = true; });
    EXPECT_EQ(t->load_state(), thd_state::blocked);
    EXPECT_FALSE(exec_called) << "running -> blocked doesn't need to run the pending task";
}

TEST(PostManagerTest, EnterBlockedFromHasPendingRunsTaskThenBlocks) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    post_manager pm;
    pm.post(std::vector<tcb*>{t}, 3);
    ASSERT_EQ(t->load_state(), thd_state::has_pending);

    int exec_called = 0;
    pm.enter_blocked(t, [&](tcb*) { ++exec_called; });

    EXPECT_EQ(exec_called, 1);
    EXPECT_EQ(t->load_state(), thd_state::blocked);
    EXPECT_EQ(t->get_opcode(), 0u);
}


TEST(PostManagerTest, ExitBlockedFromBlockedGoesToRunning) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    t->transition(thd_state::blocked);

    post_manager pm;
    pm.exit_blocked(t);
    EXPECT_EQ(t->load_state(), thd_state::running);
}

TEST(PostManagerTest, ExitBlockedWaitsThroughHandshakingThenResumes) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    t->transition(thd_state::handshaking);

    post_manager pm;
    std::atomic<bool> returned{false};
    std::thread driver([&] {
        pm.exit_blocked(t);
        returned.store(true);
    });

    std::this_thread::sleep_for(30ms);
    ASSERT_FALSE(returned.load());

    t->transition(thd_state::blocked);

    ASSERT_TRUE(wait_until([&]{ return returned.load(); }));
    driver.join();
    EXPECT_EQ(t->load_state(), thd_state::running);
}

TEST(PostManagerTest, ThreadExitFromRunningGoesToDead) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    post_manager pm;
    pm.thread_exit(t, [](tcb*){ FAIL() << "no pending task to run"; });
    EXPECT_EQ(t->load_state(), thd_state::dead);
}

TEST(PostManagerTest, ThreadExitFromHasPendingRunsTaskThenDies) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    post_manager pm;
    pm.post(std::vector<tcb*>{t}, 5);

    int exec_called = 0;
    pm.thread_exit(t, [&](tcb*) { ++exec_called; });
    EXPECT_EQ(exec_called, 1);
    EXPECT_EQ(t->load_state(), thd_state::dead);
}

TEST(PostManagerTest, PrologueWaitsUntilStateLeavesNeedStart) {
    auto owners = make_tcbs(1);
    auto* t = owners[0].get();
    t->transition(thd_state::need_start);

    post_manager pm;
    std::atomic<bool> returned{false};
    std::thread waiter([&]{ pm.prologue(t); returned.store(true); });

    std::this_thread::sleep_for(30ms);
    ASSERT_FALSE(returned.load());

    t->transition(thd_state::running);
    ASSERT_TRUE(wait_until([&]{ return returned.load(); }));
    waiter.join();
}

TEST(PostManagerTest, FinishStwMovesHandshakingToBlockedAndNeedStartToRunning) {
    auto owners = make_tcbs(4);
    auto ptrs   = as_ptrs(owners);

    ptrs[0]->transition(thd_state::handshaking);
    ptrs[1]->transition(thd_state::handshaking);
    ptrs[2]->transition(thd_state::need_start);
    // ptrs[3] stays running - finish_stw asserts state is one of
    // {handshaking, running, need_start}, so it's allowed.

    post_manager pm;
    pm.finish_stw(ptrs);

    EXPECT_EQ(ptrs[0]->load_state(), thd_state::blocked);
    EXPECT_EQ(ptrs[1]->load_state(), thd_state::blocked);
    EXPECT_EQ(ptrs[2]->load_state(), thd_state::running);
    EXPECT_EQ(ptrs[3]->load_state(), thd_state::running);
}


TEST(PostManagerTest, WaitOnAllAckReturnsImmediatelyWhenNothingPosted) {
    post_manager pm;
    std::atomic<bool> returned{false};
    std::thread t([&]{ pm.wait_on_all_ack(); returned.store(true); });
    EXPECT_TRUE(wait_until([&]{ return returned.load(); }));
    t.join();
}

TEST(PostManagerTest, AckEventuallyReleasesWaiter) {
    auto owners = make_tcbs(3);
    auto ptrs   = as_ptrs(owners);
    for (auto* t : ptrs) t->transition(thd_state::blocked);

    post_manager pm;
    auto handshaked = pm.post(ptrs, 1);
    ASSERT_EQ(handshaked.size(), 3u);

    std::atomic<bool> returned{false};
    std::thread waiter([&]{ pm.wait_on_all_ack(); returned.store(true); });

    std::this_thread::sleep_for(20ms);
    EXPECT_FALSE(returned.load());
    pm.ack();
    std::this_thread::sleep_for(10ms);
    EXPECT_FALSE(returned.load());
    pm.ack();
    std::this_thread::sleep_for(10ms);
    EXPECT_FALSE(returned.load());
    pm.ack();

    EXPECT_TRUE(wait_until([&]{ return returned.load(); }));
    waiter.join();
}

TEST(PostManagerTest, NeedsSafepointIsTrueOnlyForHasPending) {
    tcb t{nullptr, 0};
    EXPECT_FALSE(needs_safepoint(&t));
    t.transition(thd_state::has_pending);
    EXPECT_TRUE(needs_safepoint(&t));
    t.transition(thd_state::blocked);
    EXPECT_FALSE(needs_safepoint(&t));
}