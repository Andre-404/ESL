#include <gtest/gtest.h>
#include <array>
#include <vector>
#include <thread>
#include <atomic>
#include <unordered_set>
#include "../gc-config.h"
#include "../mark-buf.h"
#include "../managed.h"
#include "rpmalloc/rpmalloc.h"

using namespace gc;
using namespace gc::detail;

namespace {

    managed* make_managed(uint8_t id = 0) {
        return new managed(id, move_state::none);
    }

    class RpmallocEnvironment : public testing::Environment {
    public:
        void SetUp() override {
            rpmalloc_initialize();
            rpmalloc_thread_initialize();
        }
        void TearDown() override {
            rpmalloc_thread_finalize(1);
            rpmalloc_finalize();
        }
    };

    testing::Environment* const rpmalloc_env = testing::AddGlobalTestEnvironment(new RpmallocEnvironment());

    struct RpmallocThreadInitializer {
        RpmallocThreadInitializer() {
            rpmalloc_thread_initialize();
        }
        ~RpmallocThreadInitializer() {
            rpmalloc_thread_finalize(1);
        }
    };

    inline void ensure_rpmalloc_thread_ready() {
        thread_local RpmallocThreadInitializer thread_init;
    }

}

TEST(MarkBufTest, NewBufIsEmpty) {
    auto buf = mark_buf {};
    EXPECT_TRUE(buf.empty());
    EXPECT_FALSE(buf.full());
    EXPECT_EQ(buf.pop(), nullptr);
}

TEST(MarkBufTest, PushReturnsFalseUntilFull) {
    auto buf = mark_buf {};
    auto owned = std::vector<std::unique_ptr<managed>> {};
    for (int i = 0; i < 127; ++i) {
        owned.emplace_back(make_managed((uint8_t)i));
        EXPECT_FALSE(buf.push(owned.back().get())) << "push #" << i;
    }
    owned.emplace_back(make_managed(127));
    EXPECT_TRUE(buf.push(owned.back().get())) << "128th push should signal full";
    EXPECT_TRUE(buf.full());
    EXPECT_FALSE(buf.empty());
}

TEST(MarkBufTest, PopFromEmptyReturnsNullptrAndDoesNotUnderflow) {
    auto buf = mark_buf {};
    EXPECT_EQ(buf.pop(), nullptr);
    EXPECT_EQ(buf.pop(), nullptr); // still safe
    EXPECT_TRUE(buf.empty());
}

TEST(MarkBufTest, FillThenDrainRestoresEmpty) {
    auto buf = mark_buf {};
    auto items = std::array<managed*, 128> {};
    for (auto& p : items) p = make_managed();
    for (auto* p : items) buf.push(p);
    ASSERT_TRUE(buf.full());

    auto popped = 0;
    while (buf.pop()) ++popped;
    EXPECT_EQ(popped, 128);
    EXPECT_TRUE(buf.empty());
    EXPECT_FALSE(buf.full());

    for (auto* p : items) delete p;
}

TEST(MarkBufManagerTest, FullPoolPopOnEmptyReturnsNullptr) {
    auto mgr = mark_buf_manager { };
    EXPECT_EQ(mgr.pop_full(), nullptr);
}

TEST(MarkBufManagerTest, FullPoolPushPopRoundTrip) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    auto a = mgr.pop_empty();
    auto b = mgr.pop_empty();
    ASSERT_NE(a, nullptr);
    ASSERT_NE(b, nullptr);

    mgr.push_full(a);
    mgr.push_full(b);

    EXPECT_EQ(mgr.pop_full(), b);
    EXPECT_EQ(mgr.pop_full(), a);
    EXPECT_EQ(mgr.pop_full(), nullptr);

    mgr.push_empty(a);
    mgr.push_empty(b);
}


TEST(MarkBufManagerTest, EmptyPoolAllocatesWhenStarvedAndRecycles) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    auto a = mgr.pop_empty();
    ASSERT_NE(a, nullptr);
    EXPECT_TRUE(a->empty());

    mgr.push_empty(a);
    auto b = mgr.pop_empty();
    EXPECT_EQ(a, b) << "manager should recycle returned buffers";

    mgr.push_empty(b);
}

TEST(MarkBufManagerTest, EmptyPoolReturnsManyDistinctBuffersWhenDrained) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    constexpr int N = 32;
    auto bufs = std::array<mark_buf*, N> {};
    for (auto& p : bufs) {
        p = mgr.pop_empty();
        ASSERT_NE(p, nullptr);
        EXPECT_TRUE(p->empty());
    }
    auto uniq = std::unordered_set<mark_buf*> { bufs.begin(), bufs.end() };
    EXPECT_EQ((int)uniq.size(), N);

    for (auto* p : bufs) mgr.push_empty(p);
}

TEST(MarkBufManagerTest, ConcurrentEmptyPoolPushPopNoLossNoDuplicate) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    constexpr int kBufs = 128;
    auto all = std::vector<mark_buf*> {};
    all.reserve(kBufs);
    for (int i = 0; i < kBufs; ++i) all.push_back(mgr.pop_empty());
    for (auto* p : all) mgr.push_empty(p);

    constexpr int kThreads = 8;
    constexpr int kOpsPerThread = 2000;
    auto total_popped = std::atomic<int> { 0 };

    auto threads = std::vector<std::thread> {};
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&] {
            ensure_rpmalloc_thread_ready();
            for (int i = 0; i < kOpsPerThread; ++i) {
                if (mark_buf* b = mgr.pop_empty()) {
                    total_popped.fetch_add(1, std::memory_order_relaxed);
                    mgr.push_empty(b);
                }
            }
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(total_popped.load(), kThreads * kOpsPerThread);
}