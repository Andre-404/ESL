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

// The empty pool reserves a slot with fetch_add before checking the cap, so a push that turns
// out to be over the cap has to give that reservation back. It used to just free the buffer
// and leave the counter raised, which ratcheted up until every subsequent push looked over the
// limit - at which point the pool stopped pooling and every mark buffer went through malloc.
TEST(MarkBufManagerTest, OverflowingThePoolDoesNotStrandTheCounter) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    constexpr auto cap = gc::config::empty_mark_bufs_limit;

    auto bufs = std::vector<mark_buf*> {};
    for (size_t i = 0; i < cap * 2; ++i) bufs.push_back(mgr.pop_empty());
    for (auto* b : bufs) mgr.push_empty(b);

    EXPECT_EQ(mgr.pooled(), cap)
        << "after overflowing by " << cap << " the counter must still match what the pool holds";
}

TEST(MarkBufManagerTest, PoolStillPoolsAfterAnOverflowEpisode) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    constexpr auto cap = gc::config::empty_mark_bufs_limit;

    // Overflow it, then drain it completely.
    auto bufs = std::vector<mark_buf*> {};
    for (size_t i = 0; i < cap * 2; ++i) bufs.push_back(mgr.pop_empty());
    for (auto* b : bufs) mgr.push_empty(b);
    for (size_t i = 0; i < cap; ++i) ASSERT_NE(mgr.pop_empty(), nullptr);

    EXPECT_EQ(mgr.pooled(), 0u) << "draining the pool must bring the count back to zero";

    // A drained pool has to accept a full cap worth of buffers again.
    for (size_t i = 0; i < cap; ++i) mgr.push_empty(mgr.pop_empty());
    EXPECT_EQ(mgr.pooled(), 1u) << "each push/pop pair nets out";

    auto refill = std::vector<mark_buf*> {};
    for (size_t i = 0; i < cap; ++i) refill.push_back(mgr.pop_empty());
    for (auto* b : refill) mgr.push_empty(b);
    EXPECT_EQ(mgr.pooled(), cap);
}

TEST(MarkBufManagerTest, ConcurrentOverflowKeepsTheCounterWithinTheCap) {
    ensure_rpmalloc_thread_ready();
    auto mgr = mark_buf_manager {};
    constexpr auto cap = gc::config::empty_mark_bufs_limit;
    constexpr int kThreads = 8, kOps = 500;

    auto threads = std::vector<std::thread> {};
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&] {
            ensure_rpmalloc_thread_ready();
            // 8 threads holding 64 each is twice the cap, so the run spends its time pushing
            // over the limit - which is the only path that touches the reservation.
            auto held = std::vector<mark_buf*> {};
            for (int i = 0; i < kOps; ++i) {
                held.push_back(mgr.pop_empty());
                if (held.size() == 64) {
                    for (auto* b : held) mgr.push_empty(b);
                    held.clear();
                }
            }
            for (auto* b : held) mgr.push_empty(b);
        });
    }
    for (auto& th : threads) th.join();

    // Racing pushes can transiently overshoot by one per thread, which is harmless, but the
    // count must not have drifted off into the thousands.
    EXPECT_LE(mgr.pooled(), cap + kThreads);
}
