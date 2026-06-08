#include <gtest/gtest.h>
#include <atomic>
#include <chrono>
#include <thread>
#include <unordered_set>
#include <vector>
#include "../collector.h"
#include "../TCB.h"
#include "../managed.h"
#include "../pg-meta.h"
#include "rpmalloc/rpmalloc.h"

using namespace gc;
using namespace gc::detail;
using namespace std::chrono_literals;

namespace {

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

    class CollectorTest : public ::testing::Test {
    protected:
        collector*  gc          = nullptr;
        uint8_t*    gc_flag     = nullptr;

        void SetUp() override {
            gc_flag = new uint8_t(0);
            gc      = new collector(*gc_flag);
            // Give the worker thread a moment to enter its loop.
            std::this_thread::sleep_for(20ms);
            ensure_rpmalloc_thread_ready();
        }

        void TearDown() override {
            // INTENTIONALLY LEAK: ~collector would terminate the program by
            // destroying a joinable thread. The worker is parked at
            // _collection_req.wait(none) and harmless.
        }

        tcb* make_tcb() {
            auto t =  gc->create_tcb(nullptr, 0);
            gc->thd_prologue(t);
            return t;
        }
    };

}

TEST_F(CollectorTest, WbActiveFalseWhenNoCollectionRunning) {
    EXPECT_FALSE(gc->wb_active()) << "gc_flag is none(0); write barrier should be off";
}

TEST_F(CollectorTest, RegisterManyRoots) {
    std::vector<size_t> roots(100, 0);
    for (auto& r : roots) gc->register_root(&r);
    SUCCEED();
}

TEST_F(CollectorTest, ThdPrologueOnFreshTcbDoesNotBlock) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocReturnsNonNullForEachSizeClass) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        for (size_t sz : config::sz_classes) {
            auto* obj = gc->alloc(sz, t);
            EXPECT_NE(obj, nullptr) << "alloc(sz=" << sz << ") failed";
        }
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocOfDifferentSizesUsesDifferentPages) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        auto* o32  = gc->alloc(32, t);
        auto* o64  = gc->alloc(64, t);
        auto* o128 = gc->alloc(128, t);
        ASSERT_NE(o32,  nullptr);
        ASSERT_NE(o64,  nullptr);
        ASSERT_NE(o128, nullptr);
        EXPECT_NE(pg_from_obj(o32),  pg_from_obj(o64));
        EXPECT_NE(pg_from_obj(o64),  pg_from_obj(o128));
        EXPECT_NE(pg_from_obj(o32),  pg_from_obj(o128));
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocLargeSizeRoutesToBigPage) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        auto* obj = gc->alloc(4000, t);
        ASSERT_NE(obj, nullptr);
        EXPECT_TRUE(is_large_pg(pg_from_obj(obj)));
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocOfSizeZeroDoesNotCrash) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        auto* obj = gc->alloc(0, t);
        EXPECT_NE(obj, nullptr);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, ManyAllocsExerciseNewPageFetches) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        std::vector<managed*> objs;
        for (int i = 0; i < config::page_sz * 2 / 64; ++i) {
            auto* o = gc->alloc(64, t);
            ASSERT_NE(o, nullptr) << "alloc #" << i;
            objs.push_back(o);
        }
        std::unordered_set<pg_meta*> pages;
        for (auto* o : objs) pages.insert(pg_from_obj(o));
        EXPECT_GE(pages.size(), 3u);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocReturnsObjectsAtPageSlots) {
    // Every returned pointer should land at start_off + k*block_sz for
    // some non-negative k within the page.
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        for (int i = 0; i < 50; ++i) {
            auto* o = gc->alloc(64, t);
            ASSERT_NE(o, nullptr);
            auto* pg = pg_from_obj(o);
            auto base   = reinterpret_cast<uintptr_t>(pg);
            auto offset = reinterpret_cast<uintptr_t>(o) - base;
            EXPECT_GE(offset, pg->start_off());
            EXPECT_LT(offset, config::page_sz);
            EXPECT_EQ((offset - pg->start_off()) % pg->block_sz(), 0u);
        }
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, TCBDeathReusePages) {
    auto* t = make_tcb();
    managed* o = nullptr;
    managed* o2 = nullptr;
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        o = gc->alloc(64, t);
        gc->delete_tcb(t);
    }};
    thd.join();
    t = make_tcb();
    thd = std::thread { [&]() {
        gc->thd_prologue(t);
        o2 = gc->alloc(64, t);
        gc->delete_tcb(t);
    }};
    thd.join();

    EXPECT_EQ(pg_from_obj(o), pg_from_obj(o2));
}

TEST_F(CollectorTest, SetPausedAndResumed) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        gc->alloc(64, t);
        gc->set_paused(t);
        gc->set_resumed(t);
        auto* obj_after_resume = gc->alloc(64, t);
        EXPECT_NE(obj_after_resume, nullptr);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, RepeatedPauseResumeCycles) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        for (int i = 0; i < 20; ++i) {
            gc->alloc(64, t);
            gc->set_paused(t);
            gc->set_resumed(t);
        }
        SUCCEED();
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, ConcurrentLightAllocsManyThreads) {
    constexpr int kThreads = 8;
    constexpr int kAllocs  = 30;
    std::atomic<int> success{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, tcb]{
            gc->thd_prologue(tcb);
            for (int i = 0; i < kAllocs; ++i) {
                if (gc->alloc(64, tcb)) success.fetch_add(1, std::memory_order_relaxed);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(success.load(), kThreads * kAllocs);
}

TEST_F(CollectorTest, ConcurrentLightAllocsProduceDistinctPointers) {
    constexpr int kThreads = 4;
    constexpr int kAllocs  = 20;

    std::vector<std::vector<managed*>> per_thread(kThreads);
    std::vector<std::thread> threads;

    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, t, tcb]{
            gc->thd_prologue(tcb);
            per_thread[t].reserve(kAllocs);
            for (int i = 0; i < kAllocs; ++i) {
                auto* o = gc->alloc(64, tcb);
                ASSERT_NE(o, nullptr);
                per_thread[t].push_back(o);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();

    std::unordered_set<managed*> all;
    for (auto& v : per_thread)
        for (auto* p : v)
            EXPECT_TRUE(all.insert(p).second) << "duplicate pointer across threads: " << p;
    EXPECT_EQ(all.size(), kThreads * kAllocs);
}

TEST_F(CollectorTest, ConcurrentTcbCreationAndDeletion) {
    constexpr int kThreads = 8;
    constexpr int kCycles  = 50;

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&]{
            ensure_rpmalloc_thread_ready();
            std::vector<std::thread> new_thds;
            for (int i = 0; i < kCycles; ++i) {
                auto* tcb = make_tcb();
                new_thds.emplace_back([&, tcb]() {
                    gc->thd_prologue(tcb);
                    gc->alloc(64, tcb);
                    gc->delete_tcb(tcb);
                });
            }
            for (auto& thd : new_thds) thd.join();
        });
    }
    for (auto& th : threads) th.join();
    SUCCEED();
}

TEST_F(CollectorTest, ConcurrentMixedSizeClassesLightLoad) {
    // Each thread sticks to one size class, allocating fewer items than
    // a page holds. Per-size-class allocator isolation under contention.
    constexpr int kThreads = 10;
    constexpr int kAllocs  = 200;
    std::atomic<int> success{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, t, tcb]{
            gc->thd_prologue(tcb);
            size_t sz = config::sz_classes[t % config::szclass_cnt];
            for (int i = 0; i < kAllocs; ++i) {
                if (gc->alloc(sz, tcb)) success.fetch_add(1, std::memory_order_relaxed);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(success.load(), kThreads * kAllocs);
}

TEST_F(CollectorTest, ManyShortLivedThreadsInWavesLightLoad) {
    constexpr int kWaves   = 4;
    constexpr int kPerWave = 16;

    for (int w = 0; w < kWaves; ++w) {
        std::vector<std::thread> threads;
        for (int t = 0; t < kPerWave; ++t) {
            auto* tcb = make_tcb();
            threads.emplace_back([&, tcb]{
                gc->thd_prologue(tcb);
                pg_from_obj(gc->alloc(64, tcb));
                pg_from_obj(gc->alloc(32, tcb));
                pg_from_obj(gc->alloc(128, tcb));
                gc->delete_tcb(tcb);
            });
        }
        for (auto& th : threads) th.join();
    }
    SUCCEED();
}

TEST_F(CollectorTest, ConcurrentBigAllocsAreImmuneToRecycledFullPageBug) {
    constexpr int kThreads = 4;
    constexpr int kPer     = 10;
    std::atomic<int> success{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, tcb]{
            gc->thd_prologue(tcb);
            for (int i = 0; i < kPer; ++i) {
                if (gc->alloc(4000, tcb)) success.fetch_add(1, std::memory_order_relaxed);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(success.load(), kThreads * kPer);
}

TEST_F(CollectorTest, MixedAllocAndBlockingPatternLightLoad) {
    // Half the threads weave set_paused / set_resumed into their loops,
    // each thread stays under one page worth of allocs.
    constexpr int kThreads = 4;
    constexpr int kAllocs  = 20;
    std::atomic<int> alloc_ok{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, t, tcb]{
            gc->thd_prologue(tcb);
            for (int i = 0; i < kAllocs; ++i) {
                if (t % 2 == 0 && i % 5 == 0) {
                    gc->set_paused(tcb);
                    gc->set_resumed(tcb);
                }
                if (gc->alloc(64, tcb)) alloc_ok.fetch_add(1, std::memory_order_relaxed);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(alloc_ok.load(), kThreads * kAllocs);
}

TEST_F(CollectorTest, ManyAllocsSinglePersistentThread) {
    auto* t = make_tcb();
    auto thd = std::thread {[&]() {
        gc->thd_prologue(t);
        constexpr int kAllocs = 500000;
        int ok = 0;
        for (int i = 0; i < kAllocs; ++i) {
            size_t sz = config::sz_classes[i % config::szclass_cnt];
            if (gc->alloc(sz, t)) ++ok;
        }
        EXPECT_EQ(ok, kAllocs);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, ConcurrentAllocsFromMultipleThreads) {
    constexpr int kThreads = 4;
    constexpr int kAllocsPerThread = 200;
    std::atomic<int> success{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, tcb]{
            gc->thd_prologue(tcb);
            for (int i = 0; i < kAllocsPerThread; ++i)
                if (gc->alloc(64, tcb)) success.fetch_add(1, std::memory_order_relaxed);
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(success.load(), kThreads * kAllocsPerThread);
}

TEST_F(CollectorTest, HighChurnTcbLifecycleNoCrashesNoHangs) {
    constexpr int kThreads   = 12;
    constexpr int kCycles    = 30;
    constexpr int kPerCycle  = 8;

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([&]{
            ensure_rpmalloc_thread_ready();
            std::vector<std::thread> new_thds;
            for (int c = 0; c < kCycles; ++c) {
                auto* tcb = make_tcb();
                new_thds.emplace_back([&, tcb]() {
                    gc->thd_prologue(tcb);
                    for (int i = 0; i < kPerCycle; ++i) gc->alloc(32 + 32 * (i % 3), tcb);
                    gc->delete_tcb(tcb);
                });
            }
            for (auto& thd : new_thds) thd.join();
        });
    }
    for (auto& th : threads) th.join();
    SUCCEED();
}

TEST_F(CollectorTest, ConcurrentAllocsAndPauseResumeUnderPressure) {
    constexpr int kThreads = 8;
    constexpr int kIters   = 1000000;
    std::atomic<int> alloc_ok{0};

    std::vector<std::thread> threads;
    for (int t = 0; t < kThreads; ++t) {
        auto* tcb = make_tcb();
        threads.emplace_back([&, tcb]{
            gc->thd_prologue(tcb);
            for (int i = 0; i < kIters; ++i) {
                if (gc->alloc(64, tcb)) alloc_ok.fetch_add(1, std::memory_order_relaxed);
                gc->set_paused(tcb);
                gc->set_resumed(tcb);
            }
            gc->delete_tcb(tcb);
        });
    }
    for (auto& th : threads) th.join();
    EXPECT_EQ(alloc_ok.load(), kThreads * kIters);
}
