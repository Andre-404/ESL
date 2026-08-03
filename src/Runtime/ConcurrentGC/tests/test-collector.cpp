#include <gtest/gtest.h>
#include <atomic>
#include <thread>
#include <unordered_set>
#include <vector>
#include "../collector.h"
#include "../TCB.h"
#include "../managed.h"
#include "../pg-meta.h"
#include "../gc-heuristic.h"
#include "customization-helper.h"
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
            // Must actually run. A collector owns a 1 TiB heap reservation, and
            // leaking one per test overruns the address space thread sanitizer
            // leaves to the application after ~10 fixtures, at which point every
            // later SetUp throws bad_alloc.
            delete gc;
            delete gc_flag;
            gc = nullptr;
            gc_flag = nullptr;
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
        EXPECT_TRUE(pg_from_obj(obj)->is_large());
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
            for (int c = 0; c < kCycles; ++c) {
                auto* tcb = make_tcb();
                auto thd = std::thread { [&, tcb]() {
                    gc->thd_prologue(tcb);
                    for (int i = 0; i < kPerCycle; ++i) gc->alloc(32 + 32 * (i % 3), tcb);
                    gc->delete_tcb(tcb);
                } };
                thd.join();
            }
        });
    }
    for (auto& th : threads) th.join();
    SUCCEED();
}

TEST_F(CollectorTest, ConcurrentAllocsAndPauseResumeUnderPressure) {
    constexpr int kThreads = 8;
    constexpr int kIters   = 100;
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

// ---------------------------------------------------------------------------------------
// Whole-cycle tests.
//
// Every fixture above runs under the default 20 MB initial heap, which almost none of them
// come close to - so the collector never actually collects, and mark/copy/prune/free went
// untested end to end. That is how an evacuated page that was never returned to the allocator,
// and a live count that doubled on every copying cycle, both went unnoticed.
//
// A small initial heap makes cycles cost a few hundred KB of allocation instead of twenty MB.
// ---------------------------------------------------------------------------------------
namespace {
    class CollectorCycleTest : public ::testing::Test {
    protected:
        // Small enough that a few hundred KB of garbage drives a cycle, instead of the 20 MB
        // the default tuning wants.
        static constexpr size_t kInitialHeap = 256u << 10;

        collector* gc      = nullptr;
        uint8_t*   gc_flag = nullptr;

        void SetUp() override {
            test_custom::hooks.reset();
            gc_flag = new uint8_t(0);
            gc = new collector(*gc_flag, gc_tuning { .initial_heap = kInitialHeap });
            std::this_thread::sleep_for(20ms);
            ensure_rpmalloc_thread_ready();
        }
        void TearDown() override {
            delete gc;
            delete gc_flag;
            gc = nullptr;
            gc_flag = nullptr;
            test_custom::hooks.reset();
        }

        tcb* make_tcb() {
            auto* t = gc->create_tcb(nullptr, 0);
            gc->thd_prologue(t);
            return t;
        }

        // alloc hands back raw storage; a managed has to be constructed in it or its state
        // byte is whatever the previous occupant of the slot left behind - which the marker
        // reads, and which after a copying cycle can be a stale forwarding word.
        managed* fresh(size_t sz, tcb* t) {
            auto* mem = gc->alloc(sz, t);
            return mem ? new (mem) managed(1, move_state::none) : nullptr;
        }

        // What generated code does between operations. Without it a thread never reaches a
        // safepoint, the collector blocks forever in wait_on_all_ack, and no cycle can finish.
        void poll(tcb* t) {
            if (needs_safepoint(t)) gc->process_pending(t);
        }

        // Fills the root array, spacing the objects out with garbage so they end up scattered
        // over many pages rather than packed onto one.
        //
        // This has to finish before the first cycle can start. scan_globals reads root slots
        // straight out of the array with a plain load, so a mutator storing into one while a
        // cycle is in flight is an unsynchronized write to memory the collector is reading -
        // and every object reached through that torn read then races too. Generated code
        // writes globals under the write barrier, which is what makes it safe in production;
        // a test has no barrier, so it stays under the allocation trigger instead.
        void seed_roots(std::vector<size_t>& roots, int garbage_per_root, tcb* t) {
            auto budget = (roots.size() * (garbage_per_root + 1) + 1) * 64;
            ASSERT_LT(budget, kInitialHeap) << "seeding must not itself trigger a collection";

            for (size_t i = 0; i < roots.size(); ++i) {
                auto* obj = fresh(64, t);
                ASSERT_NE(obj, nullptr);
                roots[i] = ptr_to_word(obj);
                for (int k = 0; k < garbage_per_root; ++k) ASSERT_NE(fresh(64, t), nullptr);
            }
            ASSERT_EQ(gc->metrics().cycles(), 0u) << "a cycle ran during seeding";
        }

        // Churns garbage on its own thread until at least `want` cycles have completed.
        void churn_until_cycles(size_t want, size_t max_allocs = 2'000'000) {
            auto* t = make_tcb();
            auto thd = std::thread { [&, t] {
                gc->thd_prologue(t);
                for (size_t i = 0; i < max_allocs && gc->metrics().cycles() < want; ++i) {
                    ASSERT_NE(fresh(64 + 64 * (i % 4), t), nullptr) << "allocation " << i << " failed";
                    poll(t);
                }
                gc->delete_tcb(t);
            } };
            thd.join();
        }
    };
}

TEST_F(CollectorCycleTest, CyclesRunToCompletionUnderChurn) {
    churn_until_cycles(5);
    EXPECT_GE(gc->metrics().cycles(), 5u) << "a small heap plus steady garbage must drive real cycles";
}

TEST_F(CollectorCycleTest, ManyCyclesLeaveTheCollectorUsable) {
    churn_until_cycles(20);
    ASSERT_GE(gc->metrics().cycles(), 20u);

    // The collector has to still hand out memory after all that mark/copy/prune/free traffic.
    auto* t = make_tcb();
    auto thd = std::thread { [&, t] {
        gc->thd_prologue(t);
        for (int i = 0; i < 1000; ++i) { EXPECT_NE(fresh(64, t), nullptr); poll(t); }
        gc->delete_tcb(t);
    } };
    thd.join();
}

TEST_F(CollectorCycleTest, ConcurrentThreadsSurviveManyCycles) {
    constexpr int kThreads = 4;
    constexpr int kAllocs  = 40'000;

    std::vector<std::thread> threads;
    std::atomic<int> ok { 0 };
    for (int i = 0; i < kThreads; ++i) {
        auto* t = make_tcb();
        threads.emplace_back([&, t] {
            gc->thd_prologue(t);
            for (int k = 0; k < kAllocs; ++k) {
                if (fresh(64, t)) ok.fetch_add(1, std::memory_order_relaxed);
                poll(t);
            }
            gc->delete_tcb(t);
        });
    }
    for (auto& th : threads) th.join();

    EXPECT_EQ(ok.load(), kThreads * kAllocs);
    EXPECT_GT(gc->metrics().cycles(), 0u) << "this much allocation must have collected at least once";
}

// Objects reachable from a registered root have to survive every cycle, and have to still be
// allocated slots afterwards - following the forwarding pointer if a copying cycle moved them.
// This is the end to end statement of what mark, copy, root fixup and prune owe each other.
TEST_F(CollectorCycleTest, RootedObjectsSurviveAndStayAllocated) {
    constexpr int kRoots = 32;
    // Never resized: the collector holds pointers into this storage for its whole life.
    std::vector<size_t> roots(kRoots, 0);
    for (auto& r : roots) gc->register_root(&r);

    auto* t = make_tcb();
    auto thd = std::thread { [&, t] {
        gc->thd_prologue(t);
        seed_roots(roots, 3, t);
        // Enough garbage to run cycles over and over while the roots stay reachable.
        for (int i = 0; i < 200'000 && gc->metrics().cycles() < 10; ++i) {
            ASSERT_NE(fresh(64, t), nullptr);
            poll(t);
        }
        gc->delete_tcb(t);
    } };
    thd.join();

    ASSERT_GE(gc->metrics().cycles(), 1u);
    for (int i = 0; i < kRoots; ++i) {
        auto* obj = to_accurate_ptr(roots[i]);
        ASSERT_NE(obj, nullptr) << "root " << i << " was cleared";
        auto* pg = pg_from_obj(obj);
        EXPECT_EQ(pg->from_interior(reinterpret_cast<uint8_t*>(obj)), obj)
            << "root " << i << " no longer points at an allocated slot";
    }
}

// Reclamation has to keep up: allocating many times the heap trigger only works if dead pages
// come back. A leak shows up here as an abort out of force_collection, or as a hang.
TEST_F(CollectorCycleTest, GarbageIsReclaimedAcrossManyCycles) {
    constexpr size_t kAllocs = 300'000;   // ~20 MB of 64 byte garbage against a 256 KB trigger
    auto* t = make_tcb();
    auto thd = std::thread { [&, t] {
        gc->thd_prologue(t);
        for (size_t i = 0; i < kAllocs; ++i) { ASSERT_NE(fresh(64, t), nullptr); poll(t); }
        gc->delete_tcb(t);
    } };
    thd.join();

    EXPECT_GT(gc->metrics().cycles(), 10u);
}

// Whether a cycle compacts is a heuristic call, so an ordinary churn test may never take the
// copying path at all. Two things force it here: the copy cost model is zeroed, so any
// reclaimable fragmentation is worth compacting, and the workload deliberately produces that
// fragmentation by scattering a few long lived objects across pages of garbage. Uniform
// garbage is not enough - pages that die outright are freed by the pruner and never register
// as fragmented in the first place.
namespace {
    class CollectorCopyingTest : public CollectorCycleTest {
    protected:
        void SetUp() override {
            test_custom::hooks.reset();
            gc_flag = new uint8_t(0);
            gc = new collector(*gc_flag, gc_tuning {
                .initial_heap = kInitialHeap,
                .copy_bias = 0.0,
                .copy_fixed_cost = std::chrono::nanoseconds { 0 },
            });
            std::this_thread::sleep_for(20ms);
            ensure_rpmalloc_thread_ready();
        }

        // Seeds survivors spread thinly over many pages, then churns garbage to drive cycles.
        // Three dead objects per survivor leaves every seeded page a quarter live, which is
        // the fragmentation compaction exists to collapse.
        void scatter_survivors(std::vector<size_t>& roots, size_t want_cycles = 6) {
            auto* t = make_tcb();
            auto thd = std::thread { [&, t] {
                gc->thd_prologue(t);
                seed_roots(roots, 3, t);
                for (int i = 0; i < 400'000 && gc->metrics().cycles() < want_cycles; ++i) {
                    ASSERT_NE(fresh(64, t), nullptr);
                    poll(t);
                }
                gc->delete_tcb(t);
            } };
            thd.join();
        }
    };
}

TEST_F(CollectorCopyingTest, CompactingCyclesRunAndReclaim) {
    constexpr int kRoots = 800;
    std::vector<size_t> roots(kRoots, 0);
    for (auto& r : roots) gc->register_root(&r);

    scatter_survivors(roots);

    EXPECT_GT(gc->metrics().cycles(), 5u);
    EXPECT_GT(gc->metrics().copy_pause_ms(), 0.0)
        << "sparse surviving pages with the copy cost zeroed must have driven a compaction";
}

TEST_F(CollectorCopyingTest, RootsSurviveCompactingCycles) {
    constexpr int kRoots = 800;
    // Never resized: the collector holds pointers into this storage for its whole life.
    std::vector<size_t> roots(kRoots, 0);
    for (auto& r : roots) gc->register_root(&r);

    scatter_survivors(roots);

    ASSERT_GT(gc->metrics().cycles(), 0u);
    ASSERT_GT(gc->metrics().copy_pause_ms(), 0.0) << "this test is only meaningful if a copy ran";

    // Roots are rewritten by update_globals when their object moves, so each one must still
    // land on a slot its page agrees is allocated - and must not be left pointing at the
    // forwarding stub the copier wrote over the original.
    for (int i = 0; i < kRoots; ++i) {
        auto* obj = to_accurate_ptr(roots[i]);
        ASSERT_NE(obj, nullptr) << "root " << i << " was cleared";
        EXPECT_EQ(obj->state(), move_state::none)
            << "root " << i << " still points at a forwarding stub rather than where it moved to";
        auto* pg = pg_from_obj(obj);
        EXPECT_EQ(pg->from_interior(reinterpret_cast<uint8_t*>(obj)), obj)
            << "root " << i << " no longer points at an allocated slot";
    }
}
