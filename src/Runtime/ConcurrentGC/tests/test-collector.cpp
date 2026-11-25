#include <gtest/gtest.h>
#include <atomic>
#include <cmath>
#include <cstring>
#include <mutex>
#include <thread>
#include <unordered_set>
#include <vector>
#include "../collector.h"
#include "../TCB.h"
#include "../managed.h"
#include "../pg-meta.h"
#include "../gc-heuristic.h"
#include "../gc-api.h"
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
        EXPECT_NE(pg_meta::head_from_ptr(o32),  pg_meta::head_from_ptr(o64));
        EXPECT_NE(pg_meta::head_from_ptr(o64),  pg_meta::head_from_ptr(o128));
        EXPECT_NE(pg_meta::head_from_ptr(o32),  pg_meta::head_from_ptr(o128));
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
        EXPECT_TRUE(pg_meta::head_from_ptr(obj)->szclass() == config::large_class);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, ManyAllocsExerciseNewPageFetches) {
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        std::vector<managed*> objs;
        // One past two full pages, so a third fetch is forced
        for (size_t i = 0; i < config::blocks_in_pg(config::sz_to_class(64)) * 2 + 1; ++i) {
            auto* o = gc->alloc(64, t);
            ASSERT_NE(o, nullptr) << "alloc #" << i;
            objs.push_back(o);
        }
        std::unordered_set<pg_meta*> pages;
        for (auto* o : objs) pages.insert(pg_meta::head_from_ptr(o));
        EXPECT_GE(pages.size(), 3u);
        gc->delete_tcb(t);
    }};
    thd.join();
}

TEST_F(CollectorTest, AllocReturnsObjectsAtPageSlots) {
    // Every returned pointer should land at slot k of its page, for some non-negative k
    auto* t = make_tcb();
    auto thd = std::thread { [&]() {
        gc->thd_prologue(t);
        for (int i = 0; i < 50; ++i) {
            auto* o = gc->alloc(64, t);
            ASSERT_NE(o, nullptr);
            auto* pg = pg_meta::head_from_ptr(o);
            auto offset = reinterpret_cast<uint8_t*>(o) - pg->get_data();
            EXPECT_GE(offset, 0);
            EXPECT_LT(offset, (ptrdiff_t)config::page_sz);
            EXPECT_EQ(offset % pg->block_sz(), 0u);
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

    EXPECT_EQ(pg_meta::head_from_ptr(o), pg_meta::head_from_ptr(o2));
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
                pg_meta::head_from_ptr(gc->alloc(64, tcb));
                pg_meta::head_from_ptr(gc->alloc(32, tcb));
                pg_meta::head_from_ptr(gc->alloc(128, tcb));
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
        virtual gc_tuning tuning() const { return { .initial_heap = kInitialHeap }; }

        collector* gc      = nullptr;
        uint8_t*   gc_flag = nullptr;

        void SetUp() override {
            test_custom::hooks.reset();
            gc_flag = new uint8_t(0);
            gc = new collector(*gc_flag, tuning());
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
            return mem ? new (mem) managed(1) : nullptr;
        }

        // What generated code does between operations. Without it a thread never reaches a
        // safepoint, the collector blocks forever in wait_on_all_ack, and no cycle can finish.
        void poll(tcb* t) { gc->poll(t); }

        // A mutator's load of a word slot, for the raw pointer words of the test hooks. Only
        // right on a mutator thread, whose r14 is reloaded after every pause
        static managed* read_word(size_t& slot) {
            auto w = load_raw(slot);
            if ((w & ~config::color_bit) && ((w ^ platform::read_color()) & config::color_bit))
                w = heal_word(&slot, w);
            return reinterpret_cast<managed*>(w & ~config::color_bit);
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

// A pause is the wait for every thread to stop (ttsp) followed by the stop itself (stw), both
// inside the pause's interval. Maxima, since a cycle may still be publishing its numbers
TEST_F(CollectorCycleTest, PauseSplitsIntoTimeToSafepointAndStw) {
    churn_until_cycles(3);
    using phase = gc_metrics::phase;
    auto& m = gc->metrics();
    EXPECT_GT(m.max_ms(phase::ttsp), 0.0);
    EXPECT_GT(m.max_ms(phase::stw), 0.0);
    EXPECT_LE(m.max_ms(phase::ttsp), m.max_ms(phase::pause));
    EXPECT_LE(m.max_ms(phase::stw), m.max_ms(phase::pause));
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
// allocated slots afterwards - healing a root left from before a copy on the way.
// This is the end to end statement of what mark, copy, root healing and prune owe each other.
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
        // Read like a mutator would, which takes this thread's color
        for (int i = 0; i < kRoots; ++i) {
            auto* obj = read_word(roots[i]);
            ASSERT_NE(obj, nullptr) << "root " << i << " was cleared";
            auto* pg = pg_meta::head_from_ptr(obj);
            EXPECT_EQ(pg->from_interior(reinterpret_cast<uint8_t*>(obj)), obj)
                << "root " << i << " no longer points at an allocated slot";
        }
        gc->delete_tcb(t);
    } };
    thd.join();

    ASSERT_GE(gc->metrics().cycles(), 1u);
}

// Nothing rewrites pointers in the pause: a copy only flips the color, and every reference left
// from before it, in a root or inside an object, is forwarded by its next load or by the next
// mark, which then lets the reloc tables go. Half the chains are only ever walked at the end, so
// they rely on the marker alone, across several copies. Only stranded runs are copied, so the churn
// stays out of the chains' size class, and the chains grow after the first copy to strand more
namespace {
    // Copies at the few KB these tests keep live, which the default floor never allows
    struct CopyingCycleTest : CollectorCycleTest {
        gc_tuning tuning() const override { return { .initial_heap = kInitialHeap, .min_copy_bytes = 0 }; }
    };
}

TEST_F(CopyingCycleTest, ChainsSurviveCopiesWithoutAPointerUpdate) {
    struct node : managed {
        uint64_t id;
        managed* next;
        explicit node(uint64_t i) : managed(1), id(i), next(nullptr) {}
    };
    constexpr int kChains = 16, kLen = 48;   // three runs of 64 B blocks, a quarter of them live
    test_custom::hooks.obj_trace = [](managed* m, std::function<void(managed*)>& cb) {
        if (auto next = read_ref(static_cast<node*>(m)->next, std::memory_order_acquire)) cb(next);
    };
    std::vector<size_t> roots(kChains, 0);
    for (auto& r : roots) gc->register_root(&r);

    size_t flips = 0;
    auto* t = make_tcb();
    auto thd = std::thread { [&, t] {
        gc->thd_prologue(t);
        // Every block is a node, garbage too: the marker may trace whatever the stack scan finds
        auto make = [&](uint64_t id, size_t sz = 64) { return new (gc->alloc(sz, t)) node(id); };
        // Garbage between the nodes, so the runs they end up in are sparse. No poll in here, so
        // no copy moves prev
        int len = 0;
        auto grow = [&](int to) {
            for (int c = 0; c < kChains; ++c) {
                auto* prev = (node*)nullptr;
                if (len) {
                    prev = static_cast<node*>(read_word(roots[c]));
                    for (int k = 1; k < len; ++k) prev = static_cast<node*>(read_ref(prev->next));
                }
                for (int k = len; k < to; ++k) {
                    auto* n = make(c * kLen * 2 + k);
                    if (prev) store_ref(prev->next, (managed*)n); else roots[c] = ptr_to_word(n);
                    prev = n;
                    for (int g = 0; g < 3; ++g) make(~0ull);
                }
            }
            len = to;
        };
        grow(kLen);
        ASSERT_EQ(gc->metrics().cycles(), 0u) << "a cycle ran during seeding";

        auto walk = [&](int c) {
            auto* n = read_word(roots[c]);
            for (int k = 0; k < len; ++k, n = read_ref(static_cast<node*>(n)->next)) {
                ASSERT_NE(n, nullptr) << "chain " << c << " cut at " << k;
                ASSERT_EQ(pg_meta::head_from_ptr(n)->from_interior((uint8_t*)n), n) << "chain " << c << " node " << k;
                ASSERT_EQ(static_cast<node*>(n)->id, uint64_t(c * kLen * 2 + k)) << "chain " << c << " node " << k;
            }
        };
        auto color = platform::read_color();
        for (int i = 0; i < 400'000 && gc->metrics().cycles() < 20; ++i) {
            make(~0ull, 128);
            poll(t);
            if (platform::read_color() == color) continue;
            color = platform::read_color();
            if (flips++ == 0) grow(2 * kLen);
            for (int c = 0; c < kChains; c += 2) walk(c);
        }
        for (int c = 0; c < kChains; ++c) walk(c);
        gc->delete_tcb(t);
    } };
    thd.join();

    EXPECT_GE(flips, 2u) << "fewer than two copies ran, the marker's healing went untested";
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

// Churn of 64-256 byte garbage leaves nothing worth packing. What it allocates black during a mark
// can spread over a few runs, but never past the minimum gain's floor (bug 17), so nothing moves.
// Compaction itself is covered in test-copier.cpp, which builds the runs directly.
TEST_F(CollectorCycleTest, CyclesWithoutFragmentationNeverCopy) {
    churn_until_cycles(4);

    ASSERT_GT(gc->metrics().cycles(), 3u);
    EXPECT_EQ(gc->metrics().max_ms(gc_metrics::phase::compact), 0.0);
}

// add_cleanup end to end. Objects are seeded on a thread that exits, so no stack keeps them alive.
// The hooks read what they get: a block that churn reused has fresh's type 1 instead of 7
namespace {
    struct tagged : managed {
        uint64_t id;
        explicit tagged(uint64_t i) : managed(7), id(i) {}
    };
    uint64_t tag_of(size_t word) {
        auto obj = reinterpret_cast<tagged*>(word);
        return obj->get_type_id() == 7 ? obj->id : ~0ull;
    }
}

TEST_F(CollectorCycleTest, CleanupRunsOnceOnTheRunnerAfterItsObjectDies) {
    std::atomic<int> calls { 0 };
    std::atomic<bool> rooted { true }, ran_while_rooted { false }, runner_running { false };
    std::atomic<uint64_t> got { 0 };
    test_custom::hooks.invoke_cleanup = [&](tcb_handle* thd, size_t cleanup, size_t arg) {
        if (rooted) ran_while_rooted = true;
        runner_running = static_cast<tcb*>(thd)->load_state() == thd_state::running;
        got = tag_of(cleanup) * 1000 + tag_of(arg);
        calls++;
    };
    size_t root = 0, kept = 0;
    gc->register_root(&root);
    gc->register_root(&kept);
    auto* t = make_tcb();
    std::thread { [&, t] {
        gc->thd_prologue(t);
        auto make = [&](uint64_t id) { return ptr_to_word(new (gc->alloc(64, t)) tagged(id)); };
        root = make(1);
        gc->add_cleanup(to_accurate_ptr(root), make(2), make(3));
        // Alive when the collector goes away
        kept = make(4);
        gc->add_cleanup(to_accurate_ptr(kept), make(5), make(6));
        gc->delete_tcb(t);
    } }.join();
    ASSERT_EQ(gc->metrics().cycles(), 0u) << "a cycle ran during seeding";

    churn_until_cycles(3);
    EXPECT_EQ(calls.load(), 0);
    rooted = false;
    store_raw(root, size_t(0));
    // A mark may have read the root just before, so the object can live one more cycle
    churn_until_cycles(gc->metrics().cycles() + 3);
    for (int i = 0; i < 100 && !calls; ++i) std::this_thread::sleep_for(10ms);

    EXPECT_EQ(calls.load(), 1);
    EXPECT_FALSE(ran_while_rooted.load());
    EXPECT_TRUE(runner_running.load()) << "the runner pops and runs only while running";
    EXPECT_EQ(got.load(), 2003u) << "the cleanup and arg survived on the registry's roots alone";
    delete gc;
    gc = nullptr;
    EXPECT_EQ(calls.load(), 1) << "the cleanup of an object alive at the end never runs";
}

// Copies move tracked objects and args. A tracked slot left from before a copy is healed by the next
// pause's queue_dead, a strong one by the next mark or by the runner's pop
TEST_F(CopyingCycleTest, CleanupsFollowTheirObjectsAcrossCopies) {
    constexpr int kEntries = 96;   // 9 garbage blocks each, so a quarter of the 64 B blocks stay live
    std::vector<size_t> roots(kEntries, 0), tracked(kEntries), args(kEntries);
    for (auto& r : roots) gc->register_root(&r);
    std::mutex mtx;
    std::vector<int> runs(kEntries, 0);
    int moved_args = 0, bad = 0;
    std::atomic<int> calls { 0 };
    std::atomic<bool> rooted { true }, ran_while_rooted { false };
    test_custom::hooks.invoke_cleanup = [&](tcb_handle*, size_t cleanup, size_t arg) {
        if (rooted) ran_while_rooted = true;
        auto i = tag_of(cleanup) - 1000;
        auto _ = std::scoped_lock { mtx };
        if (i >= kEntries || tag_of(arg) != 2000 + i) bad++;
        else { runs[i]++; moved_args += arg != args[i]; }
        calls++;
    };
    // Each phase on a thread of its own, so no stack keeps a tracked object alive
    auto run = [&](auto body) {
        auto* t = make_tcb();
        std::thread { [&, t] { gc->thd_prologue(t); body(t); gc->delete_tcb(t); } }.join();
    };
    run([&](tcb* t) {
        auto make = [&](uint64_t id) { return ptr_to_word(new (gc->alloc(64, t)) tagged(id)); };
        for (int i = 0; i < kEntries; ++i) {
            roots[i] = tracked[i] = make(i);
            args[i] = make(2000 + i);
            gc->add_cleanup(to_accurate_ptr(tracked[i]), make(1000 + i), args[i]);
            for (int g = 0; g < 9; ++g) make(~0ull);
        }
    });
    ASSERT_EQ(gc->metrics().cycles(), 0u) << "a cycle ran during seeding";

    // Churn stays out of the 64 B class, which leaves its runs stranded and copied
    auto churn = [&](auto done) {
        run([&](tcb* t) {
            for (int i = 0; i < 400'000 && gc->metrics().cycles() < 40 && !done(); ++i) {
                new (gc->alloc(128, t)) tagged(~0ull);
                poll(t);
            }
        });
    };
    // On to a pause after the copy with the roots still set, whose queue_dead must heal the tracked slots
    size_t flips = 0, flip_cycle = 0;
    auto color = ~0ull;
    churn([&] {
        auto c = platform::read_color();
        if (color != ~0ull && c != color && !flips++) flip_cycle = gc->metrics().cycles();
        color = c;
        return flips && gc->metrics().cycles() >= flip_cycle + 2;
    });
    int moved_tracked = 0;
    run([&](tcb*) { for (int i = 0; i < kEntries; ++i) moved_tracked += (size_t)read_word(roots[i]) != tracked[i]; });
    rooted = false;
    for (auto& r : roots) store_raw(r, size_t(0));
    churn([&] { return calls >= kEntries; });
    for (int i = 0; i < 100 && calls < kEntries; ++i) std::this_thread::sleep_for(10ms);

    ASSERT_GE(flips, 1u) << "nothing was copied";
    EXPECT_GT(moved_tracked, 0) << "no tracked object moved, queue_dead's heal went untested";
    EXPECT_GT(moved_args, 0) << "no arg moved, the strong slots' heal went untested";
    EXPECT_FALSE(ran_while_rooted.load());
    EXPECT_EQ(bad, 0) << "a cleanup got a stale address";
    for (int i = 0; i < kEntries; ++i) EXPECT_EQ(runs[i], 1) << "entry " << i;
}

// A cleanup is ordinary mutator code on the runner: it allocates, polls and takes part in the pauses
// it causes, while another thread churns
TEST_F(CollectorCycleTest, CleanupsMayAllocateAndPollOnTheRunner) {
    constexpr int kEntries = 8;
    std::mutex mtx;
    std::vector<int> runs(kEntries, 0);
    std::atomic<int> calls { 0 };
    test_custom::hooks.invoke_cleanup = [&](tcb_handle* thd, size_t, size_t arg) {
        auto rt = static_cast<tcb*>(thd);
        for (int i = 0; i < 20'000; ++i) { EXPECT_NE(fresh(64, rt), nullptr); poll(rt); }
        auto i = tag_of(arg) - 2000;
        { auto _ = std::scoped_lock { mtx }; if (i < kEntries) runs[i]++; }
        calls++;
    };
    auto* s = make_tcb();
    std::thread { [&, t = s] {
        gc->thd_prologue(t);
        auto make = [&](uint64_t id) { return ptr_to_word(new (gc->alloc(64, t)) tagged(id)); };
        for (int i = 0; i < kEntries; ++i) gc->add_cleanup(to_accurate_ptr(make(i)), make(1000 + i), make(2000 + i));
        gc->delete_tcb(t);
    } }.join();

    auto* t = make_tcb();
    std::thread { [&, t] {
        gc->thd_prologue(t);
        for (size_t i = 0; i < 4'000'000 && calls < kEntries; ++i) { EXPECT_NE(fresh(64 + 64 * (i % 4), t), nullptr); poll(t); }
        gc->delete_tcb(t);
    } }.join();
    for (int i = 0; i < 100 && calls < kEntries; ++i) std::this_thread::sleep_for(10ms);

    EXPECT_EQ(calls.load(), kEntries);
    for (int i = 0; i < kEntries; ++i) EXPECT_EQ(runs[i], 1) << "entry " << i;
    EXPECT_GT(gc->metrics().cycles(), 4u) << "the cleanups' allocation drove cycles";
}

// Exit waits for every cleanup queued when it starts, and keeps collecting meanwhile, since they allocate
TEST_F(CollectorCycleTest, StopWaitsForTheCleanupsAlreadyQueued) {
    constexpr int kEntries = 5;
    std::atomic<int> calls { 0 };
    test_custom::hooks.invoke_cleanup = [&](tcb_handle* thd, size_t, size_t) {
        auto rt = static_cast<tcb*>(thd);
        std::this_thread::sleep_for(50ms);
        for (int i = 0; i < 2'000; ++i) { EXPECT_NE(fresh(64, rt), nullptr); poll(rt); }
        calls++;
    };
    auto* s = make_tcb();
    std::thread { [&, t = s] {
        gc->thd_prologue(t);
        auto make = [&](uint64_t id) { return ptr_to_word(new (gc->alloc(64, t)) tagged(id)); };
        for (int i = 0; i < kEntries; ++i) gc->add_cleanup(to_accurate_ptr(make(i)), make(1000 + i), make(2000 + i));
        gc->delete_tcb(t);
    } }.join();
    ASSERT_EQ(gc->metrics().cycles(), 0u);

    // The first pause queues them all
    churn_until_cycles(2);
    EXPECT_LT(calls.load(), kEntries) << "all of them ran before stop, which then had nothing to wait for";
    // This thread has no tcb, as stop's caller must
    gc->stop();
    EXPECT_EQ(calls.load(), kEntries);
}
