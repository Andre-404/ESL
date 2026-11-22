#include <gtest/gtest.h>
#include <algorithm>
#include <cstring>
#include <map>
#include <memory>
#include <thread>
#include <unordered_set>
#include <vector>

#include "../copier.h"
#include "../pg-meta.h"
#include "../allocator/pg-allocator.h"
#include "../pruner.h"
#include "../ref-slot.h"
#include "customization-helper.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;
using gc::test::for_each_marked;

namespace {
    constexpr size_t kBlock = 64;
    constexpr size_t kCap = config::blocks_in_pg(config::sz_to_class(kBlock));
    constexpr size_t kUnlimited = ~size_t(0);
    constexpr size_t kRun = config::small_obj_pgs * config::page_sz;

    reloc_hdrs& relocs() { return gc::test::heap().relocs(); }

    class CopierTest : public ::testing::Test {
    protected:
        std::vector<std::unique_ptr<test_page>> _pages;

        void SetUp() override {
            test_custom::hooks.reset();
            relocs().clear();
        }
        void TearDown() override {
            relocs().clear();
            _pages.clear();
            test_custom::hooks.reset();
        }

        // A run with exactly these blocks alive, each stamped with a unique id
        test_page* run(std::initializer_list<uint16_t> live) {
            auto& p = _pages.emplace_back(std::make_unique<test_page>(kBlock));
            for (auto b : live) stamp(*p, b);
            return p.get();
        }
        // The first n blocks alive
        test_page* run_n(size_t n) {
            auto& p = _pages.emplace_back(std::make_unique<test_page>(kBlock));
            for (size_t b = 0; b < n; ++b) stamp(*p, uint16_t(b));
            return p.get();
        }
        void stamp(test_page& p, uint16_t b) {
            p.construct(b);
            p.mark(b);
            *(uint64_t*)(p.slot_addr(b) + 8) = uint64_t(p.slot_addr(b));
        }
        static uint64_t id_at(uint8_t* addr) { return *(uint64_t*)(addr + 8); }

        static pg_meta* chain(std::initializer_list<test_page*> pages) {
            pg_meta* head = nullptr;
            pg_meta* prev = nullptr;
            for (auto* p : pages) {
                p->pg()->unlink();
                if (!head) head = p->pg(); else prev->link(p->pg());
                prev = p->pg();
            }
            return head;
        }

        // Where every survivor of the given runs has to end up: ranked across the runs in address
        // order and laid out from the front of that same strip
        static std::map<uint8_t*, uint8_t*> expected_moves(std::vector<test_page*> pages) {
            std::ranges::sort(pages, {}, [](test_page* p) { return p->pg(); });
            auto out = std::map<uint8_t*, uint8_t*> {};
            size_t g = 0;
            for (auto* p : pages)
                for_each_marked(p->pg(), [&](uint16_t b) {
                    out[p->slot_addr(b)] = pages[g / kCap]->slot_addr(uint16_t(g % kCap));
                    ++g;
                });
            return out;
        }

        // An inactive run is the emptied tail, the pruner counts it as empty too
        // What the threads of a pause do between them
        static void compact_all(copier& c) {
            while (auto plan = c.take_plan()) c.compact(*plan, relocs());
        }
        static size_t marked(test_page* p) { return p->pg()->is_active() ? p->pg()->compute_live() : 0; }
    };
}

// ---------------------------------------------------------------------------------------
// Gathering
// ---------------------------------------------------------------------------------------
TEST_F(CopierTest, NothingGatheredSelectsNothing) {
    copier c { 0.85 };
    c.gather(nullptr);
    EXPECT_EQ(c.select(kUnlimited), 0u);
    compact_all(c);
    EXPECT_EQ(c.end_copy(), 0u);
}

TEST_F(CopierTest, AListThatCannotEmptyARunIsNotACandidate) {
    // Together they have less than one run of free blocks, so packing them frees nothing
    auto* a = run_n(kCap * 6 / 10);
    auto* b = run_n(kCap * 6 / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    EXPECT_EQ(c.select(kUnlimited), 0u);
}

TEST_F(CopierTest, DenseRunsAreNotCandidates) {
    auto* dense = run_n(kCap * 9 / 10);
    auto* sparse = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ dense, sparse }));
    // The sparse run alone cannot free anything, the dense one is above the threshold
    EXPECT_EQ(c.select(kUnlimited), 0u);
}

TEST_F(CopierTest, PinnedRunsAreNotCandidates) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    b->mark(0, true);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    EXPECT_EQ(c.select(kUnlimited), 0u) << "with b pinned, a is alone and frees nothing";
}

// ---------------------------------------------------------------------------------------
// Selecting
// ---------------------------------------------------------------------------------------
TEST_F(CopierTest, SelectReportsTheRunsCopyingFrees) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    auto* c3 = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b, c3 }));
    EXPECT_EQ(c.select(kUnlimited), 2 * kRun);
}

TEST_F(CopierTest, BudgetCapsTheBytesSelected) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    // Enough for one of them, and one run alone frees nothing
    EXPECT_EQ(c.select(kCap / 10 * kBlock), 0u);
}

TEST_F(CopierTest, AGainBelowTheMinimumMakesNoPlans) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    EXPECT_EQ(c.select(kUnlimited, 2 * kRun), 0u) << "one run freed, two required";
    EXPECT_FALSE(c.has_plans());
    EXPECT_EQ(c.take_plan(), nullptr);
    c.end_copy();
}

// The budget goes where it frees the most per byte, not to whichever list came first
TEST_F(CopierTest, TheSparsestListsWinTheBudget) {
    auto* d1 = run_n(kCap * 6 / 10);
    auto* d2 = run_n(kCap * 6 / 10);
    auto* d3 = run_n(kCap * 6 / 10);
    auto* s1 = run_n(kCap / 10);
    auto* s2 = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ d1, d2, d3 }));
    c.gather(chain({ s1, s2 }));

    // Room for the sparse list, nowhere near enough for the dense one as well
    EXPECT_EQ(c.select(3 * kCap / 10 * kBlock), kRun);
    compact_all(c);
    c.end_copy();

    EXPECT_EQ(marked(s1) + marked(s2), 2 * (kCap / 10));
    EXPECT_EQ(marked(s1) == 0 || marked(s2) == 0, true) << "the sparse list was packed into one run";
    for (auto* d : { d1, d2, d3 }) EXPECT_EQ(marked(d), kCap * 6 / 10) << "the dense list was left alone";
}

// The sparsest runs of a and b are the sparsest overall, but neither list frees a run without its
// dense one. Picking runs by sparsity spent the budget on them and freed nothing, a budget spent
// on whole steps packs c instead
TEST_F(CopierTest, TheBudgetIsOnlySpentOnRunsThatFreeSomething) {
    auto* a1 = run_n(kCap / 10);
    auto* a2 = run_n(kCap * 8 / 10);
    auto* b1 = run_n(kCap / 10);
    auto* b2 = run_n(kCap * 8 / 10);
    auto* c1 = run_n(kCap * 4 / 10);
    auto* c2 = run_n(kCap * 4 / 10);
    copier c { 0.85 };
    c.gather(chain({ a1, a2 }));
    c.gather(chain({ b1, b2 }));
    c.gather(chain({ c1, c2 }));

    EXPECT_EQ(c.select(kCap * 85 / 100 * kBlock), kRun);
    compact_all(c);
    c.end_copy();
    EXPECT_EQ(marked(c1) == 0 || marked(c2) == 0, true) << "c was packed into one run";
    for (auto* p : { a1, a2, b1, b2 }) EXPECT_EQ(relocs().get(size_t(p->pg() - gc::test::heap().meta_base())), 0u);
}

// A list stops once its next step doesn't fit, the others keep spending what is left
TEST_F(CopierTest, AListThatNoLongerFitsLeavesTheBudgetToTheOthers) {
    auto* a1 = run_n(kCap * 7 / 10);
    auto* a2 = run_n(kCap * 7 / 10);
    auto* b1 = run_n(kCap / 10);
    auto* b2 = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ a1, a2 }));
    c.gather(chain({ b1, b2 }));
    // b's step costs 20% of a run, a's 140%: only b fits
    EXPECT_EQ(c.select(kCap / 2 * kBlock), kRun);
}

// Dropping the densest run of a list is free when the rest still empties as many runs
TEST_F(CopierTest, ADenseRunThatFreesNothingMoreIsDropped) {
    auto* a = run_n(kCap * 3 / 10);
    auto* b = run_n(kCap * 3 / 10);
    auto* dense = run_n(kCap * 7 / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b, dense }));
    EXPECT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();

    EXPECT_EQ(marked(dense), kCap * 7 / 10);
    EXPECT_EQ(relocs().get(size_t(dense->pg() - gc::test::heap().meta_base())), 0u) << "never compacted";
}

// ---------------------------------------------------------------------------------------
// Compacting
// ---------------------------------------------------------------------------------------
TEST_F(CopierTest, SurvivorsArePackedInOrderAndForwardToTheirNewHome) {
    auto* a = run({ 1, 4, 5, 9, 14, 200, 1000 });
    auto* b = run({ 0, 3, 8, 9, 10, 15, 63, 64, 65 });
    auto* d = run({ 2, 6, 7, 11, 12, 13, 15, 1023 });
    auto want = expected_moves({ a, b, d });

    copier c { 0.85 };
    c.gather(chain({ a, b, d }));
    ASSERT_EQ(c.select(kUnlimited), 2 * kRun);
    compact_all(c);
    EXPECT_GT(c.end_copy(), 0u);

    for (auto [from, to] : want) {
        EXPECT_EQ(reloc::forward(from), to);
        EXPECT_EQ(id_at(to), uint64_t(from)) << "the object that used to live at the old address is there";
    }
}

// 48 isn't a power of two, so a block's index has to come from a division of its offset
TEST_F(CopierTest, BlocksThatArentAPowerOfTwoForwardToTheirNewHome) {
    constexpr size_t blk = 48;
    constexpr size_t cap = config::blocks_in_pg(config::sz_to_class(blk));
    test_page* runs[2];
    for (size_t r = 0; r < 2; ++r) {
        runs[r] = _pages.emplace_back(std::make_unique<test_page>(blk)).get();
        for (uint16_t b = uint16_t(r); b < cap; b += 5) stamp(*runs[r], b);
    }
    std::ranges::sort(runs, {}, [](test_page* p) { return p->pg(); });
    auto want = std::map<uint8_t*, uint8_t*> {};
    size_t g = 0;
    for (auto* p : runs)
        for_each_marked(p->pg(), [&](uint16_t b) {
            want[p->slot_addr(b)] = runs[g / cap]->slot_addr(uint16_t(g % cap));
            ++g;
        });

    copier c { 0.85 };
    c.gather(chain({ runs[0], runs[1] }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();
    for (auto [from, to] : want) {
        EXPECT_EQ(reloc::forward(from), to);
        EXPECT_EQ(id_at(to), uint64_t(from));
    }
}

// 640 byte blocks take the one survivor at a time path and straddle pages. Three runs of 60 pack into
// two, and the second's 43rd survivor (block 71) crosses into the second kept run in the middle of page 5
TEST_F(CopierTest, BigBlocksForwardToTheirNewHome) {
    constexpr size_t blk = 640;
    constexpr size_t cap = config::blocks_in_pg(config::sz_to_class(blk));
    test_page* runs[3];
    for (auto& r : runs) {
        r = _pages.emplace_back(std::make_unique<test_page>(blk)).get();
        for (size_t i = 0; i < 60; ++i) stamp(*r, uint16_t(i * cap / 60));
    }
    std::ranges::sort(runs, {}, [](test_page* p) { return p->pg(); });
    auto want = std::map<uint8_t*, uint8_t*> {};
    size_t g = 0;
    for (auto* p : runs)
        for_each_marked(p->pg(), [&](uint16_t b) {
            want[p->slot_addr(b)] = runs[g / cap]->slot_addr(uint16_t(g % cap));
            ++g;
        });

    copier c { 0.85 };
    c.gather(chain({ runs[0], runs[1], runs[2] }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();
    for (auto [from, to] : want) {
        EXPECT_EQ(reloc::forward(from), to);
        EXPECT_EQ(id_at(to), uint64_t(from));
    }
}

// Three runs of 600 survivors only free a run when all three are packed. The first keeps its 600
// and takes the second's first 424, so the second's 425th survivor crosses into it at block 424
TEST_F(CopierTest, ACrossingInsideAUnitSplitsIt) {
    test_page* runs[3];
    for (auto& r : runs) r = run_n(600);
    std::ranges::sort(runs, {}, [](test_page* p) { return p->pg(); });
    auto want = expected_moves({ runs[0], runs[1], runs[2] });

    copier c { 0.85 };
    c.gather(chain({ runs[0], runs[1], runs[2] }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();

    auto first = [](test_page* p) { return heap_layout::page_idx(p->pg()->get_data()); };
    EXPECT_EQ(reloc::unpack(relocs().get(first(runs[1]))).split, 424u);
    EXPECT_EQ(reloc::unpack(relocs().get(first(runs[2]))).split, reloc::split_none);
    for (size_t pg = 1; pg < config::small_obj_pgs; ++pg)
        EXPECT_EQ(relocs().get(first(runs[1]) + pg), 0u) << "page " << pg << ", a run's entry sits at its first page";
    for (auto [from, to] : want) {
        EXPECT_EQ(reloc::forward(from), to);
        EXPECT_EQ(id_at(to), uint64_t(from));
    }
}

TEST_F(CopierTest, KeptRunsAreMarkedFromTheFrontAndTheTailIsEmpty) {
    // 3 * 60% packs into one full run and most of a second, and needs all three to free one
    auto* a = run_n(kCap * 6 / 10);
    auto* b = run_n(kCap * 6 / 10);
    auto* d = run_n(kCap * 6 / 10);
    auto total = marked(a) + marked(b) + marked(d);

    copier c { 0.85 };
    c.gather(chain({ a, b, d }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();

    std::vector<test_page*> sorted { a, b, d };
    std::ranges::sort(sorted, {}, [](test_page* p) { return p->pg(); });
    EXPECT_EQ(marked(sorted[0]), kCap);
    EXPECT_EQ(marked(sorted[1]), total - kCap);
    EXPECT_FALSE(sorted[2]->pg()->is_active()) << "the emptied tail is retired";
    // Marked from the front: every marked block comes before every unmarked one
    size_t blk = 0;
    for_each_marked(sorted[1]->pg(), [&](uint16_t b) { EXPECT_EQ(b, blk++) << "block " << b; });
    EXPECT_EQ(blk, total - kCap);
}

TEST_F(CopierTest, ForwardLeavesUncompactedRunsAlone) {
    auto* lone = run({ 3 });
    EXPECT_EQ(reloc::forward(lone->slot_addr(3)), lone->slot_addr(3));
}

TEST_F(CopierTest, CompactThenPruneFreesTheEmptiedRunsAndCountsSurvivorsOnce) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    auto* head = chain({ a, b });
    copier c { 0.85 };
    c.gather(head);
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();

    std::vector<pg_meta*> freed;
    pruner p;
    auto* in_use = p.prune(head, gc::test::bits(), [&](pg_meta* pg) { freed.push_back(pg); });

    ASSERT_EQ(freed.size(), 1u);
    EXPECT_EQ(freed[0], std::max(a->pg(), b->pg())) << "the higher run is the emptied tail";
    EXPECT_EQ(in_use, std::min(a->pg(), b->pg()));
    EXPECT_EQ(p.live_bytes(), 2 * (kCap / 10) * kBlock);
    // The pruner handed the freed run back through the callback; don't free it twice
    for (auto& pg : _pages) if (pg->pg() == freed[0]) pg.release();
}

TEST_F(CopierTest, EndCopyReportsTheBytesMovedAndResets) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    // Only the higher run's survivors move, the lower ones are already in place
    EXPECT_EQ(c.end_copy(), (kCap / 10) * kBlock);
    EXPECT_EQ(c.select(kUnlimited), 0u) << "nothing left over from the last cycle";
}

// ---------------------------------------------------------------------------------------
// Splitting for several threads
// ---------------------------------------------------------------------------------------
// Four runs at 10% pack into one, split for two threads they pack into one per plan. Every plan is a
// strip of its own and its survivors stay in it, so two threads can compact them at once
TEST_F(CopierTest, SplitPlansAreStripsOfTheirOwn) {
    auto pages = std::vector<test_page*> { run_n(kCap / 10), run_n(kCap / 10), run_n(kCap / 10), run_n(kCap / 10) };
    copier c { 0.85 };
    c.gather(chain({ pages[0], pages[1], pages[2], pages[3] }));
    ASSERT_EQ(c.select(kUnlimited, 1, 2), 2 * kRun) << "each plan keeps a run, one more than unsplit";

    copy_plan* plans[] = { c.take_plan(), c.take_plan() };
    ASSERT_TRUE(plans[0] && plans[1]);
    EXPECT_EQ(c.take_plan(), nullptr);
    auto want = std::map<uint8_t*, uint8_t*> {};
    for (auto* plan : plans) {
        ASSERT_EQ(plan->runs.size(), 2u);
        auto mine = std::vector<test_page*> {};
        for (auto* pg : plan->runs) mine.push_back(*std::ranges::find(pages, pg, [](test_page* p) { return p->pg(); }));
        want.merge(expected_moves(mine));
    }
    auto other = std::thread { [&] { c.compact(*plans[1], relocs()); } };
    c.compact(*plans[0], relocs());
    other.join();
    c.end_copy();

    EXPECT_EQ(want.size(), 4 * (kCap / 10));
    for (auto [from, to] : want) {
        EXPECT_EQ(reloc::forward(from), to);
        EXPECT_EQ(id_at(to), uint64_t(from));
    }
}

// Runs are dealt to the plans in density order, so dense runs next to each other in memory still
// spread evenly over the threads
TEST_F(CopierTest, SplitPlansGetTheSameMixOfDensities) {
    auto* d1 = run_n(kCap * 3 / 10);
    auto* d2 = run_n(kCap * 3 / 10);
    auto* s1 = run_n(kCap / 20);
    auto* s2 = run_n(kCap / 20);
    auto* s3 = run_n(kCap / 20);
    auto* s4 = run_n(kCap / 20);
    auto* s5 = run_n(kCap / 20);
    auto* s6 = run_n(kCap / 20);
    copier c { 0.85 };
    c.gather(chain({ d1, d2, s1, s2, s3, s4, s5, s6 }));
    ASSERT_EQ(c.select(kUnlimited, 1, 2), 6 * kRun);

    auto* a = c.take_plan();
    auto* b = c.take_plan();
    ASSERT_TRUE(a && b);
    EXPECT_EQ(a->runs.size(), 4u);
    EXPECT_EQ(b->runs.size(), 4u);
    EXPECT_EQ(a->bytes, (kCap * 3 / 10 + 3 * (kCap / 20)) * kBlock) << "one dense and three sparse runs each";
    EXPECT_EQ(a->bytes, b->bytes);
    c.compact(*a, relocs());
    c.compact(*b, relocs());
    c.end_copy();
}

// Two runs at 30% free one when packed together and none apart, so they stay one plan
TEST_F(CopierTest, NeverMorePlansThanRunsFreed) {
    auto* a = run_n(kCap * 3 / 10);
    auto* b = run_n(kCap * 3 / 10);
    copier c { 0.85 };
    c.gather(chain({ a, b }));
    EXPECT_EQ(c.select(kUnlimited, 1, 2), kRun);
    EXPECT_NE(c.take_plan(), nullptr);
    EXPECT_EQ(c.take_plan(), nullptr);
    c.end_copy();
}

// Split two ways, the middle run is a plan alone that empties nothing. Every other block of it is
// free, so it's compacted for its holes
TEST_F(CopierTest, APlanThatFreesNothingIsKeptForItsHoles) {
    auto* sparse = run_n(kCap / 10);
    auto* holey = _pages.emplace_back(std::make_unique<test_page>(kBlock)).get();
    for (uint16_t b = 0; b < kCap * 8 / 10; b += 2) stamp(*holey, b);
    auto* dense = run_n(kCap * 45 / 100);
    auto old = std::vector<uint8_t*> {};
    for_each_marked(holey->pg(), [&](uint16_t b) { old.push_back(holey->slot_addr(b)); });

    copier c { 0.85 };
    c.gather(chain({ sparse, holey, dense }));
    ASSERT_EQ(c.select(kUnlimited, 1, 2), kRun) << "the holey plan adds nothing to what's freed";
    compact_all(c);
    c.end_copy();

    EXPECT_EQ(marked(holey), old.size());
    for (size_t j = 0; j < old.size(); ++j) {
        EXPECT_EQ(reloc::forward(old[j]), holey->slot_addr(uint16_t(j)));
        EXPECT_EQ(id_at(holey->slot_addr(uint16_t(j))), uint64_t(old[j]));
    }
}

TEST_F(CopierTest, APlanThatFreesNothingWithoutHolesIsDropped) {
    auto* sparse = run_n(kCap / 10);
    auto* solid = run_n(kCap * 4 / 10);
    auto* dense = run_n(kCap * 45 / 100);
    copier c { 0.85 };
    c.gather(chain({ sparse, solid, dense }));
    ASSERT_EQ(c.select(kUnlimited, 1, 2), kRun);
    compact_all(c);
    c.end_copy();

    EXPECT_EQ(marked(solid), kCap * 4 / 10);
    EXPECT_EQ(relocs().get(size_t(solid->pg() - gc::test::heap().meta_base())), 0u) << "never compacted";
}
