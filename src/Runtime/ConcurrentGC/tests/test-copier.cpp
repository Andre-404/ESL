#include <gtest/gtest.h>
#include <algorithm>
#include <cstring>
#include <map>
#include <memory>
#include <unordered_set>
#include <vector>

#include "../copier.h"
#include "../pg-meta.h"
#include "../allocator/pg-allocator.h"
#include "../pruner.h"
#include "customization-helper.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

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
            // Survivors carry an id past the header so a test can see what ended up where
            test_custom::hooks.obj_copy = [](managed* s, managed* d) { std::memcpy(d, s, kBlock); };
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
                for (auto it = pg_meta::pg_slots_iter { p->pg(), 0 }; !it.at_end(); it.next())
                    if (it.is_marked()) {
                        out[(uint8_t*)it.get()] = pages[g / kCap]->slot_addr(uint16_t(g % kCap));
                        ++g;
                    }
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
    test_custom::hooks.obj_copy = [](managed* s, managed* d) { std::memcpy(d, s, blk); };
    test_page* runs[2];
    for (size_t r = 0; r < 2; ++r) {
        runs[r] = _pages.emplace_back(std::make_unique<test_page>(blk)).get();
        for (uint16_t b = uint16_t(r); b < cap; b += 5) stamp(*runs[r], b);
    }
    std::ranges::sort(runs, {}, [](test_page* p) { return p->pg(); });
    auto want = std::map<uint8_t*, uint8_t*> {};
    size_t g = 0;
    for (auto* p : runs)
        p->pg()->for_each_marked([&](uint16_t b) {
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

// Three runs of 600 survivors only free a run when all three are packed. The first keeps its 600
// and takes the second's first 424, so the second's 425th survivor crosses into it at block 424:
// page 3 of the run, position 40
TEST_F(CopierTest, ACrossingInsideAPageSplitsItsUnit) {
    test_page* runs[3];
    for (auto& r : runs) r = run_n(600);
    std::ranges::sort(runs, {}, [](test_page* p) { return p->pg(); });
    auto want = expected_moves({ runs[0], runs[1], runs[2] });

    copier c { 0.85 };
    c.gather(chain({ runs[0], runs[1], runs[2] }));
    ASSERT_EQ(c.select(kUnlimited), kRun);
    compact_all(c);
    c.end_copy();

    constexpr size_t per_page = config::page_sz / kBlock;
    auto first = heap_layout::page_idx(runs[1]->pg()->get_data());
    auto split = [&](size_t pg) { return reloc::unpack(relocs().get(first + pg)).split; };
    EXPECT_EQ(split(424 / per_page), 424 % per_page);
    for (size_t pg = 0; pg < 424 / per_page; ++pg) EXPECT_EQ(split(pg), reloc::split_none) << "page " << pg;
    for (size_t pg = 600 / per_page + 1; pg < config::small_obj_pgs; ++pg)
        EXPECT_EQ(relocs().get(first + pg), 0u) << "page " << pg << " has no survivor, so no table";
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
    for (auto it = pg_meta::pg_slots_iter { sorted[1]->pg(), 0 }; !it.at_end(); it.next(), ++blk)
        EXPECT_EQ(it.is_marked(), blk < total - kCap) << "block " << blk;
}

TEST_F(CopierTest, ForwardLeavesUncompactedRunsAlone) {
    auto* lone = run({ 3 });
    EXPECT_EQ(reloc::forward(lone->slot_addr(3)), lone->slot_addr(3));
}

TEST_F(CopierTest, ForwardLeavesUnmanagedObjectsAlone) {
    // Constants compiled into the program, outside the heap entirely
    alignas(16) static uint8_t constant[16];
    auto* obj = new (constant) managed(1, move_state::unmanaged);
    EXPECT_EQ(forward(obj), obj);
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
// Updating
// ---------------------------------------------------------------------------------------
TEST_F(CopierTest, UpdateGlobalsLeavesUnmovedPointerEncoded) {
    auto* p = run({ 0 });
    copier c { 0.85 };
    size_t root = reinterpret_cast<size_t>(p->slot(0));
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);
    EXPECT_EQ(root, reinterpret_cast<size_t>(p->slot(0)));
}

TEST_F(CopierTest, UpdateGlobalsFollowsTheRelocTables) {
    auto* a = run_n(kCap / 10);
    auto* b = run_n(kCap / 10);
    auto* hi = std::max(a, b, [](test_page* x, test_page* y) { return x->pg() < y->pg(); });
    auto want = expected_moves({ a, b });

    copier c { 0.85 };
    c.gather(chain({ a, b }));
    ASSERT_GT(c.select(kUnlimited), 0u);
    compact_all(c);
    c.end_copy();

    size_t root = reinterpret_cast<size_t>(hi->slot(0));
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);
    EXPECT_EQ(root, reinterpret_cast<size_t>(want[hi->slot_addr(0)]));
}

TEST_F(CopierTest, UpdateGlobalsSkipsRootsThatToAccuratePtrRejects) {
    test_custom::hooks.to_accurate_ptr = [](size_t) -> managed* { return nullptr; };
    copier c { 0.85 };
    size_t root = 0xDEADBEEF;
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);
    EXPECT_EQ(root, 0xDEADBEEFu) << "rejected root must not be rewritten";
}

TEST_F(CopierTest, UpdateGlobalsRunsPtrToWordOnEveryUpdate) {
    auto* p = run({ 0 });
    test_custom::hooks.ptr_to_word = [](managed* m) { return reinterpret_cast<size_t>(m) | 0xAB00000000000000ull; };
    copier c { 0.85 };
    size_t root = reinterpret_cast<size_t>(p->slot(0));
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);
    EXPECT_EQ(root & 0xFF00000000000000ull, 0xAB00000000000000ull);
}

TEST_F(CopierTest, UpdatePtrsVisitsMarkedSlotsOnly) {
    auto* p = run({ 0, 1, 2 });
    p->construct(3);
    std::unordered_set<managed*> updated;
    test_custom::hooks.obj_update_ptrs = [&](managed* m) { updated.insert(m); };

    copier c { 0.85 };
    c.update_ptrs(p->pg());
    EXPECT_EQ(updated.size(), 3u);
    EXPECT_FALSE(updated.count(p->slot(3))) << "dead";
}

TEST_F(CopierTest, UpdatePtrsOnEmptyPageDoesNothing) {
    auto* p = run({});
    bool any = false;
    test_custom::hooks.obj_update_ptrs = [&](managed*) { any = true; };
    copier c { 0.85 };
    c.update_ptrs(p->pg());
    EXPECT_FALSE(any);
}
