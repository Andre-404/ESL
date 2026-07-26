#include <gtest/gtest.h>
#include <cmath>
#include <cstring>
#include <memory>
#include <new>
#include <unordered_set>
#include <vector>
#include "../pruner.h"
#include "../page-allocator.h"
#include "../pg-meta.h"

using namespace gc;
using namespace gc::detail;

namespace {
    class test_page {
    public:
        explicit test_page(size_t block_sz) {
            pg = _a.alloc_pg(block_sz, 1);
        }
        ~test_page() {
            if (pg) {
                pg->unlink();
                _a.free_pgs(pg);
            }
        }
        test_page(const test_page&) = delete;
        test_page& operator=(const test_page&) = delete;

        void mark_n(int n, bool pinned = false) {
            for (int i = 0; i < n; ++i) {
                auto* slot = reinterpret_cast<managed*>(
                        reinterpret_cast<uint8_t*>(pg) + pg->start_off()
                        + i * pg->block_sz());
                new (slot) managed(1, move_state::none);
                pg->record_mark(slot, pinned);
            }
        }

        pg_meta* pg = nullptr;
    private:
        pg_allocator _a;
    };

    std::vector<pg_meta*> walk(pg_meta* head, int max = 32) {
        std::vector<pg_meta*> out;
        for (auto* p = head; p && (int)out.size() < max; p = p->next()) {
            out.push_back(p);
        }
        return out;
    }

    struct prune_result {
        pg_meta* empty;
        pg_meta* in_use;
    };

    // prune used to return { empty, in_use }; it now hands empty pages to a callback one at a
    // time and returns only the in_use chain. Prepending them here is exactly what prune did
    // internally, so both chains - and the reversed order of the empty one - stay as they were.
    prune_result prune(pruner& p, pg_meta* list) {
        auto empty = (pg_meta*)nullptr;
        auto in_use = p.prune(list, [&](pg_meta* pg) {
            pg->link(empty);
            empty = pg;
        });
        return { empty, in_use };
    }
}

TEST(PrunerTest, PruneEmptyListReturnsNullPair) {
    pruner p;
    auto [empty, in_use] = prune(p, nullptr);
    EXPECT_EQ(empty,  nullptr);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(p.get_live_size(), 0);
}

TEST(PrunerTest, SingleEmptyPageEndsInEmptyChain) {
    pruner p;
    test_page tp{64};
    auto [empty, in_use] = prune(p, tp.pg);
    EXPECT_EQ(empty,  tp.pg);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(empty->next(), nullptr);
}

TEST(PrunerTest, SingleInUsePageEndsInInUseChain) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);

    auto in_use = p.prune(tp.pg, [](pg_meta*) { EXPECT_TRUE(false); });
    EXPECT_EQ(in_use, tp.pg);
    EXPECT_EQ(p.get_live_size(), 5 * 64);
}

TEST(PrunerTest, ChainOfThreeAllEmptyEndsAsThreeEmptyPages) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg);
    b.pg->link(c.pg);

    auto [empty, in_use] = prune(p, a.pg);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(walk(empty).size(), 3);
}

TEST(PrunerTest, ChainOfThreeAllInUseEndsAsThreeInUsePagesPreservingOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.mark_n(1); b.mark_n(2); c.mark_n(3);
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = prune(p, a.pg);
    EXPECT_EQ(empty, nullptr);

    auto chain = walk(in_use);
    ASSERT_EQ(chain.size(), 3u);
    EXPECT_EQ(chain[0], a.pg);
    EXPECT_EQ(chain[1], b.pg);
    EXPECT_EQ(chain[2], c.pg);
}

TEST(PrunerTest, InUseChainLastPointerMustNotLeakIntoEmptyChain) {
    pruner p;
    test_page a{64}, b{64};
    a.mark_n(1);   // in_use
    // b is empty
    a.pg->link(b.pg);

    auto [empty, in_use] = prune(p, a.pg);
    ASSERT_EQ(in_use, a.pg);
    EXPECT_EQ(in_use->next(), nullptr)<<"in_use should not be connected to empty in any way";
}

TEST(PrunerTest, AlternatingInUseAndEmptyProducesCleanChains) {
    pruner p;
    test_page a{64}, b{64}, c{64}, d{64};
    a.mark_n(1);  // in_use
    // b empty
    c.mark_n(1);  // in_use
    // d empty
    a.pg->link(b.pg); b.pg->link(c.pg); c.pg->link(d.pg);

    auto [empty, in_use] = prune(p, a.pg);

// Expected: in_use is exactly {a, c}, empty is exactly {b, d} (any order).
    auto in_use_chain = walk(in_use);
    auto empty_chain  = walk(empty);

    std::unordered_set<pg_meta*> in_use_set(in_use_chain.begin(), in_use_chain.end());
    std::unordered_set<pg_meta*> empty_set (empty_chain.begin(),  empty_chain.end());

    EXPECT_EQ(in_use_chain.size(), 2);
    EXPECT_TRUE(in_use_set.count(a.pg));
    EXPECT_TRUE(in_use_set.count(c.pg));
    EXPECT_FALSE(in_use_set.count(b.pg)) << "b should not appear in in_use chain";
    EXPECT_FALSE(in_use_set.count(d.pg)) << "d should not appear in in_use chain";

    EXPECT_EQ(empty_chain.size(), 2);
    EXPECT_TRUE(empty_set.count(b.pg));
    EXPECT_TRUE(empty_set.count(d.pg));
}

// The per size class fragmentation table is private now; estimate_evacuation is the only way
// out of the pruner, so the fragmentation it accumulates is asserted through what compaction
// would gain and what it would have to move.
TEST(PrunerTest, HalfFullPagesYieldHalfTheirBytesBack) {
    pruner p;
    // Four half full pages pack into two, so two pages worth comes back and the live half
    // of all four has to move.
    test_page a{64}, b{64}, c{64}, d{64};
    auto cap  = a.pg->block_cnt();
    auto half = cap / 2;
    for (auto* tp : { &a, &b, &c, &d }) tp->mark_n(half);
    a.pg->link(b.pg); b.pg->link(c.pg); c.pg->link(d.pg);

    prune(p, a.pg);

    auto live_frac = (double)half / cap;
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, (4 - (size_t)std::ceil(4 * live_frac)) * config::page_sz);
    EXPECT_EQ(e.move_bytes, (size_t)(4 * live_frac * config::page_sz));
}

// The estimate used to charge compaction for the *free* part of a page, which made the copy
// decision most pessimistic exactly where compaction pays best. Sparse pages must report far
// less to move than they hand back.
TEST(PrunerTest, MovableBytesAreTheLivePartNotTheFreePart) {
    pruner p;
    std::vector<std::unique_ptr<test_page>> pages;
    for (int i = 0; i < 10; ++i) pages.push_back(std::make_unique<test_page>(64));
    auto cap = pages[0]->pg->block_cnt();
    // ~10% live, so ~90% fragmented.
    for (auto& tp : pages) tp->mark_n(cap / 10);
    for (size_t i = 0; i + 1 < pages.size(); ++i) pages[i]->pg->link(pages[i + 1]->pg);

    prune(p, pages[0]->pg);

    auto e = p.estimate_evacuation();
    EXPECT_GT(e.gain_bytes, 0u);
    EXPECT_LT(e.move_bytes, e.gain_bytes)
        << "at ~90% fragmentation there is far less live data to move than there is to reclaim";
    // The live tenth of ten pages, not the dead nine tenths.
    EXPECT_LT(e.move_bytes, 2 * config::page_sz);
}

TEST(PrunerTest, FullyMarkedPageYieldsNothingToReclaim) {
    pruner p;
    test_page tp{64};
    tp.mark_n(tp.pg->block_cnt());

    prune(p, tp.pg);
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u) << "a full page cannot be compacted away";
    EXPECT_EQ(e.move_bytes, config::page_sz);
}

TEST(PrunerTest, EmptyPagesDoNotContributeToFragmentationStats) {
    pruner p;
    test_page tp{64};
    prune(p, tp.pg);

    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u) << "an empty page is freed outright, it is not compaction's business";
}

TEST(PrunerTest, LargePagesContributeToLiveSizeButNotFrag) {
    pruner p;
    test_page big{4000};  // unknown size -> large, block_cnt=1
    auto* slot = reinterpret_cast<managed*>(reinterpret_cast<uint8_t*>(big.pg) + big.pg->start_off());
    new (slot) managed(1, move_state::none);
    big.pg->record_mark(slot, false);

    prune(p, big.pg);

    EXPECT_EQ(p.get_live_size(), 1 * 4000);
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u) << "large pages are never evacuation candidates";
}

TEST(PrunerTest, FragStatsAccumulateAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{64};
    auto cap = a.pg->block_cnt();
    a.mark_n(cap / 4);
    b.mark_n(cap / 4);

    prune(p, a.pg);
    auto after_one = p.estimate_evacuation();
    prune(p, b.pg);
    auto after_two = p.estimate_evacuation();

    // Two quarter full pages pack into one, which a single page on its own cannot do.
    EXPECT_EQ(after_one.gain_bytes, 0u);
    EXPECT_EQ(after_two.gain_bytes, config::page_sz);
    EXPECT_GT(after_two.move_bytes, after_one.move_bytes) << "the second page's live data must be counted too";
}

TEST(PrunerTest, LiveSizeAccumulatesAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{32};
    a.mark_n(5);   // 5 * 64 = 320
    b.mark_n(10);  // 10 * 32 = 320

    prune(p, a.pg);
    EXPECT_EQ(p.get_live_size(), 320);
    prune(p, b.pg);
    EXPECT_EQ(p.get_live_size(), 640);
}

TEST(PrunerTest, ResetZeroesLiveSizeAndAllFragSlots) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);
    prune(p, tp.pg);
    ASSERT_GT(p.get_live_size(), 0);
    ASSERT_GT(p.estimate_evacuation().move_bytes, 0u);

    p.reset();

    EXPECT_EQ(p.get_live_size(), 0u);
    auto e = p.estimate_evacuation();
    EXPECT_EQ(e.gain_bytes, 0u);
    EXPECT_EQ(e.move_bytes, 0u);
}

TEST(PrunerTest, PostPruneAllPagesHaveLiveCountZero) {
    pruner p;
    test_page a{64}, b{64};
    a.mark_n(7);   // in_use, will become "previously alive"
    // b empty
    a.pg->link(b.pg);

    prune(p, a.pg);

    EXPECT_EQ(a.pg->live_count(), 0);
    EXPECT_EQ(b.pg->live_count(), 0);
}

TEST(PrunerTest, PruneFlipsBitmapMakingMarksTheNewAllocBitmap) {
    pruner p;
    test_page tp{64};
    tp.mark_n(3);

    size_t pre_alloc  = tp.pg->load_alloc_word(0);
    prune(p, tp.pg);
    size_t post_alloc = tp.pg->load_alloc_word(0);

    EXPECT_NE(pre_alloc, post_alloc);
    EXPECT_EQ(post_alloc & 0b111ull, 0b111ull)<<"after flip, alloc bitmap takes on the prior mark bitmap values";
}

TEST(PrunerTest, EmptyChainTerminatesAtNullptr) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = prune(p, a.pg);
    auto chain = walk(empty);
    ASSERT_EQ(chain.size(), 3);
    EXPECT_EQ(chain.back()->next(), nullptr);
}

TEST(PrunerTest, EmptyChainOrderIsReverseOfInputOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = prune(p, a.pg);
    auto chain = walk(empty);
    ASSERT_EQ(chain.size(), 3u);
    EXPECT_EQ(chain[0], c.pg);
    EXPECT_EQ(chain[1], b.pg);
    EXPECT_EQ(chain[2], a.pg);
}

TEST(PrunerTest, MixedLargeAndSmallPagesAreSeparatedCorrectly) {
    pruner p;
    test_page small{64};   // empty small
    test_page big{4000};   // in_use big
    auto* slot = reinterpret_cast<managed*>(reinterpret_cast<uint8_t*>(big.pg) + big.pg->start_off());
    new (slot) managed(1, move_state::none);
    big.pg->record_mark(slot, false);

    small.pg->link(big.pg);
    auto [empty, in_use] = prune(p, small.pg);

    EXPECT_EQ(empty, small.pg);
    EXPECT_EQ(in_use, big.pg);
    EXPECT_EQ(empty->next(), nullptr);

    EXPECT_EQ(p.get_live_size(), 4000);
}