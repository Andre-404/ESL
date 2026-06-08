#include <gtest/gtest.h>
#include <cstring>
#include <new>
#include <unordered_set>
#include <vector>
#include "../arena.h"
#include "../pruner.h"
#include "../pg-manager.h"
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
            if (pg) _a.dealloc_pgs(pg, pg);
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

}

TEST(PrunerTest, PruneEmptyListReturnsNullPair) {
    pruner p;
    auto [empty, in_use] = p.prune(nullptr);
    EXPECT_EQ(empty,  nullptr);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(p.get_live_size(), 0);
}

TEST(PrunerTest, SingleEmptyPageEndsInEmptyChain) {
    pruner p;
    test_page tp{64};
    auto [empty, in_use] = p.prune(tp.pg);
    EXPECT_EQ(empty,  tp.pg);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(empty->next(), nullptr);
}

TEST(PrunerTest, SingleInUsePageEndsInInUseChain) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);

    auto [empty, in_use] = p.prune(tp.pg);
    EXPECT_EQ(empty,  nullptr);
    EXPECT_EQ(in_use, tp.pg);
    EXPECT_EQ(p.get_live_size(), 5 * 64);
}

TEST(PrunerTest, ChainOfThreeAllEmptyEndsAsThreeEmptyPages) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg);
    b.pg->link(c.pg);

    auto [empty, in_use] = p.prune(a.pg);
    EXPECT_EQ(in_use, nullptr);
    EXPECT_EQ(walk(empty).size(), 3);
}

TEST(PrunerTest, ChainOfThreeAllInUseEndsAsThreeInUsePagesPreservingOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.mark_n(1); b.mark_n(2); c.mark_n(3);
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = p.prune(a.pg);
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

    auto [empty, in_use] = p.prune(a.pg);
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

    auto [empty, in_use] = p.prune(a.pg);

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

TEST(PrunerTest, FragmentationOfHalfFullPageIsAboutHalf) {
    pruner p;
    test_page tp{64};
    auto cnt = tp.pg->block_cnt() / 2;
    tp.mark_n(cnt);

    p.prune(tp.pg);
    auto frags = p.get_ppg_frag();
    int cls = config::sz_to_class(64);
    EXPECT_EQ(frags[cls].pg_cnt.load(), 1);
    EXPECT_NEAR(frags[cls].frag_total.load(), 1.0 - (double)cnt / tp.pg->block_cnt(), 1e-9);
}

TEST(PrunerTest, FragmentationOfFullyMarkedPageIsZero) {
    pruner p;
    test_page tp{64};
    tp.mark_n(tp.pg->block_cnt());

    p.prune(tp.pg);
    auto frags = p.get_ppg_frag();
    int cls = config::sz_to_class(64);
    EXPECT_NEAR(frags[cls].frag_total.load(), 0.0, 1e-12);
    EXPECT_EQ(frags[cls].pg_cnt.load(), 1);
}

TEST(PrunerTest, EmptyPagesDoNotContributeToFragmentationStats) {
    pruner p;
    test_page tp{64};
    p.prune(tp.pg);

    auto frags = p.get_ppg_frag();
    for (auto& f : frags) EXPECT_EQ(f.pg_cnt.load(), 0);
}

TEST(PrunerTest, LargePagesContributeToLiveSizeButNotFrag) {
    pruner p;
    test_page big{4000};  // unknown size -> large, block_cnt=1
    auto* slot = reinterpret_cast<managed*>(reinterpret_cast<uint8_t*>(big.pg) + big.pg->start_off());
    new (slot) managed(1, move_state::none);
    big.pg->record_mark(slot, false);

    p.prune(big.pg);

    EXPECT_EQ(p.get_live_size(), 1 * 4000);
    for (auto& f : p.get_ppg_frag()) EXPECT_EQ(f.pg_cnt.load(), 0);
}

TEST(PrunerTest, FragStatsAccumulateAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{64};
    auto tot = a.pg->block_cnt();
    a.mark_n(10);
    b.mark_n(20);

    p.prune(a.pg);
    p.prune(b.pg);

    int cls = config::sz_to_class(64);
    auto frags = p.get_ppg_frag();
    EXPECT_EQ(frags[cls].pg_cnt.load(), 2);
    EXPECT_NEAR(frags[cls].frag_total.load(), (1.0 - 10.0/tot) + (1.0 - 20.0/tot), 1e-9);
}

TEST(PrunerTest, LiveSizeAccumulatesAcrossMultiplePruneCalls) {
    pruner p;
    test_page a{64}, b{32};
    a.mark_n(5);   // 5 * 64 = 320
    b.mark_n(10);  // 10 * 32 = 320

    p.prune(a.pg);
    EXPECT_EQ(p.get_live_size(), 320);
    p.prune(b.pg);
    EXPECT_EQ(p.get_live_size(), 640);
}

TEST(PrunerTest, ResetZeroesLiveSizeAndAllFragSlots) {
    pruner p;
    test_page tp{64};
    tp.mark_n(5);
    p.prune(tp.pg);
    ASSERT_GT(p.get_live_size(), 0);

    p.reset();

    EXPECT_EQ(p.get_live_size(), 0u);
    for (auto& f : p.get_ppg_frag()) {
        EXPECT_EQ(f.pg_cnt.load(), 0);
        EXPECT_EQ(f.frag_total.load(), 0.0);
    }
}

TEST(PrunerTest, PostPruneAllPagesHaveLiveCountZero) {
    pruner p;
    test_page a{64}, b{64};
    a.mark_n(7);   // in_use, will become "previously alive"
    // b empty
    a.pg->link(b.pg);

    p.prune(a.pg);

    EXPECT_EQ(a.pg->live_count(), 0);
    EXPECT_EQ(b.pg->live_count(), 0);
}

TEST(PrunerTest, PruneFlipsBitmapMakingMarksTheNewAllocBitmap) {
    pruner p;
    test_page tp{64};
    tp.mark_n(3);

    size_t pre_alloc  = tp.pg->alloc_word(0);
    p.prune(tp.pg);
    size_t post_alloc = tp.pg->alloc_word(0);

    EXPECT_NE(pre_alloc, post_alloc);
    EXPECT_EQ(post_alloc & 0b111ull, 0b111ull)<<"after flip, alloc bitmap takes on the prior mark bitmap values";
}

TEST(PrunerTest, EmptyChainTerminatesAtNullptr) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = p.prune(a.pg);
    auto chain = walk(empty);
    ASSERT_EQ(chain.size(), 3);
    EXPECT_EQ(chain.back()->next(), nullptr);
}

TEST(PrunerTest, EmptyChainOrderIsReverseOfInputOrder) {
    pruner p;
    test_page a{64}, b{64}, c{64};
    a.pg->link(b.pg); b.pg->link(c.pg);

    auto [empty, in_use] = p.prune(a.pg);
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
    auto [empty, in_use] = p.prune(small.pg);

    EXPECT_EQ(empty, small.pg);
    EXPECT_EQ(in_use, big.pg);
    EXPECT_EQ(empty->next(), nullptr);

    EXPECT_EQ(p.get_live_size(), 4000);
}