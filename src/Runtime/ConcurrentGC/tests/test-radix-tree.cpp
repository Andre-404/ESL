#include <gtest/gtest.h>

#include <random>
#include <vector>

#include "../radix-tree.h"

using namespace gc;
using namespace gc::detail;

namespace gc::detail {
    // Befriended by radix_tree_t so tests can exercise the private bottom-level helper
    // find_in_chunk, whose within-chunk-only behaviour is invisible through the public find.
    template<typename Geom>
    struct radix_tree_test_peer {
        static std::optional<std::size_t> find_in_chunk(const radix_tree_t<Geom>& t,
                                                        std::size_t chunk_idx, std::size_t n) {
            return t.find_in_chunk(chunk_idx, n);
        }
    };
}

namespace {
    // Terser alias for the tests below.
    template<typename Geom>
    std::optional<std::size_t> fic(const radix_tree_t<Geom>& t, std::size_t chunk_idx, std::size_t n) {
        return radix_tree_test_peer<Geom>::find_in_chunk(t, chunk_idx, n);
    }

    // Backing store for a tree, sized from its geometry. Starts wholly free.
    template<typename Geom>
    struct tree_fixture {
        std::vector<summary> sums;
        std::vector<uint64_t> words;
        radix_tree_t<Geom> tree;

        tree_fixture() : sums(Geom::total_entries), words(Geom::words),
                         tree(sums.data(), words.data()) {
            tree.mark_free(0, Geom::total_pages);
            tree.flush_free();
        }
    };

    // Naive model: a flag per page plus a linear first fit. Deliberately the dumbest
    // implementation that is obviously correct.
    struct reference {
        std::vector<char> used;
        explicit reference(std::size_t pages) : used(pages, 0) {}

        std::optional<std::size_t> find(std::size_t n) const {
            std::size_t run = 0;
            for (std::size_t i = 0; i < used.size(); ++i) {
                run = used[i] ? 0 : run + 1;
                if (run >= n) return i + 1 - n;
            }
            return std::nullopt;
        }
        // Naive aligned first fit: step by align, scan n.
        std::optional<std::size_t> find_aligned(std::size_t n, std::size_t align) const {
            for (std::size_t i = 0; i + n <= used.size(); i += align) {
                bool ok = true;
                for (std::size_t k = 0; k < n; ++k) if (used[i + k]) { ok = false; break; }
                if (ok) return i;
            }
            return std::nullopt;
        }
        void set(std::size_t start, std::size_t n, char v) {
            for (std::size_t i = 0; i < n; ++i) used[start + i] = v;
        }
    };

    // 3 levels, 2 top-level entries, 128 chunks. Small enough that the reference scan is
    // cheap, so this one carries the bulk of the random operations.
    using small_geom = tree_geometry<1u << 16>;
    // 5 levels, 8 top-level entries - the same shape as the real heap geometry.
    using big_geom = tree_geometry<1u << 24>;
}

TEST(TreeGeometry, SmallGeometryHasTheExpectedShape) {
    EXPECT_EQ(small_geom::levels, 3u);
    EXPECT_EQ(small_geom::level_pages[0], 32768u);
    EXPECT_EQ(small_geom::level_pages[small_geom::levels - 1], config::chunk_pages);
    EXPECT_EQ(small_geom::level_entries[0], 2u);
    EXPECT_EQ(small_geom::chunks, 128u);
}

TEST(TreeGeometry, RealHeapGeometryMatchesThePlan) {
    EXPECT_EQ(heap_geometry::levels, 5u);
    EXPECT_EQ(heap_geometry::level_pages[0], summary::max_packed);
    // The roots tile the heap - find() scans all of them as a single window
    EXPECT_EQ(heap_geometry::level_entries[0], config::total_pages / summary::max_packed);
    EXPECT_EQ(heap_geometry::level_entries[0] * heap_geometry::level_pages[0], config::total_pages);
    EXPECT_EQ(heap_geometry::chunks, config::total_pages / config::chunk_pages);
    // Every level-0 entry must be exactly representable, which is what the full bit encodes.
    EXPECT_EQ(heap_geometry::level_pages[0], 1u << 21);
}

TEST(SummaryTest, WhollyFreeChunkIsFull) {
    uint64_t words[config::chunk_words] = {};
    auto s = summary::summarize(words);
    EXPECT_TRUE(s.start() == config::chunk_pages);
    EXPECT_EQ(s.max(), config::chunk_pages);
    EXPECT_EQ(s.end(), config::chunk_pages);
}

TEST(SummaryTest, WhollyAllocatedChunkIsZero) {
    uint64_t words[config::chunk_words];
    for (auto& w : words) w = ~uint64_t(0);
    auto s = summary::summarize(words);
    EXPECT_EQ(s.start(), 0u);
    EXPECT_EQ(s.max(), 0u);
    EXPECT_EQ(s.end(), 0u);
}

TEST(SummaryTest, RunStraddlingAWordBoundaryIsMeasuredWhole) {
    uint64_t words[config::chunk_words] = {};
    // allocate everything except pages [60, 70) -> a 10 page run across the word boundary
    for (auto& w : words) w = ~uint64_t(0);
    for (int p = 60; p < 70; ++p) words[p / 64] &= ~(uint64_t(1) << (p % 64));
    auto s = summary::summarize(words);
    EXPECT_EQ(s.max(), 10u);
    EXPECT_EQ(s.start(), 0u);
    EXPECT_EQ(s.end(), 0u);
}

// --- find ---------------------------------------------------------------------------------

TEST(RadixTreeTest, FindOnEmptyTreeReturnsZero) {
    tree_fixture<small_geom> f;

    EXPECT_TRUE(f.tree.find(1).has_value());
    EXPECT_EQ(*f.tree.find(1), 0u);

    EXPECT_TRUE(f.tree.find(small_geom::total_pages).has_value());
    EXPECT_EQ(*f.tree.find(small_geom::total_pages), 0u);
}

TEST(RadixTreeTest, FindReturnsNotFoundWhenRequestExceedsHeap) {
    tree_fixture<small_geom> f;
    EXPECT_FALSE(f.tree.find(small_geom::total_pages + 1).has_value());
    EXPECT_FALSE(f.tree.find(0).has_value());
}

TEST(RadixTreeTest, FindSkipsAllocatedPrefix) {
    tree_fixture<small_geom> f;
    f.tree.mark_alloc(0, 100);
    EXPECT_EQ(f.tree.find(1), 100u);
}

TEST(RadixTreeTest, FindReusesAHoleThatExactlyFits) {
    tree_fixture<small_geom> f;
    f.tree.mark_alloc(0, 1000);
    f.tree.mark_free(200, 5);
    f.tree.flush_free();
    EXPECT_EQ(f.tree.find(5), 200u);
    EXPECT_EQ(f.tree.find(6), 1000u);   // hole too small, fall past the allocated prefix
}

TEST(RadixTreeTest, FindLocatesRunSpanningTwoChunks) {
    tree_fixture<small_geom> f;
    constexpr auto cp = config::chunk_pages;
    f.tree.mark_alloc(0, small_geom::total_pages);
    // free a run straddling the boundary between chunk 3 and chunk 4
    f.tree.mark_free(4 * cp - 10, 20);
    f.tree.flush_free();
    EXPECT_EQ(f.tree.find(20), 4 * cp - 10);
    EXPECT_FALSE(f.tree.find(21).has_value());
}

TEST(RadixTreeTest, FindLocatesRunSpanningATopLevelBoundary) {
    tree_fixture<small_geom> f;
    // The cross-parent case: neither level-0 entry's max() covers this run, only the
    // carried end()+start() across the two entries does.
    constexpr auto mid = small_geom::level_pages[0];
    f.tree.mark_alloc(0, small_geom::total_pages);
    f.tree.mark_free(mid - 30, 60);
    f.tree.flush_free();
    EXPECT_EQ(f.tree.find(60), mid - 30);
    EXPECT_FALSE(f.tree.find(61).has_value());
}

TEST(RadixTreeTest, AllocThenFreeRestoresAWhollyFreeTree) {
    tree_fixture<small_geom> f;
    std::vector<summary> before(f.sums);

    f.tree.mark_alloc(1234, 5678);
    EXPECT_NE(f.tree.find(small_geom::total_pages), 0u);
    f.tree.mark_free(1234, 5678);
    f.tree.flush_free();

    EXPECT_EQ(f.sums, before);
    EXPECT_EQ(f.tree.find(small_geom::total_pages), 0u);
}

TEST(RadixTreeTest, ExhaustionThenFreeResumesAllocation) {
    tree_fixture<small_geom> f;
    f.tree.mark_alloc(0, small_geom::total_pages);
    EXPECT_FALSE(f.tree.find(1).has_value());
    f.tree.mark_free(9999, 1);
    f.tree.flush_free();
    EXPECT_EQ(f.tree.find(1), 9999u);
}

// --- find_in_chunk: the bottom level, answered straight from the bitmap --------------------

TEST(RadixTreeTest, FindInChunkFullyFreeAndFull) {
    tree_fixture<small_geom> f;
    constexpr auto cp = config::chunk_pages;
    // Wholly free chunk: first fit lands at the chunk's first page.
    EXPECT_EQ(fic(f.tree, 1, 10), 1 * cp);
    EXPECT_EQ(fic(f.tree, 1, cp), 1 * cp);

    // Wholly allocated chunk: nothing fits.
    f.tree.mark_alloc(2 * cp, cp);
    EXPECT_FALSE(fic(f.tree, 2, 1).has_value());
}

TEST(RadixTreeTest, FindInChunkExactAndTooLong) {
    tree_fixture<small_geom> f;
    constexpr auto cp = config::chunk_pages;
    const std::size_t base = cp;  // chunk 1
    // Allocate all but a 5-page hole at base+100.
    f.tree.mark_alloc(base, cp);
    f.tree.mark_free(base + 100, 5);
    f.tree.flush_free();

    EXPECT_EQ(fic(f.tree, 1, 5), base + 100);        // exact fit
    EXPECT_FALSE(fic(f.tree, 1, 6).has_value());     // one too long
}

TEST(RadixTreeTest, FindInChunkCrossingWordsButInsideChunk) {
    tree_fixture<small_geom> f;
    constexpr auto cp = config::chunk_pages;
    const std::size_t base = cp;
    f.tree.mark_alloc(base, cp);
    // Free run straddling a 64-bit word boundary (60..79), fully inside the chunk.
    f.tree.mark_free(base + 60, 20);
    f.tree.flush_free();
    EXPECT_EQ(fic(f.tree, 1, 20), base + 60);
    EXPECT_FALSE(fic(f.tree, 1, 21).has_value());
}

TEST(RadixTreeTest, FindInChunkDoesNotCrossChunkBoundary) {
    tree_fixture<small_geom> f;
    constexpr auto cp = config::chunk_pages;
    // Fill all of chunk 0 except its last 10 pages; chunk 1 is entirely free. A run of
    // 30 cannot lie wholly inside chunk 0 (only 10 free there), so within chunk 0 it must
    // report nothing even though 10 free + chunk 1 free would be contiguous - joining them
    // is the tree's job a level up, not find_in_chunk's.
    f.tree.mark_alloc(0, cp - 10);
    EXPECT_FALSE(fic(f.tree, 0, 30).has_value());
    EXPECT_EQ(fic(f.tree, 0, 10), cp - 10);   // fits at the tail
    // The cross-boundary run is chunk 1's to report, starting at its base.
    EXPECT_EQ(fic(f.tree, 1, 30), cp);
}

// --- find_aligned --------------------------------------------------------------------------

TEST(RadixTreeTest, FindAlignedBasic) {
    tree_fixture<small_geom> f;
    EXPECT_EQ(f.tree.find_aligned(8, 8), 0u);
    f.tree.mark_alloc(0, 1);                 // page 0 taken -> aligned group 0 unusable
    EXPECT_EQ(f.tree.find_aligned(8, 8), 8u);
    f.tree.mark_alloc(8, 1);
    EXPECT_EQ(f.tree.find_aligned(8, 8), 16u);
    f.tree.mark_free(0, 1);
    f.tree.mark_free(8, 1);
    f.tree.flush_free();
    EXPECT_EQ(f.tree.find_aligned(8, 8), 0u);
}

TEST(RadixTreeTest, FindAlignedResultIsAlignedAndFree) {
    tree_fixture<small_geom> f;
    std::mt19937 rng(1);
    for (int i = 0; i < 20000; ++i) f.tree.mark_alloc(rng() % small_geom::total_pages, 1);
    for (std::size_t a : { 2u, 4u, 8u, 16u, 64u }) {
        auto r = f.tree.find_aligned(a, a);
        if (!r) continue;
        EXPECT_EQ(*r % a, 0u) << "align=" << a;
        EXPECT_FALSE(f.tree.bits().any_set(*r, a)) << "align=" << a << " run not free";
    }
}

TEST(RadixTreeTest, FindAlignedRejectsBadArgs) {
    tree_fixture<small_geom> f;
    EXPECT_FALSE(f.tree.find_aligned(0, 8).has_value());     // n == 0
    EXPECT_FALSE(f.tree.find_aligned(9, 8).has_value());     // n > align
    EXPECT_FALSE(f.tree.find_aligned(8, 128).has_value());   // align > 64
    EXPECT_FALSE(f.tree.find_aligned(3, 6).has_value());     // align not a power of two
}

template<typename Geom>
static void differential_aligned(unsigned seed, int ops) {
    tree_fixture<Geom> f;
    reference ref(Geom::total_pages);
    std::mt19937 rng(seed);

    struct live { std::size_t start, n; };
    std::vector<live> live_runs;

    for (int op = 0; op < ops; ++op) {
        bool do_free = !live_runs.empty() && (rng() % 100) < 40;
        if (do_free) {
            auto i = rng() % live_runs.size();
            auto r = live_runs[i];
            live_runs[i] = live_runs.back();
            live_runs.pop_back();
            f.tree.mark_free(r.start, r.n);
            f.tree.flush_free();
            ref.set(r.start, r.n, 0);
        } else {
            std::size_t align = 1u << (1 + rng() % 4);   // 2, 4, 8, 16
            std::size_t n = 1 + rng() % align;           // n <= align
            auto got = f.tree.find_aligned(n, align);
            auto want = ref.find_aligned(n, align);
            ASSERT_EQ(got, want) << "op=" << op << " n=" << n << " align=" << align;
            if (!got) continue;
            f.tree.mark_alloc(*got, n);
            ref.set(*got, n, 1);
            live_runs.push_back({ *got, n });
        }
    }
}

TEST(RadixTreeTest, DifferentialAlignedAgainstNaive) {
    for (unsigned seed = 0; seed < 5; ++seed) differential_aligned<small_geom>(seed, 4000);
}

TEST(RadixTreeTest, DifferentialAlignedOnFullDepthGeometry) {
    differential_aligned<big_geom>(11, 800);
}

// --- the one that actually catches summary propagation bugs --------------------------------

template<typename Geom>
static void differential(unsigned seed, int ops, std::size_t max_run) {
    tree_fixture<Geom> f;
    reference ref(Geom::total_pages);
    std::mt19937 rng(seed);

    struct live { std::size_t start, n; };
    std::vector<live> live_runs;

    for (int op = 0; op < ops; ++op) {
        bool do_free = !live_runs.empty() && (rng() % 100) < 40;
        if (do_free) {
            auto i = rng() % live_runs.size();
            auto r = live_runs[i];
            live_runs[i] = live_runs.back();
            live_runs.pop_back();
            f.tree.mark_free(r.start, r.n);
            f.tree.flush_free();
            ref.set(r.start, r.n, 0);
        } else {
            std::size_t n = 1 + rng() % max_run;
            auto got = f.tree.find(n);
            auto want = ref.find(n);
            ASSERT_EQ(got, want) << "op=" << op << " n=" << n;
            if (!got) continue;
            f.tree.mark_alloc(*got, n);
            ref.set(*got, n, 1);
            live_runs.push_back({ *got, n });
        }
    }
}

TEST(RadixTreeTest, DifferentialAgainstNaiveFirstFitSmallRuns) {
    for (unsigned seed = 0; seed < 4; ++seed) differential<small_geom>(seed, 3000, 8);
}

TEST(RadixTreeTest, DifferentialAgainstNaiveFirstFitMixedRuns) {
    // runs long enough to span chunks and force multi-chunk resummarisation
    for (unsigned seed = 0; seed < 4; ++seed) differential<small_geom>(seed + 100, 2000, 2000);
}

TEST(RadixTreeTest, DifferentialOnFullDepthGeometry) {
    // 5 levels, so the descend path and the cross-parent check run at every depth
    differential<big_geom>(7, 400, 5000);
}
