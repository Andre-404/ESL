#include <gtest/gtest.h>

#include <atomic>
#include <thread>
#include <unordered_set>
#include <vector>

#include "../allocator/pg-allocator.h"

using namespace gc;
using namespace gc::detail;

namespace {
    // A span_allocator over a reservation of its own. heap_layout is a base rather than a
    // member so it is constructed before the allocator and torn down after it
    struct owned_spans : private heap_layout, public span_allocator {
        owned_spans() : span_allocator(heap()) {}
    };

    // Touches the first and last byte of every page in the run. Catches a commit that
    // covered only part of what was handed out.
    void touch_all(void* p, std::size_t npages) {
        auto* b = (volatile uint8_t*)p;
        for (std::size_t i = 0; i < npages; ++i) {
            b[i * config::page_sz] = 0xAB;
            b[i * config::page_sz + config::page_sz - 1] = 0xCD;
        }
    }

    std::size_t page_idx(span_allocator& a, const void* p) {
        return std::size_t((const uint8_t*)p - a.base()) / config::page_sz;
    }

    // free() is private now; a free goes through a batch that resummarizes the radix tree and
    // advances the scavenge cursor when it is destroyed. Scoping the batch inside these helpers
    // keeps every call site a one liner and drops the lock before the next alloc.
    void free_run(span_allocator& pa, void* p, std::size_t npages) {
        auto b = pa.begin_free();
        b.add(p, npages);
    }

    template<class Runs>
    void free_runs(span_allocator& pa, const Runs& runs) {
        auto b = pa.begin_free();
        for (auto& [p, n] : runs) b.add(p, n);
    }
}

TEST(SpanAllocatorTest, AllocReturnsPageAlignedMemory) {
    owned_spans pa;
    for (std::size_t n : { 1u, 2u, 7u, 64u }) {
        auto* p = pa.alloc(n);
        ASSERT_NE(p, nullptr) << "n=" << n;
        EXPECT_EQ(uintptr_t(p) % config::page_sz, 0u) << "n=" << n;
        
        free_run(pa, p, n);
    }
}

TEST(SpanAllocatorTest, AllocatedRunIsWritableEndToEnd) {
    owned_spans pa;
    // 200 pages crosses several 8 page commit granules, so this also covers the rounding
    constexpr std::size_t n = 200;
    auto* p = pa.alloc(n);
    ASSERT_NE(p, nullptr);
    touch_all(p, n);
    EXPECT_EQ(((uint8_t*)p)[n * config::page_sz - 1], 0xCD);
    free_run(pa, p, n);
}

TEST(SpanAllocatorTest, AllocAlignedRunIsAlignedAndWritable) {
    owned_spans pa;
    std::unordered_set<std::size_t> seen;
    std::vector<std::pair<void*, std::size_t>> runs;

    // (npages, align): align a power of two <= 64, npages <= align.
    for (auto [n, align] : { std::pair<std::size_t, std::size_t>{ 1, 8 },
                             { 8, 8 }, { 3, 4 }, { 1, 64 }, { 16, 16 } }) {
        auto* p = (uint8_t*)pa.alloc_aligned(n, align);
        ASSERT_NE(p, nullptr) << "n=" << n << " align=" << align;
        auto off_pages = std::size_t(p - pa.base()) / config::page_sz;
        EXPECT_EQ(off_pages % align, 0u) << "n=" << n << " align=" << align << " not aligned";
        for (std::size_t i = 0; i < n; ++i)
            EXPECT_TRUE(seen.insert(off_pages + i).second) << "page handed out twice";
        touch_all(p, n);
        runs.push_back({ p, n });
    }
    free_runs(pa, runs);

    // Invalid arguments give nullptr, same as an out-of-space request.
    EXPECT_EQ(pa.alloc_aligned(9, 8), nullptr);   // n > align
    EXPECT_EQ(pa.alloc_aligned(0, 8), nullptr);   // n == 0
    EXPECT_EQ(pa.alloc_aligned(3, 6), nullptr);   // align not a power of two
}

TEST(SpanAllocatorTest, MultiPageAllocationIsContiguous) {
    owned_spans pa;
    constexpr std::size_t n = 17;
    auto* p = (uint8_t*)pa.alloc(n);
    ASSERT_NE(p, nullptr);
    // every page of the run maps to a distinct, consecutive page index
    for (std::size_t i = 0; i < n; ++i) EXPECT_EQ(page_idx(pa, p + i * config::page_sz), page_idx(pa, p) + i);
    free_run(pa, p, n);
}

TEST(SpanAllocatorTest, ConcurrentAllocationsDoNotOverlap) {
    owned_spans pa;
    std::vector<std::pair<uint8_t*, std::size_t>> runs;
    std::unordered_set<std::size_t> seen;

    for (std::size_t n : { 1u, 3u, 1u, 40u, 2u, 9u }) {
        auto* p = (uint8_t*)pa.alloc(n);
        ASSERT_NE(p, nullptr);
        for (std::size_t i = 0; i < n; ++i)
            EXPECT_TRUE(seen.insert(page_idx(pa, p) + i).second) << "page handed out twice";
        runs.push_back({ p, n });
    }
    free_runs(pa, runs);
}

TEST(SpanAllocatorTest, FreeThenReallocReusesTheRun) {
    owned_spans pa;
    auto* first = (uint8_t*)pa.alloc(4);
    ASSERT_NE(first, nullptr);
    memset(first, 0xFF, 4 * config::page_sz);
    free_run(pa, first, 4);

    // The run is deliberately not re-zeroed; the caller zeroes what it needs. First fit hands
    // the same run back, still holding the old occupant.
    auto* again = (uint8_t*)pa.alloc(4);
    ASSERT_EQ(again, first) << "first fit should hand the same run back";
    free_run(pa, again, 4);
}

TEST(SpanAllocatorTest, NeverHandedOutRunIsOsZeroed) {
    owned_spans pa;
    auto* p = (uint8_t*)pa.alloc(32);
    ASSERT_NE(p, nullptr);
    // A run the OS has never handed out is fresh anonymous memory, so it reads as zero even
    // though alloc makes no zeroing promise.
    for (std::size_t i = 0; i < 32 * config::page_sz; ++i) ASSERT_EQ(p[i], 0u) << "offset " << i;
    free_run(pa, p, 32);
}

// Offering a run to the kernel then reusing it must stay correct: the run comes back at the
// same address and is writable end to end. RSS is not asserted - MADV_FREE is lazy and the
// kernel reclaims on its own schedule.
//
// Runs a real scavenger thread rather than calling scavenge() directly, with a policy that makes
// it scavenge eagerly:
// min_retain 0 so nothing is held back, no headroom over the demand peak, and a decay fast
// enough that the peak collapses to the (now zero) demand within a wake or two.
TEST(SpanAllocatorTest, ScavengedRunIsReusable) {
    owned_spans pa;
    scavenger s { pa, scavenge_policy{
        .headroom      = 0.0,
        .thrash_gain   = 0.0,
        .decay_per_sec = 1000.0,
        .min_retain    = 0,
        .idle_sleep    = std::chrono::milliseconds(1),
    } };
    constexpr std::size_t n = 64;
    auto* first = (uint8_t*)pa.alloc(n);
    ASSERT_NE(first, nullptr);
    touch_all(first, n);
    free_run(pa, first, n);

    // Poll rather than sleep a fixed amount: the scavenger releases a granule or so per wake,
    // and a loaded machine can stretch that out well past any constant we would pick here.
    auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
    while (pa.resident() > 0 && std::chrono::steady_clock::now() < deadline)
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    EXPECT_EQ(pa.resident(), 0u) << "the freed run should have been handed back to the kernel";

    auto* again = (uint8_t*)pa.alloc(n);
    ASSERT_EQ(again, first) << "first fit should hand the scavenged run back";
    touch_all(again, n);
    EXPECT_EQ(again[n * config::page_sz - 1], 0xCD);
    free_run(pa, again, n);
}

// A reused page is handed a freshly constructed header and a fresh pair of bitmaps, so nothing
// the previous occupant left behind can read back as live or allocated.
TEST(PgAllocatorTest, RecycledPageGetsACleanPgMetaHeader) {
    pg_allocator pa;
    auto* pg = pa.alloc_pg(64, config::small_obj_pgs, true);
    ASSERT_NE(pg, nullptr);
    pa.free_pgs(pg);
    // Dirty it only after freeing: while it is live the header holds the list link that
    // free_pgs walks. Freeing leaves the header mapped, so this is a legal write.
    memset((uint8_t*)pg, 0xFF, sizeof(pg_meta));

    auto* again = pa.alloc_pg(64, config::small_obj_pgs, true);
    ASSERT_EQ(again, pg) << "expected the same page back";
    EXPECT_EQ(again->block_sz(), 64u);
    EXPECT_EQ(again->compute_live(), 0u);
    // Both bitmaps must read clean, or a recycled page would look fully allocated.
    for (uint16_t i = 0; i < again->block_cnt(); ++i)
        ASSERT_EQ(again->load_alloc_word(i) & (1ull << (i % 64)), 0ull) << "slot " << i;
}

TEST(PgAllocatorTest, RelocHdrsOutliveTheirPages) {
    pg_allocator pa;
    auto* pg = pa.alloc_pg(64, config::small_obj_pgs, true);
    ASSERT_NE(pg, nullptr);
    auto idx = std::size_t(pg - pa.meta_base());
    EXPECT_EQ(pa.relocs().get(idx), 0u);
    reloc_unit table[1] {};
    pa.relocs().set(idx, 64, reloc::split_none, table, 0);
    auto entry = pa.relocs().get(idx);
    ASSERT_NE(entry, 0u);

    pa.free_pgs(pg);
    auto* again = pa.alloc_pg(32, config::small_obj_pgs, true);
    ASSERT_EQ(again, pg) << "expected the same page back";
    EXPECT_EQ(pa.relocs().get(idx), entry) << "freeing and reusing a page leaves its entry alone";

    pa.relocs().clear();
    EXPECT_EQ(pa.relocs().get(idx), 0u);
    pa.free_pgs(again);
}

TEST(PgTablesTest, RelocHdrsAreCommittedWithHeadersAndClearedTogether) {
    heap_layout layout;
    reloc_hdrs r { layout.relocs(), layout.reloc_units() };
    pg_tables t { layout.hdrs(), r };
    // far is under a different summary word than the run at 0
    constexpr std::size_t n = 200, far = (1u << 20) - 1;
    ASSERT_TRUE(t.ensure(0, n));
    ASSERT_TRUE(t.ensure(far, 1));
    for (auto pg : { std::size_t(0), n - 1, far }) ASSERT_EQ(r.get(pg), 0u) << "page " << pg;

    reloc_unit table[1] {};
    for (auto pg : { std::size_t(0), std::size_t(1), n - 1, far }) r.set(pg, 16, reloc::split_none, table, 0); // 1 is in 0's run
    EXPECT_NE(r.get(1), 0u);

    r.clear();
    for (auto pg : { std::size_t(0), std::size_t(1), n - 1, far }) EXPECT_EQ(r.get(pg), 0u) << "page " << pg;
    r.clear();
}

TEST(PgAllocatorTest, RelocTablesForwardAndAreReusedAfterClear) {
    pg_allocator pa;
    auto* pg = pa.alloc_pg(16, config::small_obj_pgs, true);
    ASSERT_NE(pg, nullptr);
    auto idx = std::size_t(pg - pa.meta_base());
    auto data = pg->get_data();

    // The page's only survivors are in its last unit, the first table in the arena is stored
    // that far below where it starts
    constexpr std::size_t pos = reloc::split_none - 2;
    auto first = reloc::unit_of(pos, reloc::split_none);
    reloc_unit table[reloc::pg_units] {};
    table[first] = { 0b11ull << (pos % 64), data + 4096 };

    auto check = [&] {
        EXPECT_EQ(reloc::forward(data + pos * 16), data + 4096);
        EXPECT_EQ(reloc::forward(data + (pos + 1) * 16), data + 4096 + 16);
    };
    pa.relocs().set(idx, 16, reloc::split_none, table, first);
    check();
    auto entry = pa.relocs().get(idx);
    EXPECT_EQ(reloc::forward(data + config::page_sz), data + config::page_sz) << "the next page has no entry";

    pa.relocs().clear();
    EXPECT_EQ(reloc::forward(data + pos * 16), data + pos * 16);
    pa.relocs().set(idx, 16, reloc::split_none, table, first);
    check();
    EXPECT_EQ(pa.relocs().get(idx), entry) << "clear rewinds the arena";

    pa.relocs().clear();
    pa.free_pgs(pg);
}

TEST(SpanAllocatorTest, ImpossibleRequestsFail) {
    owned_spans pa;
    EXPECT_EQ(pa.alloc(0), nullptr);
    EXPECT_EQ(pa.alloc(config::total_pages + 1), nullptr);
}

TEST(SpanAllocatorTest, FreeingEverythingReturnsTheHeapToTheStart) {
    owned_spans pa;
    std::vector<std::pair<void*, std::size_t>> runs;
    for (int i = 0; i < 50; ++i) {
        auto n = std::size_t(1 + i % 7);
        auto* p = pa.alloc(n);
        ASSERT_NE(p, nullptr);
        runs.push_back({ p, n });
    }
    free_runs(pa, runs);

    // with the whole heap free again, first fit must come back to page 0
    auto* p = pa.alloc(1);
    EXPECT_EQ(p, pa.base());
    free_run(pa, p, 1);
}

TEST(SpanAllocatorTest, ConcurrentAllocFreeIsSafe) {
    owned_spans pa;
    constexpr int threads = 8, iters = 400;
    std::atomic<int> failures{ 0 };

    std::vector<std::thread> ts;
    for (int t = 0; t < threads; ++t) {
        ts.emplace_back([&, t] {
            for (int i = 0; i < iters; ++i) {
                std::size_t n = 1 + (t + i) % 5;
                auto* p = (uint8_t*)pa.alloc(n);
                if (!p) { ++failures; continue; }
                // write through the whole run, so overlapping handouts corrupt each other
                memset(p, t + 1, n * config::page_sz);
                for (std::size_t k = 0; k < n * config::page_sz; k += config::page_sz)
                    if (p[k] != uint8_t(t + 1)) { ++failures; break; }
                free_run(pa, p, n);
            }
        });
    }
    for (auto& th : ts) th.join();
    EXPECT_EQ(failures.load(), 0);

    auto* p = pa.alloc(1);
    EXPECT_EQ(p, pa.base()) << "every run was freed, so the heap should be empty again";
    free_run(pa, p, 1);
}

// ---------------------------------------------------------------------------------------
// Scavenger cursor arithmetic.
//
// scavenge walks granules downward from _scavenge_idx. The loop guard used to be `hi >= 0`,
// so on reaching granule 0 it evaluated can_free(hi - 1) with hi == 0: -1 converted to
// size_t, indexing the committed bitmap at 2^64/64 words past its base. That bitmap is the
// last carve out of a raw mmap, so the read lands far outside the mapping - and ASAN cannot
// see it, because the region has no redzones. It simply faults.
//
// It also used to store `_scavenge_idx = hi` unconditionally, so a wake that planned nothing
// still walked the cursor one granule further up the heap, every time, forever.
// ---------------------------------------------------------------------------------------
namespace gc::detail {
    struct span_allocator_test_peer {
        static std::size_t scavenge(span_allocator& pa, std::size_t n) { return pa.scavenge(n); }
        static int64_t idx(const span_allocator& pa) { return pa._scavenge_idx; }
        static void set_idx(span_allocator& pa, int64_t v) { pa._scavenge_idx = v; }
    };
}

namespace {
    using peer = gc::detail::span_allocator_test_peer;
}

TEST(SpanAllocatorScavengeTest, WalkingDownToGranuleZeroStaysInBounds) {
    owned_spans pa;
    // Hold page 0 so granule 0 is not freeable: the walk has to step past it and reach the
    // bottom, which is exactly where the off-by-one read used to happen.
    auto* held = pa.alloc(1);
    ASSERT_NE(held, nullptr);

    peer::set_idx(pa, 0);
    EXPECT_EQ(peer::scavenge(pa, 64), 0u) << "nothing is freeable, so nothing is released";
    EXPECT_GE(peer::idx(pa), 0) << "the cursor must not go negative";

    free_run(pa, held, 1);
}

TEST(SpanAllocatorScavengeTest, WalkFromAboveDownThroughZeroStaysInBounds) {
    owned_spans pa;
    // Commit a stretch, hand it all back, then sweep from the top with a budget larger than
    // the number of freeable granules so the walk runs off the bottom of the heap.
    constexpr std::size_t n = 256;
    auto* p = pa.alloc(n);
    ASSERT_NE(p, nullptr);
    touch_all(p, n);
    ASSERT_GT(pa.resident(), 0u);
    free_run(pa, p, n);

    EXPECT_GT(peer::scavenge(pa, 1u << 20), 0u);
    EXPECT_EQ(pa.resident(), 0u) << "every granule was free, so all of them go back";
    EXPECT_GE(peer::idx(pa), 0);
}

TEST(SpanAllocatorScavengeTest, PlanningNothingDoesNotDriftTheCursor) {
    owned_spans pa;
    auto* p = pa.alloc(64);
    ASSERT_NE(p, nullptr);
    free_run(pa, p, 64);

    auto before = peer::idx(pa);
    for (int i = 0; i < 100; ++i) EXPECT_EQ(peer::scavenge(pa, 0), 0u);
    EXPECT_EQ(peer::idx(pa), before)
        << "a wake with no budget must leave the cursor where it was, not walk it up the heap";
}

TEST(SpanAllocatorScavengeTest, CursorNeverClimbsAboveTheHighestFreedGranule) {
    owned_spans pa;
    auto* p = pa.alloc(32);
    ASSERT_NE(p, nullptr);
    touch_all(p, 32);
    free_run(pa, p, 32);

    auto highest = peer::idx(pa);
    for (int i = 0; i < 20; ++i) {
        peer::scavenge(pa, 4);
        EXPECT_LE(peer::idx(pa), highest) << "the cursor only ever moves down until a free re-raises it";
        EXPECT_GE(peer::idx(pa), 0);
    }
}

TEST(SpanAllocatorScavengeTest, ScavengeRespectsItsBudget) {
    owned_spans pa;
    constexpr std::size_t n = 256;   // 32 granules at 8 pages each
    auto* p = pa.alloc(n);
    ASSERT_NE(p, nullptr);
    touch_all(p, n);
    auto resident_before = pa.resident();
    free_run(pa, p, n);

    auto released = peer::scavenge(pa, 4);
    EXPECT_LE(released, 4u) << "a budget of four granules must not release more than four";
    EXPECT_EQ(pa.resident(), resident_before - released);
}
