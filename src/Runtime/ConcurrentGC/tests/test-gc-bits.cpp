#include <gtest/gtest.h>

#include <atomic>
#include <barrier>
#include <thread>
#include <vector>
#include <algorithm>

#include "../gc-bits.h"
#include "../os-memory.h"

using namespace gc;
using namespace gc::detail;

namespace {
    // gc_bits does not own its storage any more: the page allocator carves a base out of the
    // heap reservation and hands it over, so the tests have to provide one. The reservation is
    // a private base rather than a member so that it - and therefore the address - is
    // constructed before the gc_bits subobject it is passed to, and torn down after it.
    struct reservation {
        uint8_t* base;
        reservation() : base((uint8_t*)os::reserve(config::bits_region_sz, config::commit_syscall_sz)) {}
        ~reservation() { os::release(base, config::bits_region_sz); }
        reservation(const reservation&) = delete;
        reservation& operator=(const reservation&) = delete;
    };

    // A gc_bits with a reservation of its own, so a test can keep saying `gc_bits {}` in spirit
    // and still get an arena that dies with it.
    struct owned_bits : private reservation, public gc_bits {
        owned_bits() : gc_bits(base) {}
    };

    // The contract callers rely on: a bitmap of `bits` bits is read and written a word at a
    // time through atomic_ref<uint64_t>, so a slot must be
    //   - big enough for words_for(bits) whole 64 bit words,
    //   - 8 aligned, or the atomic_ref is UB and quietly not atomic,
    //   - zeroed,
    //   - disjoint from every other outstanding slot.
    // Everything below is written in terms of this rather than gc_bits' own arithmetic, so a
    // change to how it sizes slots is checked against the requirement instead of restating it.
    constexpr std::size_t words_for(std::size_t bits) { return (bits + 63) / 64; }
    constexpr std::size_t bytes_for(std::size_t bits) { return words_for(bits) * sizeof(uint64_t); }

    // Bit counts a 64 KiB run actually produces, plus the awkward edges around a word.
    // 4096 is a run of 16 byte blocks, 25 a run of 2560 byte blocks.
    constexpr std::size_t bit_counts[] = { 1, 25, 64, 65, 127, 128, 511, 512, 2048, 4096 };

    struct slot {
        uint64_t* p;
        std::size_t words;
        uint64_t tag;
    };

    slot claim(gc_bits& b, std::size_t bits, uint64_t tag, bool mark) {
        auto s = mark ? b.mark_bits(bits) : b.alloc_bits(bits);
        return { s.data(), s.size(), tag };
    }

    // Writes the whole word span, which is what a real bitmap does as soon as a block in its
    // last word is marked - so an undersized slot shows up as damage to its neighbour.
    void stamp(const slot& s) {
        for (std::size_t i = 0; i < s.words; ++i)
            std::atomic_ref { s.p[i] }.store(s.tag, std::memory_order_relaxed);
    }

    ::testing::AssertionResult intact(const slot& s) {
        for (std::size_t i = 0; i < s.words; ++i) {
            auto got = std::atomic_ref { s.p[i] }.load(std::memory_order_relaxed);
            if (got != s.tag)
                return ::testing::AssertionFailure()
                    << "slot " << (void*)s.p << " (" << s.words << " words) word " << i
                    << ": wrote 0x" << std::hex << s.tag << " read back 0x" << got << std::dec
                    << " - another slot overlaps this one";
        }
        return ::testing::AssertionSuccess();
    }

    ::testing::AssertionResult zeroed(const slot& s) {
        for (std::size_t i = 0; i < s.words; ++i)
            if (s.p[i] != 0)
                return ::testing::AssertionFailure()
                    << "slot " << (void*)s.p << " word " << i << " of " << s.words
                    << " was not zero on hand-out";
        return ::testing::AssertionSuccess();
    }
}

// ---------------------------------------------------------------------------
// Geometry. A slot has to be as large, as aligned and as clean as the caller was
// promised; everything else is built on that.
// ---------------------------------------------------------------------------

TEST(GcBitsTest, ConsecutiveSlotsAreAtLeastAsFarApartAsTheyAreLarge) {
    for (auto bits : bit_counts) {
        auto b = owned_bits {};
        auto a = b.alloc_bits(bits);
        auto c = b.alloc_bits(bits);
        ASSERT_FALSE(a.empty());
        ASSERT_FALSE(c.empty());
        auto apart = std::size_t((uint8_t*)c.data() - (uint8_t*)a.data());
        EXPECT_GE(apart, bytes_for(bits))
            << "bits=" << bits << ": slots are " << apart << " bytes apart but each needs "
            << bytes_for(bits) << " (" << words_for(bits) << " words)";
    }
}

TEST(GcBitsTest, SlotsAreEightAligned) {
    auto b = owned_bits {};
    for (auto bits : bit_counts) {
        auto p = b.alloc_bits(bits);
        ASSERT_FALSE(p.empty());
        EXPECT_EQ(uintptr_t(p.data()) % alignof(uint64_t), 0u) << "bits=" << bits;
    }
}

TEST(GcBitsTest, WritingOneSlotDoesNotDisturbTheNext) {
    auto b = owned_bits {};
    constexpr std::size_t bits = 4096;

    auto first = claim(b, bits, 0xAAAAAAAAAAAAAAAAull, false);
    auto second = claim(b, bits, 0xBBBBBBBBBBBBBBBBull, false);
    ASSERT_NE(first.p, nullptr);
    ASSERT_NE(second.p, nullptr);

    stamp(second);
    stamp(first);
    EXPECT_TRUE(intact(second)) << "the first slot ran over the second";
}

TEST(GcBitsTest, ZeroSizedRequestIsRejected) {
    auto b = owned_bits {};
    EXPECT_TRUE(b.alloc_bits(0).empty());
    EXPECT_TRUE(b.mark_bits(0).empty());
}

// ---------------------------------------------------------------------------
// Halves and flipping.
// ---------------------------------------------------------------------------

// The two roles must never share storage: the mark bitmap being filled this cycle is what
// becomes the alloc bitmap at the flip, so an overlap would corrupt live allocation state.
TEST(GcBitsTest, AllocAndMarkComeFromDifferentHalves) {
    auto b = owned_bits {};
    auto a = (uint8_t*)b.alloc_bits(4096).data();
    auto m = (uint8_t*)b.mark_bits(4096).data();
    ASSERT_NE(a, nullptr);
    ASSERT_NE(m, nullptr);

    auto gap = std::size_t(std::max(a, m) - std::min(a, m));
    EXPECT_GE(gap, config::bits_arena_sz - bytes_for(4096));
}

TEST(GcBitsTest, FlipResetsTheOutgoingHalfAndSwapsRoles) {
    auto b = owned_bits {};
    constexpr std::size_t bits = 512;   // 64 bytes

    auto a0 = b.alloc_bits(bits).data();   // live half, offset 0
    auto m0 = b.mark_bits(bits).data();    // spare half, offset 0
    ASSERT_NE(a0, nullptr);
    ASSERT_NE(m0, nullptr);

    b.flip();

    // The old live half held nothing but dead alloc bitmaps, so it rewinds and is now where
    // mark bitmaps land.
    EXPECT_EQ(b.mark_bits(bits).data(), a0);
    // The old mark bitmaps are the alloc bitmaps now, so allocating past them must not reuse
    // their storage.
    EXPECT_EQ(b.alloc_bits(bits).data(), m0 + words_for(bits));
}

TEST(GcBitsTest, TwoFlipsReturnToTheOriginalHalf) {
    auto b = owned_bits {};
    auto a0 = b.alloc_bits(512).data();
    ASSERT_NE(a0, nullptr);

    b.flip();
    b.flip();

    EXPECT_EQ(b.alloc_bits(512).data(), a0);
}

// Storage a flip recycles must come back clean across its whole word span - a short memset
// would leave a previous cycle's mark bits looking like live objects.
TEST(GcBitsTest, RecycledStorageComesBackFullyZeroed) {
    for (auto bits : bit_counts) {
        auto b = owned_bits {};

        auto first = claim(b, bits, ~uint64_t(0), false);
        ASSERT_NE(first.p, nullptr) << "bits=" << bits;
        stamp(first);

        b.flip();

        auto reused = claim(b, bits, 0, true);
        ASSERT_EQ(reused.p, first.p) << "bits=" << bits << ": expected the rewound half to hand"
                                        " the same bytes back";
        EXPECT_TRUE(zeroed(reused)) << "bits=" << bits;
    }
}

// ---------------------------------------------------------------------------
// Exhaustion and accounting.
// ---------------------------------------------------------------------------

TEST(GcBitsTest, ExhaustionReturnsNullUntilTheNextFlip) {
    auto b = owned_bits {};
    // capacity() is bytes, so this is one word more than a half can hold.
    constexpr std::size_t too_many = gc_bits::capacity() * 8 + 64;

    EXPECT_TRUE(b.alloc_bits(too_many).empty());
    // The cursor is left past the end on purpose, so the half stays closed.
    EXPECT_TRUE(b.alloc_bits(64).empty());
    EXPECT_EQ(b.used(), gc_bits::capacity()) << "used() must clamp the overshoot";

    // A failure in one half leaves the other untouched.
    EXPECT_FALSE(b.mark_bits(64).empty());

    b.flip();
    // The exhausted half was the live one, so the flip rewound it; it is the mark half now.
    EXPECT_FALSE(b.mark_bits(64).empty());
    EXPECT_EQ(b.used(), bytes_for(64));
}

TEST(GcBitsTest, UsedTracksTheFullerHalf) {
    auto b = owned_bits {};
    EXPECT_EQ(b.used(), 0u);

    ASSERT_FALSE(b.alloc_bits(512).empty());
    EXPECT_EQ(b.used(), 64u);

    ASSERT_FALSE(b.mark_bits(4096).empty());
    EXPECT_EQ(b.used(), 512u);

    // A partial word still costs a whole one.
    ASSERT_FALSE(b.alloc_bits(1).empty());
    EXPECT_EQ(b.used(), 512u);

    auto p1 = b.alloc_bits(1).data();
    auto p2 = b.alloc_bits(1).data();
    ASSERT_NE(p1, nullptr);
    EXPECT_EQ(p2, p1 + 1) << "one word per slot, even for a single bit";
}

// ---------------------------------------------------------------------------
// Under contention. Mutators claim alloc bitmaps for new runs while the sweep claims
// mark bitmaps, so both cursors take concurrent bumps.
// ---------------------------------------------------------------------------

// Every thread stamps its slots with a private tag and, once everyone has finished claiming,
// checks its own slots still read that tag back. Any two slots sharing a byte fail one of the
// two checks.
TEST(GcBitsTest, ConcurrentClaimsDoNotOverlap) {
    constexpr int threads = 8;
    constexpr int per_thread = 300;

    auto b = owned_bits {};
    auto out = std::vector<std::vector<slot>>(threads);
    auto sync = std::barrier { threads };
    auto failures = std::atomic<int> { 0 };

    auto ts = std::vector<std::jthread> {};
    for (int t = 0; t < threads; ++t) {
        ts.emplace_back([&, t] {
            auto& mine = out[t];
            mine.reserve(per_thread);
            sync.arrive_and_wait();

            for (int i = 0; i < per_thread; ++i) {
                auto bits = bit_counts[(t * per_thread + i) % std::size(bit_counts)];
                // Alternate halves so both cursors, and both commit paths, see contention.
                auto s = claim(b, bits, (uint64_t(t) << 32) | uint64_t(i) | (1ull << 63),
                               (i & 1) != 0);
                if (!s.p) { failures.fetch_add(1, std::memory_order_relaxed); return; }
                stamp(s);
                mine.push_back(s);
            }

            sync.arrive_and_wait();

            for (auto& s : mine) if (!intact(s)) failures.fetch_add(1, std::memory_order_relaxed);
        });
    }
    ts.clear();

    EXPECT_EQ(failures.load(), 0);
}

// The zeroing half of the contract, while other threads hammer neighbouring addresses.
TEST(GcBitsTest, SlotsAreZeroedWhileOtherThreadsClaim) {
    constexpr int threads = 8;
    constexpr int per_thread = 300;

    auto b = owned_bits {};
    auto sync = std::barrier { threads };
    auto dirty = std::atomic<int> { 0 };

    auto ts = std::vector<std::jthread> {};
    for (int t = 0; t < threads; ++t) {
        ts.emplace_back([&, t] {
            sync.arrive_and_wait();
            for (int i = 0; i < per_thread; ++i) {
                auto s = claim(b, bit_counts[(t + i) % std::size(bit_counts)], ~uint64_t(0),
                               (i & 1) != 0);
                if (!s.p) { dirty.fetch_add(1, std::memory_order_relaxed); return; }
                for (std::size_t w = 0; w < s.words; ++w)
                    if (std::atomic_ref { s.p[w] }.load(std::memory_order_relaxed) != 0)
                        dirty.fetch_add(1, std::memory_order_relaxed);
                stamp(s);
            }
        });
    }
    ts.clear();

    EXPECT_EQ(dirty.load(), 0) << "a slot came back holding a previous tenant's bits";
}

// Cycles: claim concurrently, quiesce, flip, repeat. flip() needs a safepoint, so the threads
// join before it - what is under test is that recycled storage comes back clean and that the
// surviving half never hands the same bytes out twice.
TEST(GcBitsTest, RepeatedFlipsRecycleCleanStorageUnderLoad) {
    constexpr int threads = 8;
    constexpr int per_thread = 200;
    constexpr int cycles = 6;

    auto b = owned_bits {};
    auto problems = std::atomic<int> { 0 };

    for (int c = 0; c < cycles; ++c) {
        auto out = std::vector<std::vector<slot>>(threads);
        auto sync = std::barrier { threads };
        auto ts = std::vector<std::jthread> {};

        for (int t = 0; t < threads; ++t) {
            ts.emplace_back([&, t, c] {
                auto& mine = out[t];
                mine.reserve(per_thread);
                sync.arrive_and_wait();

                for (int i = 0; i < per_thread; ++i) {
                    auto bits = bit_counts[(c + t + i) % std::size(bit_counts)];
                    auto tag = (uint64_t(c) << 48) | (uint64_t(t) << 32) | uint64_t(i) | 1;
                    auto s = claim(b, bits, tag, (i & 1) != 0);
                    if (!s.p) { problems.fetch_add(1, std::memory_order_relaxed); return; }
                    if (!zeroed(s)) problems.fetch_add(1, std::memory_order_relaxed);
                    stamp(s);
                    mine.push_back(s);
                }

                sync.arrive_and_wait();
                for (auto& s : mine)
                    if (!intact(s)) problems.fetch_add(1, std::memory_order_relaxed);
            });
        }
        ts.clear();
        b.flip();
    }

    EXPECT_EQ(problems.load(), 0);
}

// used() is what a pressure trigger reads, so it has to stay sane while both cursors move.
TEST(GcBitsTest, UsedNeverExceedsCapacityUnderLoad) {
    constexpr int threads = 8;
    constexpr int per_thread = 400;

    auto b = owned_bits {};
    auto bad = std::atomic<int> { 0 };
    auto sync = std::barrier { threads };

    auto ts = std::vector<std::jthread> {};
    for (int t = 0; t < threads; ++t) {
        ts.emplace_back([&, t] {
            sync.arrive_and_wait();
            for (int i = 0; i < per_thread; ++i) {
                auto s = claim(b, bit_counts[(t + i) % std::size(bit_counts)], 1, (i & 1) != 0);
                if (!s.p) { bad.fetch_add(1, std::memory_order_relaxed); return; }
                if (b.used() > gc_bits::capacity()) bad.fetch_add(1, std::memory_order_relaxed);
            }
        });
    }
    ts.clear();

    EXPECT_EQ(bad.load(), 0);
    EXPECT_LE(b.used(), gc_bits::capacity());
}
