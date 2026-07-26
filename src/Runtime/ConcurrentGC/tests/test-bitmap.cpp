#include <gtest/gtest.h>

#include <random>
#include <vector>

#include "../bitmap.h"

using namespace gc;
using namespace gc::detail;

namespace {
    // Naive model: a flag per page (1 == allocated / bit set). Deliberately the
    // dumbest implementation that is obviously correct, mirrored against the bitmap.
    struct reference {
        std::vector<char> bits;
        explicit reference(std::size_t n) : bits(n, 0) {}

        void set_range(std::size_t start, std::size_t n, char v) {
            for (std::size_t i = 0; i < n; ++i) bits[start + i] = v;
        }
        bool any_set(std::size_t start, std::size_t n) const {
            for (std::size_t i = 0; i < n; ++i) if (bits[start + i]) return true;
            return false;
        }
        bool all_set(std::size_t start, std::size_t n) const {
            for (std::size_t i = 0; i < n; ++i) if (!bits[start + i]) return false;
            return true;
        }
    };

    // A bitmap plus its reference model over the same number of pages, kept in lock
    // step. Backing store starts wholly clear (free).
    struct fixture {
        static constexpr std::size_t chunks = 4;
        static constexpr std::size_t pages = chunks * config::chunk_pages;
        static constexpr std::size_t words = pages / 64;

        std::vector<uint64_t> store;
        bitmap bm;
        reference ref;

        fixture() : store(words, 0), bm(store.data()), ref(pages) {}

        void set_range(std::size_t start, std::size_t n) {
            bm.set_range(start, n);
            ref.set_range(start, n, 1);
        }
        void clear_range(std::size_t start, std::size_t n) {
            bm.clear_range(start, n);
            ref.set_range(start, n, 0);
        }
        // Assert every bit, plus a batch of range queries, agree with the model.
        void expect_consistent() const {
            for (std::size_t i = 0; i < pages; ++i)
                ASSERT_EQ(bm.test(i), bool(ref.bits[i])) << "bit " << i;
        }
    };
}

TEST(Bitmap, SingleBitRoundTrip) {
    std::vector<uint64_t> store(4, 0);
    bitmap bm(store.data());

    EXPECT_FALSE(bm.test(0));
    EXPECT_FALSE(bm.test(63));
    EXPECT_FALSE(bm.test(64));

    bm.set(0);
    bm.set(63);
    bm.set(64);
    bm.set(200);
    EXPECT_TRUE(bm.test(0));
    EXPECT_TRUE(bm.test(63));
    EXPECT_TRUE(bm.test(64));
    EXPECT_TRUE(bm.test(200));
    EXPECT_FALSE(bm.test(1));
    EXPECT_FALSE(bm.test(62));
    EXPECT_FALSE(bm.test(65));

    bm.clear(63);
    EXPECT_FALSE(bm.test(63));
    EXPECT_TRUE(bm.test(64));  // neighbour in the next word untouched
}

TEST(Bitmap, EmptyRangeIsNoOp) {
    fixture f;
    f.set_range(10, 0);
    f.expect_consistent();
    EXPECT_FALSE(f.bm.any_set(10, 0));  // vacuously nothing set
    EXPECT_TRUE(f.bm.all_set(10, 0));   // vacuously all set
}

TEST(Bitmap, SetRangeWithinOneWord) {
    fixture f;
    f.set_range(5, 10);  // bits 5..14 in word 0
    f.expect_consistent();
    EXPECT_FALSE(f.bm.test(4));
    EXPECT_TRUE(f.bm.test(5));
    EXPECT_TRUE(f.bm.test(14));
    EXPECT_FALSE(f.bm.test(15));
}

TEST(Bitmap, SetRangeSingleBit) {
    fixture f;
    f.set_range(70, 1);
    f.expect_consistent();
    EXPECT_TRUE(f.bm.test(70));
    EXPECT_FALSE(f.bm.test(69));
    EXPECT_FALSE(f.bm.test(71));
}

TEST(Bitmap, SetRangeSpansTwoWords) {
    fixture f;
    f.set_range(60, 10);  // bits 60..63 (word 0) + 64..69 (word 1)
    f.expect_consistent();
    EXPECT_FALSE(f.bm.test(59));
    EXPECT_TRUE(f.bm.test(60));
    EXPECT_TRUE(f.bm.test(69));
    EXPECT_FALSE(f.bm.test(70));
}

TEST(Bitmap, SetRangeSpansManyWordsHitsMemsetPath) {
    fixture f;
    f.set_range(40, 200);  // head in word 0, several full middle words, tail past
    f.expect_consistent();
    EXPECT_FALSE(f.bm.test(39));
    EXPECT_TRUE(f.bm.test(40));
    EXPECT_TRUE(f.bm.test(239));
    EXPECT_FALSE(f.bm.test(240));
}

TEST(Bitmap, RangesOnWordBoundaries) {
    fixture f;
    f.set_range(0, 64);    // exactly word 0
    f.set_range(128, 64);  // exactly word 2
    f.expect_consistent();
    EXPECT_TRUE(f.bm.all_set(0, 64));
    EXPECT_TRUE(f.bm.all_set(128, 64));
    EXPECT_FALSE(f.bm.any_set(64, 64));  // word 1 stays clear
}

TEST(Bitmap, ClearRangePunchesHole) {
    fixture f;
    f.set_range(0, fixture::pages);  // fill everything
    f.clear_range(100, 50);
    f.expect_consistent();
    EXPECT_TRUE(f.bm.test(99));
    EXPECT_FALSE(f.bm.any_set(100, 50));
    EXPECT_TRUE(f.bm.test(150));
}

TEST(Bitmap, AnyAndAllSet) {
    fixture f;
    EXPECT_FALSE(f.bm.any_set(0, fixture::pages));
    EXPECT_FALSE(f.bm.all_set(0, 10));

    f.set_range(200, 100);
    EXPECT_TRUE(f.bm.any_set(150, 100));   // overlaps the set region
    EXPECT_FALSE(f.bm.any_set(0, 200));    // strictly before it
    EXPECT_TRUE(f.bm.all_set(200, 100));
    EXPECT_FALSE(f.bm.all_set(199, 100));  // one clear bit at the start
    EXPECT_FALSE(f.bm.all_set(200, 101));  // one clear bit at the end

    // Single set bit made visible across a multi-word range.
    fixture g;
    g.set_range(150, 1);
    EXPECT_TRUE(g.bm.any_set(0, fixture::pages));
    EXPECT_FALSE(g.bm.all_set(0, fixture::pages));
}

TEST(Bitmap, RandomizedAgainstReference) {
    fixture f;
    std::mt19937_64 rng(0xC0FFEE);
    std::uniform_int_distribution<std::size_t> page(0, fixture::pages - 1);
    std::uniform_int_distribution<int> coin(0, 1);

    auto rand_range = [&] {
        std::size_t start = page(rng);
        std::size_t max_n = fixture::pages - start;
        std::size_t n = std::uniform_int_distribution<std::size_t>(0, max_n)(rng);
        return std::pair{start, n};
    };

    for (int iter = 0; iter < 4000; ++iter) {
        auto [start, n] = rand_range();
        if (coin(rng)) f.set_range(start, n); else f.clear_range(start, n);

        f.expect_consistent();

        // Range queries against the model.
        auto [qs, qn] = rand_range();
        EXPECT_EQ(f.bm.any_set(qs, qn), f.ref.any_set(qs, qn)) << "any_set " << qs << " " << qn;
        EXPECT_EQ(f.bm.all_set(qs, qn), f.ref.all_set(qs, qn)) << "all_set " << qs << " " << qn;
    }
}
