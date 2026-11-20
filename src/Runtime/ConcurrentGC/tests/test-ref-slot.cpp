#include <gtest/gtest.h>
#include <atomic>
#include <thread>
#include "../gc-api.h"
#include "../platform-specific.h"

using namespace gc;

namespace {

    struct node : managed {
        node() : managed(0, move_state::none) {}
    };
    struct unmanaged {};

    // A slot holds a word or a pointer to something managed, nothing else gets through
    static_assert(ref_slot<size_t>);
    static_assert(ref_slot<managed*>);
    static_assert(ref_slot<node*>);
    static_assert(!ref_slot<unmanaged*>);
    static_assert(!ref_slot<node>);
    static_assert(!ref_slot<int>);

}

TEST(RefSlotTest, ReadsWhatTheSlotHolds) {
    node target;

    size_t word = 0x7ffe'0000'dead'bee0;
    EXPECT_EQ(load_raw(word), word);
    EXPECT_EQ(load_raw(word, std::memory_order_acquire), word);

    node* ptr = &target;
    EXPECT_EQ(load_raw(ptr), &target);
    EXPECT_EQ(read_ref(ptr), &target);

    ptr = nullptr;
    EXPECT_EQ(load_raw(ptr), nullptr);
    EXPECT_EQ(read_ref(ptr), nullptr);
}

TEST(RefSlotTest, StoresWhatItIsGiven) {
    node target;

    size_t word = 0;
    store_raw(word, 0x7ffe'0000'dead'bee0);
    EXPECT_EQ(word, 0x7ffe'0000'dead'bee0);
    store_raw(word, 1);
    EXPECT_EQ(word, 1u);

    // The value converts to what the slot holds, so a derived pointer or nullptr needs no cast
    managed* ptr = nullptr;
    store_ref(ptr, &target);
    EXPECT_EQ(ptr, &target);
    store_raw(ptr, nullptr);
    EXPECT_EQ(ptr, nullptr);
}

// What the collector relies on: whatever a reader gets out of a slot is a value some store put
// there, never a mix of two. Passes trivially on x86, but under tsan it is what checks that the
// reads and the stores really are atomic
TEST(RefSlotTest, ConcurrentReadersOnlySeeStoredValues) {
    // Pointer slots, so neither value may carry the color bit
    auto a = (managed*)0x0000'0000'ffff'fff0;
    auto b = (managed*)0x0000'7ffe'0000'0000;
    managed* slot = a;
    std::atomic<bool> done { false };

    std::thread writer([&] {
        for (int i = 0; i < 200'000; i++) {
            if (i % 4 < 2) store_ref(slot, i % 2 ? a : b);
            else store_raw(slot, i % 2 ? a : b);
        }
        done.store(true, std::memory_order_release);
    });

    size_t torn = 0;
    while (!done.load(std::memory_order_acquire)) {
        auto collector = load_raw(slot, std::memory_order_acquire);
        auto mutator = read_ref(slot);
        torn += (collector != a && collector != b) + (mutator != a && mutator != b);
    }
    writer.join();
    EXPECT_EQ(torn, 0u);
}

namespace {
    // r14 holds the good color, a thread local under UNIT_TESTS
    struct colored : ::testing::Test {
        void TearDown() override { detail::platform::write_color(0); }
    };
}

TEST_F(colored, StoreAddsTheGoodColorAndReadStripsIt) {
    node target;
    managed* ptr = nullptr;

    detail::platform::write_color(config::color_bit);
    store_ref(ptr, &target);
    EXPECT_EQ((size_t)load_raw(ptr), (size_t)&target | config::color_bit);
    EXPECT_EQ(read_ref(ptr), &target);

    detail::platform::write_color(0);
    store_ref(ptr, &target);
    EXPECT_EQ(load_raw(ptr), &target);
    EXPECT_EQ(read_ref(ptr), &target);
}

TEST_F(colored, NullReadsBackAsNullInEitherColor) {
    node target;
    managed* ptr = &target;

    detail::platform::write_color(config::color_bit);
    store_ref(ptr, nullptr);
    EXPECT_EQ((size_t)load_raw(ptr), config::color_bit);
    EXPECT_EQ(read_ref(ptr), nullptr);

    detail::platform::write_color(0);
    store_ref(ptr, nullptr);
    EXPECT_EQ(load_raw(ptr), nullptr);
    EXPECT_EQ(read_ref(ptr), nullptr);
}

// The slow path: forward what a bad colored slot held and put it back with the good color. Static
// storage is outside the heap, so it forwards to itself
TEST_F(colored, ReadingABadColorHealsTheSlot) {
    node target;
    auto* want = (managed*)&target;

    auto* slot = (managed*)((size_t)want | config::color_bit);
    EXPECT_EQ(read_ref(slot), want);
    EXPECT_EQ(load_raw(slot), want) << "healed with the good color";

    managed* null = (managed*)config::color_bit;
    EXPECT_EQ(read_ref(null), nullptr);
    EXPECT_EQ(load_raw(null), nullptr);

    // A word slot goes through to_accurate_ptr and ptr_to_word, raw pointers under the test hooks
    size_t word = (size_t)want | config::color_bit;
    EXPECT_EQ(heal_word(&word, word), (size_t)want);
    EXPECT_EQ(word, (size_t)want);
}

// Outside the heap is told by address alone: a stale pointer can outlive the run its object moved
// out of, so the heal never reads through it
TEST_F(colored, HealNeverReadsThroughThePointer) {
    auto* unmapped = (managed*)uintptr_t(16);
    auto* slot = (managed*)((size_t)unmapped | config::color_bit);
    EXPECT_EQ(read_ref(slot), unmapped);
}

// A store that lands between the load and the heal wins: the CAS fails and the reader still gets
// what it loaded, fixed
TEST_F(colored, HealLosesToANewerStore) {
    node stale, newer;
    managed* slot = &newer;
    EXPECT_EQ(heal_ptr((size_t*)&slot, (size_t)&stale | config::color_bit), (size_t)&stale);
    EXPECT_EQ(load_raw(slot), (managed*)&newer);
}
