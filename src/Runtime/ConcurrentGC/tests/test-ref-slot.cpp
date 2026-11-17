#include <gtest/gtest.h>
#include <atomic>
#include <thread>
#include "../ref-slot.h"
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
    store_raw(word, 1, std::memory_order_release);
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

// Nothing flips the color yet, so a bad one is a bug and the slow path stops the program
TEST_F(colored, BadColorTakesTheSlowPath) {
    node target;
    managed* ptr = (managed*)((size_t)&target | config::color_bit);
    EXPECT_DEATH((void)read_ref(ptr), "bad pointer color");
}
