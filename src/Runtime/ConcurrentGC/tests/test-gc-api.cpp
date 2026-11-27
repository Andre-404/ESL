#include <gtest/gtest.h>

#include "../gc-api.h"
#include "customization-helper.h"

using namespace gc;

namespace {
    struct obj : managed {
        explicit obj(uint8_t type) : managed(type) {}
    };

    class GcApiTest : public ::testing::Test {
    protected:
        void SetUp() override   { detail::test_custom::hooks.reset(); }
        void TearDown() override { detail::test_custom::hooks.reset(); }
    };
}

// A mark running at allocation time has already set the object's bit, so it never asks
// obj_is_pinned. make_gc pins the page itself once the type byte is there
TEST_F(GcApiTest, MakeGcPinsAPinnedTypeAsSoonAsItIsBuilt) {
    detail::test_custom::hooks.obj_is_pinned = [](managed* m) { return m->get_type_id() == 2; };
    static uint8_t flag = 0;
    init_gc(flag);
    auto* h = create_tcb(nullptr, 0);
    init_thd(h, __builtin_frame_address(0));

    // Different size classes, so different pages
    auto* pinned = make_gc<obj>(h, 0, uint8_t(2));
    auto* loose = make_gc<obj>(h, 200, uint8_t(1));
    EXPECT_TRUE(detail::pg_meta::head_from_ptr(pinned)->has_pinned());
    EXPECT_FALSE(detail::pg_meta::head_from_ptr(loose)->has_pinned());

    delete_tcb(h);
    end_gc();
}

// Nothing else asks for a cycle under a heap this size, so each collect runs one of its own
TEST_F(GcApiTest, InitTakesTheTuningAndCollectRunsCycles) {
    static uint8_t flag = 0;
    init_gc(flag, { .initial_heap = 64u << 20 });
    auto* h = create_tcb(nullptr, 0);
    init_thd(h, __builtin_frame_address(0));
    EXPECT_EQ(stats().heap_trigger, 64u << 20);
    collect(h);
    collect(h);
    EXPECT_EQ(stats().cycles, 2u);

    end_gc();
    collect(h);
    EXPECT_EQ(stats().cycles, 2u) << "no cycle runs after end_gc";
    delete_tcb(h);
}

#ifndef NDEBUG
// The rule run_blocking states, which debug builds assert
TEST_F(GcApiTest, ABlockedThreadCantAllocateOrBarrier) {
    GTEST_FLAG_SET(death_test_style, "threadsafe");
    auto blocked = [](bool alloc) {
        static uint8_t flag = 0;
        init_gc(flag);
        auto* h = create_tcb(nullptr, 0);
        init_thd(h, __builtin_frame_address(0));
        auto* o = make_gc<obj>(h, 0, uint8_t(1));
        run_blocked(h, [&] {
            if (alloc) (void)make_gc<obj>(h, 0, uint8_t(1));
            else write_barrier(h, o);
        });
    };
    EXPECT_DEATH(blocked(true), "blocked");
    EXPECT_DEATH(blocked(false), "blocked");
}
#endif
