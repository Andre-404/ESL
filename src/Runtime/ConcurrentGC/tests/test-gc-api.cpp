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
