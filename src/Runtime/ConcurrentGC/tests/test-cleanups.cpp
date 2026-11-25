#include <gtest/gtest.h>
#include "../cleanups.h"
#include "../marker.h"
#include "customization-helper.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

namespace {
    class CleanupsTest : public ::testing::Test {
    protected:
        void SetUp() override   { test_custom::hooks.reset(); }
        void TearDown() override { test_custom::hooks.reset(); }
    };
    size_t w(managed* m) { return ptr_to_word(m); }
}

// Liveness is the tracked object's mark bit, read in the pause after marking. pop takes the oldest
// entry queued, so the live entry goes in first: queued by mistake, it would come out first
TEST_F(CleanupsTest, QueueDeadTakesOnlyEntriesWhoseTrackedObjectIsUnmarked) {
    test_page tracked{64}, strong{64};
    auto live = tracked.construct(0), dead = tracked.construct(1);
    tracked.mark(0);
    cleanups cl;
    cl.add(live, w(strong.construct(0)), w(strong.construct(1)));
    cl.add(dead, w(strong.construct(2)), 0);

    cl.queue_dead();
    auto e = cl.pop();
    ASSERT_TRUE(e);
    EXPECT_EQ(e->first, w(strong.slot(2)));
    EXPECT_EQ(e->second, 0u) << "a word that isn't a reference comes back as it went in";

    // A cycle later nothing is marked, so the other entry goes too
    tracked.next_cycle();
    cl.queue_dead();
    e = cl.pop();
    ASSERT_TRUE(e);
    EXPECT_EQ(e->first, w(strong.slot(0)));
    EXPECT_EQ(e->second, w(strong.slot(1)));
    EXPECT_FALSE(cl.pop()) << "both entries have been taken";
}

TEST_F(CleanupsTest, KeepaliveSlotsOfLiveAndQueuedEntriesAreRoots) {
    test_page tracked{64}, strong{64};
    auto live = tracked.construct(0), dead = tracked.construct(1);
    tracked.mark(0);
    cleanups cl;
    cl.add(live, w(strong.construct(0)), w(strong.construct(1)));
    cl.add(dead, w(strong.construct(2)), w(strong.construct(3)));
    cl.queue_dead();

    marker m;
    m.scan_roots(cl.keepalive_slots());
    EXPECT_EQ(strong->compute_live(), 4u);
}

// A constant compiled into the program never dies, so its cleanup never runs
TEST_F(CleanupsTest, QueueDeadKeepsObjectsOutsideTheHeap) {
    alignas(16) static unsigned char constant[16];
    test_page tracked{64}, strong{64};
    cleanups cl;
    cl.add(tracked.construct(0), w(strong.construct(0)), 0);
    cl.add(new (constant) managed(1), w(strong.construct(1)), 0);
    cl.queue_dead();
    auto e = cl.pop();
    ASSERT_TRUE(e);
    EXPECT_EQ(e->first, w(strong.slot(0)));
    EXPECT_FALSE(cl.pop()) << "the constant's entry stays live";
}
