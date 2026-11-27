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

TEST_F(CleanupsTest, AddReturnsIncreasingIdsAndZeroOutsideTheHeap) {
    alignas(16) static unsigned char constant[16];
    test_page tracked{64};
    cleanups cl;
    auto a = cl.add(tracked.construct(0), 0, 0), b = cl.add(tracked.construct(1), 0, 0);
    EXPECT_GT(a, 0);
    EXPECT_GT(b, a);
    EXPECT_EQ(cl.add(new (constant) managed(1), 0, 0), 0) << "a constant never dies, so it isn't registered";
    EXPECT_EQ(cl.count().first, 2u);
}

// Removing leaves a tombstone until the next pause, whose slots hold nothing
TEST_F(CleanupsTest, RemovedEntryIsNeitherQueuedNorARoot) {
    test_page tracked{64}, strong{64};
    cleanups cl;
    auto id = cl.add(tracked.construct(0), w(strong.construct(0)), w(strong.construct(1)));
    ASSERT_TRUE(cl.remove(id));
    for (size_t& slot : cl.keepalive_slots()) EXPECT_EQ(slot, 0u);
    marker m;
    m.scan_roots(cl.keepalive_slots());
    EXPECT_EQ(strong->compute_live(), 0u) << "the cleanup and arg can die in the cycle that removed them";

    cl.queue_dead();
    EXPECT_FALSE(cl.pop()) << "tracked is unmarked, but its entry was removed";
    EXPECT_EQ(cl.count(), std::pair(size_t(0), size_t(0)));
}

// The second entry dies, so a compaction that moved the last entry into its place would break the order
TEST_F(CleanupsTest, RemoveFindsEntriesAfterQueueDeadCompacts) {
    test_page tracked{64};
    cleanups cl;
    int64_t ids[4];
    for (uint16_t i = 0; i < 4; ++i) ids[i] = cl.add(tracked.construct(i), 0, 0);
    for (uint16_t i : { 0, 2, 3 }) tracked.mark(i);
    cl.queue_dead();
    ASSERT_TRUE(cl.pop());
    EXPECT_TRUE(cl.remove(ids[0]));
    EXPECT_TRUE(cl.remove(ids[2]));
    EXPECT_TRUE(cl.remove(ids[3]));
}

TEST_F(CleanupsTest, RemoveOfAQueuedOrRemovedIdIsFalse) {
    test_page tracked{64};
    cleanups cl;
    auto removed = cl.add(tracked.construct(0), 0, 0), queued = cl.add(tracked.construct(1), 0, 0);
    ASSERT_TRUE(cl.remove(removed));
    EXPECT_FALSE(cl.remove(removed)) << "the tombstone is still in _live";
    cl.queue_dead();
    EXPECT_FALSE(cl.remove(removed));
    EXPECT_FALSE(cl.remove(queued)) << "its object died, so the cleanup is queued";
    EXPECT_FALSE(cl.remove(0));
    EXPECT_FALSE(cl.remove(queued + 1));
    EXPECT_TRUE(cl.pop());
}

TEST_F(CleanupsTest, ExternalIsChargedTakenBackAndResetByThePause) {
    test_page tracked{64};
    cleanups cl;
    auto a = cl.add(tracked.construct(0), 0, 0, 100), b = cl.add(tracked.construct(1), 0, 0, 30);
    tracked.mark(0);
    tracked.mark(1);
    EXPECT_EQ(cl.external(), 130u);
    ASSERT_TRUE(cl.remove(a));
    EXPECT_EQ(cl.external(), 30u);
    cl.queue_dead();
    EXPECT_EQ(cl.external(), 0u) << "the pause had its chance at what was charged before it";
    ASSERT_TRUE(cl.remove(b));
    EXPECT_EQ(cl.external(), 0u) << "a refund after the reset clamps at zero";
}
