#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <new>
#include <vector>
#include <span>
#include <unordered_set>
#include "../copier.h"
#include "../pg-meta.h"
#include "../page-allocator.h"
#include "../pruner.h"
#include "customization-helper.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

namespace {
    class CopierTest : public ::testing::Test {
    protected:
        void SetUp() override { test_custom::hooks.reset(); }
        void TearDown() override { test_custom::hooks.reset(); }

        static pg_meta* chain(std::initializer_list<test_page*> pages) {
            if (pages.size() == 0) return nullptr;
            pg_meta* prev = nullptr;
            pg_meta* head = nullptr;
            for (auto* rp : pages) {
                if (!head) head = rp->pg();
                else       prev->link(rp->pg());
                prev = rp->pg();
            }
            return head;
        }
    };

}

TEST_F(CopierTest, UpdateGlobalsLeavesUnmovedPointerEncoded) {
    test_page rp{64};
    managed* obj = rp.construct(0, 1, move_state::none);

    copier c{ 0.5 };
    size_t root = reinterpret_cast<size_t>(obj);
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);

    EXPECT_EQ(root, reinterpret_cast<size_t>(obj));
}

TEST_F(CopierTest, UpdateGlobalsFollowsForwardingPointerForMovedObjects) {
    test_page rp_src{64};
    test_page rp_dst{64};
    managed* src  = rp_src.construct(0, 1, move_state::none);
    managed* dest = rp_dst.construct(0, 2, move_state::none);

    src->set_state(move_state::moved);
    set_moved(src, dest);

    copier c{ 0.5 };
    size_t root = reinterpret_cast<size_t>(src);
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);

    EXPECT_EQ(root, reinterpret_cast<size_t>(dest)) << "moved object's root must follow forwarding pointer ";
}

TEST_F(CopierTest, UpdateGlobalsSkipsRootsThatToAccuratePtrRejects) {
    test_custom::hooks.to_accurate_ptr = [](size_t) -> managed* { return nullptr; };

    copier c{0.5};
    size_t root = 0xDEADBEEF;
    size_t original = root;
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);

    EXPECT_EQ(root, original) << "rejected root must not be rewritten";
}

TEST_F(CopierTest, UpdateGlobalsRunsPtrToWordOnEveryUpdate) {
    test_page rp{64};
    managed* obj = rp.construct(0, 1);

    test_custom::hooks.ptr_to_word = [](managed* p) {
        return reinterpret_cast<size_t>(p) | 0xAB00000000000000ull;
    };

    copier c{0.5};
    size_t root = reinterpret_cast<size_t>(obj);
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);

    EXPECT_EQ(root & 0xFF00000000000000ull, 0xAB00000000000000ull) << "ptr_to_word tag must have been applied";
}

TEST_F(CopierTest, UpdatePtrsCallsObjUpdatePtrsOnMarkedObjectsOnly) {
    test_page rp{64};
    rp.construct(0, 1);
    rp.construct(1, 1);
    rp.construct(2, 1);
    rp.mark(0);
    rp.mark(2);
    // slot 1 is unmarked.

    std::unordered_set<managed*> updated;
    test_custom::hooks.obj_update_ptrs = [&](managed* m) { updated.insert(m); };

    copier c{0.5};
    c.update_ptrs(rp.pg());

    EXPECT_EQ(updated.size(), 2);
    EXPECT_TRUE(updated.count(rp.slot(0)));
    EXPECT_TRUE(updated.count(rp.slot(2)));
    EXPECT_FALSE(updated.count(rp.slot(1)));
}

TEST_F(CopierTest, UpdatePtrsResetsTempPinnedToNone) {
    test_page rp{64};
    managed* a = rp.construct(0, 1, move_state::temp_pinned);
    managed* b = rp.construct(1, 1, move_state::pinned);
    rp.mark(0); rp.mark(1);

    copier c{0.5};
    c.update_ptrs(rp.pg());

    EXPECT_EQ(a->state(), move_state::none)    << "temp_pinned -> none";
    EXPECT_EQ(b->state(), move_state::pinned)  << "pinned must NOT be reset";
}

TEST_F(CopierTest, UpdatePtrsOnEmptyPageDoesNothing) {
    test_page rp{64};
    bool any_update = false;
    test_custom::hooks.obj_update_ptrs = [&](managed*) { any_update = true; };

    copier c{0.5};
    c.update_ptrs(rp.pg());
    EXPECT_FALSE(any_update);
}

TEST_F(CopierTest, CopyObjectsOnEmptyListIsNoOp) {
    copier c{0.5};
    c.copy_objects(nullptr);
    SUCCEED();
}

TEST_F(CopierTest, CopyObjectsWithOnlySourcesConvertsOneToTarget) {
    test_page sa{64}, sb{64};
    sa.construct(0, 1); sa.construct(1, 1); sa.mark(0); sa.mark(1);
    sb.construct(0, 1); sb.construct(1, 1); sb.mark(0); sb.mark(1);
    pg_meta* head = chain({ &sa, &sb });

    int copy_count = 0;
    test_custom::hooks.obj_copy = [&](managed* /*src*/, managed* /*dest*/) { ++copy_count; };

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(copy_count, 2) << "the two live objects of one source page must be copied";
}

TEST_F(CopierTest, FullyFullPageIsNeitherSourceNorTarget) {
    test_page full{64};
    for (uint16_t i = 0; i < full.pg()->block_cnt(); ++i) {
        full.construct(i, 1); full.mark(i);
    }
    test_page sparse{64};
    sparse.construct(0, 1); sparse.mark(0);

    pg_meta* head = chain({ &full, &sparse });

    int copy_count = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copy_count; };

    copier c{0.5};
    c.copy_objects(head);

    // Source is converted to target, no copying happens
    EXPECT_EQ(copy_count, 0);
}

TEST_F(CopierTest, PinnedSparsePageIsTreatedAsTargetNotSource) {
    test_page pinned{64};
    pinned.construct(0, 1); pinned.mark(0, true);

    test_page sparse{64};
    sparse.construct(0, 1); sparse.construct(1, 1);
    sparse.mark(0); sparse.mark(1);

    pg_meta* head = chain({ &pinned, &sparse });

    std::vector<std::pair<managed*, managed*>> copies;
    test_custom::hooks.obj_copy = [&](managed* s, managed* d) {
        copies.emplace_back(s, d);
    };

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(copies.size(), 2u);
    // Every destination must be on the pinned page
    for (auto [s, d] : copies) {
        EXPECT_EQ(pg_meta::head_from_ptr(d), pinned.pg());
        EXPECT_EQ(pg_meta::head_from_ptr(s), sparse.pg());
    }
}

TEST_F(CopierTest, CopiedObjectsAreMarkedMovedWithForwardingPtr) {
    test_page src{64}, dst{64};
    managed* src_obj = src.construct(0, 7);
    src.mark(0);
    dst.construct(0, 9);
    dst.mark(0, true);

    pg_meta* head = chain({ &dst, &src });

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(src_obj->state(), move_state::moved)<<"dest ptr can't overwrite moved byte";

    auto fwd = get_moved(src_obj);
    EXPECT_EQ(pg_meta::head_from_ptr(fwd), dst.pg());
}

TEST_F(CopierTest, CopyObjectsUpdatesTargetMarkBitmap) {
    test_page src{64}, dst{64};
    src.construct(0, 1); src.mark(0);

    dst.construct(0, 9); dst.mark(0, true);

    pg_meta* head = chain({ &dst, &src });

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(dst.pg()->compute_live(), 2);
    EXPECT_FALSE(src.pg()->is_active()) << "the drained page is retired, not recounted";
}

TEST_F(CopierTest, ObjCopyIsCalledExactlyOncePerLiveSourceObject) {
    test_page pinned{64};
    pinned.construct(0, 1);
    pinned.mark(0, true);

    test_page src{64};
    for (uint16_t i = 0; i < 5; ++i) {
        src.construct(i, 1);
        src.mark(i);
    }

    pg_meta* head = chain({ &pinned, &src });

    int copies = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copies; };

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(copies, 5) << "one obj_copy per marked source slot";
}

TEST_F(CopierTest, AfterCopySourcePagesAreRetired) {
    test_page src{64};
    test_page pinned{64};
    src.construct(0, 1); src.construct(1, 1);
    src.mark(0); src.mark(1);
    pinned.construct(0, 1); pinned.mark(0, true);

    pg_meta* head = chain({ &pinned, &src });
    ASSERT_EQ(src.pg()->compute_live(), 2);

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_FALSE(src.pg()->is_active());
    EXPECT_FALSE(src.pg()->has_pinned());
}


TEST_F(CopierTest, ThresholdZeroPromotesEverythingToTarget) {
    test_page a{64}, b{64};
    a.construct(0, 1); a.mark(0);
    b.construct(0, 1); b.mark(0);

    pg_meta* head = chain({ &a, &b });

    int copies = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copies; };

    copier c{0.0};
    c.copy_objects(head);
    EXPECT_EQ(copies, 0);
}

TEST_F(CopierTest, ThresholdHighMakesEverySparsePageASourceUntilOneConverts) {
    test_page a{64}, b{64};
    a.construct(0, 1); a.mark(0);
    b.construct(0, 1); b.mark(0);

    pg_meta* head = chain({ &a, &b });

    int copies = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copies; };

    copier c{2.0};
    c.copy_objects(head);
    EXPECT_EQ(copies, 1);
}
// ---------------------------------------------------------------------------------------
// Evacuation must leave a source page genuinely empty.
//
// copy_objects used to flip the source page's two bitmaps, which republished the pre-copy
// alloc bitmap as the live one: every moved-from corpse read back as an allocated object, the
// pruner never saw the page as empty, and the page was never returned to the allocator. It now
// marks the page inactive instead. These pin the whole sequence: the corpses are unreachable,
// the pruner frees the page, and the live accounting counts survivors once.
// ---------------------------------------------------------------------------------------
namespace {
    // A dense pinned target plus a sparse source, wired into one list. Both pages carry
    // realistic alloc bits so the post-copy state of the source is observable.
    struct evac_fixture {
        test_page src { 64 };
        test_page dst { 64 };

        evac_fixture() {
            for (uint16_t i = 0; i < 2; ++i) {
                src.construct(i, 1);
                src.allocate(i);
                src.mark(i);
            }
            // Pinned, so split_pages keeps it a target no matter how sparse it is.
            dst.construct(0, 9);
            dst.allocate(0);
            dst.mark(0, true);
        }
        pg_meta* list() { dst.pg()->link(src.pg()); return dst.pg(); }
    };
}

TEST_F(CopierTest, EvacuatedSourcePageIsRejectedByPointerLookup) {
    evac_fixture f;
    copier c{0.5};
    c.copy_objects(f.list());

    // from_interior is never reached for an inactive page: the lookup that guards it rejects
    // the whole page, which is what keeps a moved-from corpse from reading back as an object.
    EXPECT_FALSE(f.src.pg()->is_active());
    EXPECT_FALSE(gc::test::heap().pg_active(f.src.pg()));
}

TEST_F(CopierTest, CopyThenPruneFreesTheEvacuatedSourcePage) {
    evac_fixture f;
    auto* head = f.list();

    copier c{0.5};
    c.copy_objects(head);

    std::vector<pg_meta*> freed;
    pruner p;
    auto* in_use = p.prune(head, gc::test::bits(), [&](pg_meta* pg) { freed.push_back(pg); });

    ASSERT_EQ(freed.size(), 1u) << "the evacuated page is empty and must go back to the allocator";
    EXPECT_EQ(freed[0], f.src.pg());
    EXPECT_EQ(in_use, f.dst.pg());
    EXPECT_EQ(in_use->next(), nullptr);
}

TEST_F(CopierTest, CopyThenPruneCountsSurvivorsOnce) {
    evac_fixture f;
    auto* head = f.list();

    copier c{0.5};
    c.copy_objects(head);

    pruner p;
    p.prune(head, gc::test::bits(), [](pg_meta*) {});

    // One resident plus the two that moved in, on the target page alone. Counting the
    // evacuated originals again - or counting the target twice, once while splitting pages
    // and once while pruning - both show up here.
    EXPECT_EQ(p.live_bytes(), 3u * 64u);
}
