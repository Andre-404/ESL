#include <gtest/gtest.h>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <new>
#include <vector>
#include <span>
#include <unordered_set>
#include "../copier.h"
#include "../pg-meta.h"
#include "../page-allocator.h"
#include "../customization.h"
#include "customization-helper.h"

using namespace gc;
using namespace gc::detail;

namespace {

    class real_page {
    public:
        explicit real_page(size_t block_sz) : _block_sz(block_sz) {
            _pg = _a.alloc_pg(block_sz, 1);
        }
        ~real_page() { _a.dealloc_pgs(_pg, _pg); }
        real_page(const real_page&) = delete;
        real_page& operator=(const real_page&) = delete;

        pg_meta* pg() const { return _pg; }
        size_t block_sz() const { return _block_sz; }

        managed* construct(uint16_t i, uint8_t type_id, move_state st = move_state::none) {
            return new (slot_addr(i)) managed(type_id, st);
        }
        managed* slot(uint16_t i) const { return reinterpret_cast<managed*>(slot_addr(i)); }
        uint8_t* slot_addr(uint16_t i) const {
            return reinterpret_cast<uint8_t*>(_pg) + _pg->start_off() + i * _block_sz;
        }

        // Mark slot i in the page's mark bitmap (also bumps live_count).
        bool mark(uint16_t i, bool pinned = false) {
            return _pg->record_mark(slot(i), pinned);
        }

    private:
        pg_allocator _a;
        pg_meta*     _pg = nullptr;
        size_t       _block_sz;
    };

    class CopierTest : public ::testing::Test {
    protected:
        void SetUp() override { test_custom::hooks.reset(); }
        void TearDown() override { test_custom::hooks.reset(); }

        static pg_meta* chain(std::initializer_list<real_page*> pages) {
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
    real_page rp{64};
    managed* obj = rp.construct(0, 1, move_state::none);

    copier c{ 0.5 };
    size_t root = reinterpret_cast<size_t>(obj);
    std::vector<size_t*> roots = { &root };
    c.update_globals(roots);

    EXPECT_EQ(root, reinterpret_cast<size_t>(obj));
}

TEST_F(CopierTest, UpdateGlobalsFollowsForwardingPointerForMovedObjects) {
    real_page rp_src{64};
    real_page rp_dst{64};
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
    real_page rp{64};
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
    real_page rp{64};
    rp.construct(0, 1);
    rp.construct(1, 1);
    rp.construct(2, 1);
    rp.mark(0);
    rp.mark(2);
    // slot 1 is unmarked.
    rp.pg()->compute_live();

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
    real_page rp{64};
    managed* a = rp.construct(0, 1, move_state::temp_pinned);
    managed* b = rp.construct(1, 1, move_state::pinned);
    rp.mark(0); rp.mark(1);
    rp.pg()->compute_live();

    copier c{0.5};
    c.update_ptrs(rp.pg());

    EXPECT_EQ(a->state(), move_state::none)    << "temp_pinned -> none";
    EXPECT_EQ(b->state(), move_state::pinned)  << "pinned must NOT be reset";
}

TEST_F(CopierTest, UpdatePtrsOnEmptyPageDoesNothing) {
    real_page rp{64};
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
    real_page sa{64}, sb{64};
    sa.construct(0, 1); sa.construct(1, 1); sa.mark(0); sa.mark(1);
    sb.construct(0, 1); sb.construct(1, 1); sb.mark(0); sb.mark(1);
    sa.pg()->compute_live(); sb.pg()->compute_live();
    pg_meta* head = chain({ &sa, &sb });

    int copy_count = 0;
    test_custom::hooks.obj_copy = [&](managed* /*src*/, managed* /*dest*/) { ++copy_count; };

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(copy_count, 2) << "the two live objects of one source page must be copied";
}

TEST_F(CopierTest, FullyFullPageIsNeitherSourceNorTarget) {
    real_page full{64};
    for (uint16_t i = 0; i < full.pg()->block_cnt(); ++i) {
        full.construct(i, 1); full.mark(i);
    }
    real_page sparse{64};
    sparse.construct(0, 1); sparse.mark(0);
    sparse.pg()->compute_live();

    pg_meta* head = chain({ &full, &sparse });

    int copy_count = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copy_count; };

    copier c{0.5};
    c.copy_objects(head);

    // Source is converted to target, no copying happens
    EXPECT_EQ(copy_count, 0);
}

TEST_F(CopierTest, PinnedSparsePageIsTreatedAsTargetNotSource) {
    real_page pinned{64};
    pinned.construct(0, 1); pinned.mark(0, true);
    pinned.pg()->compute_live();

    real_page sparse{64};
    sparse.construct(0, 1); sparse.construct(1, 1);
    sparse.mark(0); sparse.mark(1);
    sparse.pg()->compute_live();

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
        EXPECT_EQ(pg_from_obj(d), pinned.pg());
        EXPECT_EQ(pg_from_obj(s), sparse.pg());
    }
}

TEST_F(CopierTest, CopiedObjectsAreMarkedMovedWithForwardingPtr) {
    real_page src{64}, dst{64};
    managed* src_obj = src.construct(0, 7);
    src.mark(0);
    dst.construct(0, 9);
    dst.mark(0, true);
    src.pg()->compute_live();
    dst.pg()->compute_live();

    pg_meta* head = chain({ &dst, &src });

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(src_obj->state(), move_state::moved)<<"dest ptr can't overwrite moved byte";

    auto fwd = get_moved(src_obj);
    EXPECT_EQ(pg_from_obj(fwd), dst.pg());
}

TEST_F(CopierTest, CopyObjectsUpdatesTargetMarkBitmap) {
    real_page src{64}, dst{64};
    src.construct(0, 1); src.mark(0);

    dst.construct(0, 9); dst.mark(0, true);
    src.pg()->compute_live();
    dst.pg()->compute_live();

    pg_meta* head = chain({ &dst, &src });

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(dst.pg()->live_count(), 2);
    EXPECT_EQ(src.pg()->live_count(), 0);
}

TEST_F(CopierTest, ObjCopyIsCalledExactlyOncePerLiveSourceObject) {
    real_page pinned{64};
    pinned.construct(0, 1);
    pinned.mark(0, true);
    pinned.pg()->compute_live();

    real_page src{64};
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

TEST_F(CopierTest, AfterCopySourcePagesAreReset) {
    real_page src{64};
    real_page pinned{64};
    src.construct(0, 1); src.construct(1, 1);
    src.mark(0); src.mark(1);
    pinned.construct(0, 1); pinned.mark(0, true);
    src.pg()->compute_live();
    pinned.pg()->compute_live();

    pg_meta* head = chain({ &pinned, &src });
    ASSERT_EQ(src.pg()->live_count(), 2);

    copier c{0.5};
    c.copy_objects(head);

    EXPECT_EQ(src.pg()->live_count(), 0);
    EXPECT_FALSE(src.pg()->has_pinned());
}


TEST_F(CopierTest, ThresholdZeroPromotesEverythingToTarget) {
    real_page a{64}, b{64};
    a.construct(0, 1); a.mark(0);
    b.construct(0, 1); b.mark(0);
    a.pg()->compute_live();
    b.pg()->compute_live();

    pg_meta* head = chain({ &a, &b });

    int copies = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copies; };

    copier c{0.0};
    c.copy_objects(head);
    EXPECT_EQ(copies, 0);
}

TEST_F(CopierTest, ThresholdHighMakesEverySparsePageASourceUntilOneConverts) {
    real_page a{64}, b{64};
    a.construct(0, 1); a.mark(0);
    b.construct(0, 1); b.mark(0);
    a.pg()->compute_live();
    b.pg()->compute_live();

    pg_meta* head = chain({ &a, &b });

    int copies = 0;
    test_custom::hooks.obj_copy = [&](managed*, managed*) { ++copies; };

    copier c{2.0};
    c.copy_objects(head);
    EXPECT_EQ(copies, 1);
}