#include <gtest/gtest.h>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <new>
#include <span>
#include <unordered_set>
#include <vector>
#include "../marker.h"
#include "../pg-meta.h"
#include "../page-allocator.h"
#include "../platform-specific.h"
#include "customization-helper.h"
#include "pg-fixture.h"

using namespace gc;
using namespace gc::detail;
using gc::test::test_page;

namespace {
    class fake_stack {
    public:
        fake_stack(thd_mark_info& info, std::span<size_t> words)
                : _saved_override(platform::read_stack_override) {
            platform::read_stack_override = reinterpret_cast<size_t>(words.data());
            info.track_stack(reinterpret_cast<size_t>(words.data() + words.size()));
            info.capture_ctx();
        }
        ~fake_stack() { platform::read_stack_override = _saved_override; }
        fake_stack(const fake_stack&) = delete;
        fake_stack& operator=(const fake_stack&) = delete;
    private:
        size_t _saved_override;
    };

    class MarkerTest : public ::testing::Test {
    protected:
        void SetUp() override   { test_custom::hooks.reset(); }
        void TearDown() override { test_custom::hooks.reset(); }
    };

} // namespace


TEST_F(MarkerTest, ScanGlobalsSkipsUnmanagedObjects) {
    test_page rp{64};
    rp.construct(0, 1, move_state::unmanaged);

    marker m;
    size_t addr = reinterpret_cast<size_t>(rp.slot(0));
    std::vector<size_t*> roots = { &addr };
    m.scan_globals(roots);
    EXPECT_EQ(rp.pg()->compute_live(), 0u);
}

TEST_F(MarkerTest, ScanGlobalsSkipsWhenToAccuratePtrRejects) {
    test_page rp{64};
    rp.construct(0, 1);
    test_custom::hooks.to_accurate_ptr = [](size_t) -> managed* { return nullptr; };

    marker m;
    size_t addr = reinterpret_cast<size_t>(rp.slot(0));
    std::vector<size_t*> roots = { &addr };
    m.scan_globals(roots);
    EXPECT_EQ(rp.pg()->compute_live(), 0u);
}

TEST_F(MarkerTest, ScanGlobalsMarksReachableObjects) {
    test_page rp{64};
    rp.construct(0,  1);
    rp.construct(5,  1);
    rp.construct(10, 1);

    marker m;
    size_t a = reinterpret_cast<size_t>(rp.slot(0));
    size_t b = reinterpret_cast<size_t>(rp.slot(5));
    size_t c = reinterpret_cast<size_t>(rp.slot(10));
    std::vector<size_t*> roots = { &a, &b, &c };
    m.scan_globals(roots);

    EXPECT_EQ(rp.pg()->compute_live(), 3u);
    EXPECT_FALSE(rp.pg()->has_pinned());
}

TEST_F(MarkerTest, UntraceableObjectStillIncrementsLiveCount) {
    // push_obj records the mark BEFORE checking obj_traceable. An
    // untraceable object therefore still bumps live_count
    test_page rp{64};
    rp.construct(0, 1);
    test_custom::hooks.obj_traceable = [](managed*) { return false; };

    marker m;
    size_t a = reinterpret_cast<size_t>(rp.slot(0));
    std::vector<size_t*> roots = { &a };
    m.scan_globals(roots);

    EXPECT_EQ(rp.pg()->compute_live(), 1u);
}

TEST_F(MarkerTest, AliasedRootsAreDeduplicatedByRecordMark) {
    test_page rp{64};
    rp.construct(0, 1);

    marker m;
    size_t a = reinterpret_cast<size_t>(rp.slot(0));
    size_t b = a, c = a, d = a;
    std::vector<size_t*> roots = { &a, &b, &c, &d };
    m.scan_globals(roots);

    EXPECT_EQ(rp.pg()->compute_live(), 1u);
}

TEST_F(MarkerTest, PinnedStateObjectSetsPagePinned) {
    // push_obj passes (state != none) as is_pinned to record_mark.
    test_page rp{64};
    rp.construct(0, 1, move_state::pinned);

    marker m;
    size_t a = reinterpret_cast<size_t>(rp.slot(0));
    std::vector<size_t*> roots = { &a };
    m.scan_globals(roots);

    EXPECT_EQ(rp.pg()->compute_live(), 1u);
    EXPECT_TRUE(rp.pg()->has_pinned());
}

TEST_F(MarkerTest, ScanGlobalsAcrossManyBuffersStillMarksAll) {
    constexpr int N = 80;
    test_page rp{32};
    for (uint16_t i = 0; i < N; ++i) rp.construct(i, 1);

    std::vector<size_t> root_storage(N);
    std::vector<size_t*> roots(N);
    for (int i = 0; i < N; ++i) {
        root_storage[i] = reinterpret_cast<size_t>(rp.slot(i));
        roots[i] = &root_storage[i];
    }

    marker m;
    m.scan_globals(roots);
    EXPECT_EQ(rp.pg()->compute_live(), N);
}

TEST_F(MarkerTest, ScanStackOnUntrackedThreadDoesNothing) {
    test_page rp{64};
    rp.construct(0, 1);

    thd_mark_info info;   // no track_stack -> get_ctx returns empty spans
    marker m;
    m.scan_stack(info, false, [](uint8_t* p) {
        return reinterpret_cast<managed*>(p);
    });
    EXPECT_EQ(rp.pg()->compute_live(), 0u);
}

TEST_F(MarkerTest, ScanStackWithPinSetsTempPinnedAndPagePinned) {
    test_page rp{64};
    managed* obj = rp.construct(0, 1, move_state::none);

    // Fake stack: one word containing the object's address.
    std::vector<size_t> stack_words = { reinterpret_cast<size_t>(obj) };
    thd_mark_info info;
    fake_stack fs(info, stack_words);

    marker m;
    m.scan_stack(info, true, [&](uint8_t* p) -> managed* {
        return reinterpret_cast<managed*>(p) == obj ? obj : nullptr;
    });

    EXPECT_EQ(rp.pg()->compute_live(), 1u);
    EXPECT_TRUE(rp.pg()->has_pinned());
    EXPECT_EQ(obj->state(), move_state::temp_pinned);
}

TEST_F(MarkerTest, ScanStackWithoutPinLeavesObjectStateUnchanged) {
    test_page rp{64};
    managed* obj = rp.construct(0, 1, move_state::none);

    std::vector<size_t> stack_words = { reinterpret_cast<size_t>(obj) };
    thd_mark_info info;
    fake_stack fs(info, stack_words);

    marker m;
    m.scan_stack(info, false, [&](uint8_t* p) -> managed* {
        return reinterpret_cast<managed*>(p) == obj ? obj : nullptr;
    });

    EXPECT_EQ(rp.pg()->compute_live(), 1u);
    EXPECT_FALSE(rp.pg()->has_pinned());
    EXPECT_EQ(obj->state(), move_state::none);
}

TEST_F(MarkerTest, ScanStackPinsEvenForAlreadyMarkedObject) {
    test_page rp{64};
    managed* obj = rp.construct(0, 1, move_state::none);

    std::vector<size_t> stack_words = { reinterpret_cast<size_t>(obj) };
    thd_mark_info info;
    fake_stack fs(info, stack_words);
    marker m;
    m.scan_stack(info, false, [&](uint8_t* p) -> managed* {
        return reinterpret_cast<managed*>(p) == obj ? obj : nullptr;
    });

    ASSERT_EQ(rp.pg()->compute_live(), 1u);
    ASSERT_FALSE(rp.pg()->has_pinned());

    m.scan_stack(info, true, [&](uint8_t* p) -> managed* {
        return reinterpret_cast<managed*>(p) == obj ? obj : nullptr;
    });

    EXPECT_EQ(obj->state(), move_state::temp_pinned) << "mark callback runs before push_obj's record_mark check";
    EXPECT_TRUE(rp.pg()->has_pinned()) << "page pinned bit updates even on marked objects";
}

TEST_F(MarkerTest, ScanStackIgnoresWordsGetBaseRejects) {
    test_page rp{64};
    managed* obj = rp.construct(7, 1);
    std::vector<size_t> stack_words(8, 0xDEADBEEF);
    stack_words[3] = reinterpret_cast<size_t>(obj);

    thd_mark_info info;
    fake_stack fs(info, stack_words);

    marker m;
    m.scan_stack(info, false, [&](uint8_t* p) -> managed* {
        return reinterpret_cast<managed*>(p) == obj ? obj : nullptr;
    });
    EXPECT_EQ(rp.pg()->compute_live(), 1u);
}

TEST_F(MarkerTest, TraceNReturnsZeroWhenNoWorkAvailable) {
    marker m;
    EXPECT_EQ(m.trace_n(1024), 0u);
}

TEST_F(MarkerTest, TraceNVisitsRootsAndChildren) {
    test_page rp{64};
    managed* root = rp.construct(0, 1);
    managed* c1   = rp.construct(1, 2);
    managed* c2   = rp.construct(2, 3);

    std::vector<managed*> visited;
    test_custom::hooks.obj_trace =
    [&, root, c1, c2](managed* m, std::function<void(managed*)>& mark) {
        visited.push_back(m);
        if (m == root) { mark(c1); mark(c2); }
    };
    test_custom::hooks.obj_size = [](managed*) { return size_t{10}; };

    marker m;
    size_t r = reinterpret_cast<size_t>(root);
    std::vector<size_t*> roots = { &r };
    m.scan_globals(roots);

    size_t traced = m.trace_n(40);
    EXPECT_GE(traced, 30u);   // three objects, 10 bytes each
    EXPECT_EQ(visited.size(), 3u);
    std::unordered_set<managed*> seen(visited.begin(), visited.end());
    EXPECT_TRUE(seen.count(root));
    EXPECT_TRUE(seen.count(c1));
    EXPECT_TRUE(seen.count(c2));
}

TEST_F(MarkerTest, TraceNStopsCloseToByteBudget) {
    test_page rp{32};
    for (uint16_t i = 0; i < 10; ++i) rp.construct(i, 1);

    test_custom::hooks.obj_size = [](managed*) { return size_t{100}; };

    marker m;
    std::vector<size_t> storage(10);
    std::vector<size_t*> roots(10);
    for (int i = 0; i < 10; ++i) {
        storage[i] = reinterpret_cast<size_t>(rp.slot(i));
        roots[i] = &storage[i];
    }
    m.scan_globals(roots);

    size_t traced = m.trace_n(250);
    EXPECT_GE(traced, 250);
    EXPECT_LT(traced, 250 + 100 + 100) << "expected at most one obj_size of overshoot, got " << traced;
}

TEST_F(MarkerTest, TraceNHandlesCyclesViaRecordMarkDedupe) {
    test_page rp{64};
    managed* a = rp.construct(0, 1);
    managed* b = rp.construct(1, 1);

    int a_visits = 0, b_visits = 0;
    test_custom::hooks.obj_trace =
    [&, a, b](managed* m, std::function<void(managed*)>& mark) {
        if (m == a) { ++a_visits; mark(b); }
        else if (m == b) { ++b_visits; mark(a); }
    };
    test_custom::hooks.obj_size = [](managed*) { return size_t{1}; };

    marker m;
    size_t r = reinterpret_cast<size_t>(a);
    std::vector<size_t*> roots = { &r };
    m.scan_globals(roots);
    (void)m.trace_n(10000);

    EXPECT_EQ(a_visits, 1);
    EXPECT_EQ(b_visits, 1);
}

TEST_F(MarkerTest, TraceNAcrossBufferBoundary) {
    constexpr int N = 150;
    test_page rp{32};
    std::vector<managed*> chain;
    chain.reserve(N);
    for (int i = 0; i < N; ++i) chain.push_back(rp.construct((uint16_t)i, 1));

    int total_visited = 0;
    test_custom::hooks.obj_trace =
    [&chain, &total_visited](managed* m, std::function<void(managed*)>& mark) {
        ++total_visited;
        auto it = std::find(chain.begin(), chain.end(), m);
        if (it != chain.end() && it + 1 != chain.end()) mark(*(it + 1));
    };
    test_custom::hooks.obj_size = [](managed*) { return size_t{1}; };

    marker m;
    size_t r = reinterpret_cast<size_t>(chain.front());
    std::vector<size_t*> roots = { &r };
    m.scan_globals(roots);
    (void)m.trace_n(10000);

    EXPECT_EQ(total_visited, N);
}

TEST_F(MarkerTest, TraceNCanBeCalledRepeatedlyToDrainTheWorklist) {
    constexpr int N = 200;
    test_page rp{32};
    std::vector<managed*> objs;
    objs.reserve(N);
    for (int i = 0; i < N; ++i) objs.push_back(rp.construct((uint16_t)i, 1));

    int visited = 0;
    test_custom::hooks.obj_trace =
    [&visited](managed*, std::function<void(managed*)>&) { ++visited; };
    test_custom::hooks.obj_size = [](managed*) { return size_t{1}; };

    marker m;
    std::vector<size_t> storage(N);
    std::vector<size_t*> roots(N);
    for (int i = 0; i < N; ++i) {
        storage[i] = reinterpret_cast<size_t>(objs[i]);
        roots[i] = &storage[i];
    }
    m.scan_globals(roots);

    size_t total = 0;
    while (true) {
        size_t got = m.trace_n(20);
        if (got == 0) break;
        total += got;
    }
    EXPECT_EQ(visited, N);
    EXPECT_EQ(total, N);
}

TEST_F(MarkerTest, GetBufReturnsAnEmptyBuf) {
    marker m;
    mark_buf* b = m.get_buf();
    ASSERT_NE(b, nullptr);
    EXPECT_TRUE(b->empty());
    m.push_buf(b);
}

TEST_F(MarkerTest, PushBufRecyclesEmptyBufs) {
    marker m;
    mark_buf* a = m.get_buf();
    mark_buf* b = m.get_buf();
    ASSERT_NE(a, b);
    m.push_buf(a);
    m.push_buf(b);
    mark_buf* x = m.get_buf();
    mark_buf* y = m.get_buf();
    std::unordered_set<mark_buf*> recycled = { a, b };
    EXPECT_TRUE(recycled.count(x));
    EXPECT_TRUE(recycled.count(y));
}