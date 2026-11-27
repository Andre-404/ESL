#include <gtest/gtest.h>
#include <algorithm>
#include <new>
#include <vector>

#include "../heap-verifier.h"
#include "customization-helper.h"

using namespace gc;
using namespace gc::detail;

// Pages come from a pg_manager of the test's own, not pg-fixture's shared allocator: there is one heap per
// process, and the verifier walks a pg_manager
namespace {
    struct holder : managed {
        managed* ref;
        explicit holder(managed* r = nullptr) : managed(1), ref(r) {}
    };

    size_t count_bytes(void* p, size_t n, uint8_t b) { return std::count((uint8_t*)p, (uint8_t*)p + n, b); }

    class HeapVerifierTest : public ::testing::Test {
    protected:
        pg_manager pgs;
        heap_verifier verifier { true, pgs };
        pg_meta* run = nullptr;

        void SetUp() override {
            test_custom::hooks.reset();
            // Every object in these tests is a holder
            test_custom::hooks.obj_trace = [](managed* m, std::function<void(managed*)>& cb) {
                cb(static_cast<holder*>(m)->ref);
            };
            run = pgs.new_pg(64);
        }
        void TearDown() override { test_custom::hooks.reset(); }

        // An allocated block of run, marked if the mark reached it
        holder* block(uint16_t i, bool marked, managed* ref = nullptr) {
            auto obj = new (run->get_data() + i * run->block_sz()) holder(ref);
            run->store_alloc_word(i, run->load_alloc_word(i) | 1ull << i % 64);
            if (marked) run->record_mark(obj);
            return obj;
        }
        // A large run's object, published the way arena does once it is built
        holder* large(managed* ref = nullptr) {
            auto pg = pgs.new_pg(64 * 1024);
            auto obj = new (pg->get_data()) holder(ref);
            pg->store_alloc_word(0, 1);
            return obj;
        }
    };
}

TEST_F(HeapVerifierTest, RunsAreTheActiveHeads) {
    auto big = pg_meta::head_from_ptr(large());
    auto gone = pg_meta::head_from_ptr(large());
    { auto b = pgs.start_batch(); b.add(gone); }
    pgs.free_pgs();

    std::vector<pg_meta*> seen;
    for (auto r : pgs.runs()) seen.push_back(r);
    std::vector<pg_meta*> want { run, big };
    std::ranges::sort(seen);
    std::ranges::sort(want);
    EXPECT_EQ(seen, want) << "a continuation page or a freed run was taken for a run";
}

TEST_F(HeapVerifierTest, ConsistentHeapHasNoViolations) {
    // Out of the heap, like a constant compiled into the program
    static holder constant;
    auto big = large(&constant);
    pg_meta::head_from_ptr(big)->record_mark(big);
    block(0, true, block(1, true, big));
    block(2, false);
    EXPECT_EQ(verifier.check(), std::vector<violation>{});
}

TEST_F(HeapVerifierTest, MarkedHolderPointingAtAnUnmarkedBlockIsReported) {
    auto dead = block(5, false);
    auto h = block(0, true, dead);
    EXPECT_EQ(verifier.check(), (std::vector<violation>{ { violation::unmarked, h, dead } }));
}

TEST_F(HeapVerifierTest, PointersThatAreNotABlockStartAreReported) {
    auto mid = (managed*)((uint8_t*)block(4, true) + 8);
    auto never_allocated = (managed*)(run->get_data() + 6 * run->block_sz());
    // In the heap, on a page no run ever took
    auto no_page = (managed*)(heap_layout::heap(run->get_data()) + config::heap_max_sz - config::page_sz);
    auto a = block(0, true, mid), b = block(1, true, never_allocated), c = block(2, true, no_page);
    EXPECT_EQ(verifier.check(), (std::vector<violation>{
        { violation::not_a_block, a, mid },
        { violation::not_a_block, b, never_allocated },
        { violation::not_a_block, c, no_page },
    }));
}

TEST_F(HeapVerifierTest, MarkedButUnallocatedIsReported) {
    auto obj = new (run->get_data() + 3 * run->block_sz()) holder();
    run->record_mark(obj);
    EXPECT_EQ(verifier.check(), (std::vector<violation>{ { violation::unallocated, obj, nullptr } }));
}

TEST_F(HeapVerifierTest, AfterMarkPoisonsTheDeadSmallBlocks) {
    auto live = block(0, true);
    auto dead = block(1, false);
    auto never_allocated = run->get_data() + 2 * run->block_sz();
    auto big = large();

    heap_verifier { false, pgs }.after_mark();
    EXPECT_EQ(count_bytes(dead, 64, heap_verifier::poison_byte), 0u) << "off, it touched the heap";

    verifier.after_mark();
    EXPECT_EQ(count_bytes(dead, 64, heap_verifier::poison_byte), 64u);
    EXPECT_EQ(count_bytes(live, 64, heap_verifier::poison_byte), 0u);
    EXPECT_EQ(count_bytes(never_allocated, 64, heap_verifier::poison_byte), 0u);
    EXPECT_EQ(count_bytes(big, 64 * 1024, heap_verifier::poison_byte), 64 * 1024);
}
