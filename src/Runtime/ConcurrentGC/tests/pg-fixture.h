#pragma once
#include <cstdint>
#include <new>

#include "../managed.h"
#include "../page-allocator.h"
#include "../pg-meta.h"

namespace gc::test {
    // One allocator for the whole binary. Pages have to come from a real one: a pg_meta derives
    // its page's data address from where the header sits in the heap reservation, so a header
    // built on malloc'd memory addresses nothing. A pg_allocator reserves the whole heap region
    // and starts a scavenger thread, which is not worth doing per fixture.
    inline detail::pg_allocator& heap() {
        static auto a = detail::pg_allocator {};
        return a;
    }
    inline detail::gc_bits& bits() { return heap().bits(); }

    // A page borrowed from the shared heap for the lifetime of the fixture
    class test_page {
        detail::pg_meta* _pg;
    public:
        // npages defaults the way pg_manager::new_pg sizes a run: one page for a size class,
        // enough pages to hold the object for anything larger
        explicit test_page(size_t block_sz, size_t npages = 0)
            : _pg(heap().alloc_pg(block_sz,
                  npages ? npages : (block_sz + config::page_sz - 1) / config::page_sz)) {}
        ~test_page() { if (_pg) { _pg->unlink(); heap().free_pgs(_pg); } }
        test_page(const test_page&) = delete;
        test_page& operator=(const test_page&) = delete;

        detail::pg_meta* pg() const { return _pg; }
        detail::pg_meta* operator->() const { return _pg; }
        size_t block_sz() const { return _pg->block_sz(); }

        uint8_t* slot_addr(uint16_t i) const { return _pg->get_data() + i * _pg->block_sz(); }
        managed* slot(uint16_t i) const { return (managed*)slot_addr(i); }
        managed* construct(uint16_t i, uint8_t type_id = 1, move_state st = move_state::none) {
            return new (slot_addr(i)) managed(type_id, st);
        }

        bool mark(uint16_t i, bool pinned = false) { return _pg->record_mark(slot(i), pinned); }
        // What a mark phase would have left behind: the first n slots built and marked
        void mark_n(int n, bool pinned = false) {
            for (int i = 0; i < n; ++i) { construct(i); mark(i, pinned); }
        }
        // The alloc bit obj_allocator's cache flush publishes for a real allocation. Pages built
        // by hand otherwise carry an empty alloc bitmap, which hides what evacuation does to them.
        void allocate(uint16_t i) {
            _pg->store_alloc_word(i, _pg->load_alloc_word(i) | (1ull << (i % 64)));
        }
        // Hands the page a fresh mark bitmap, the way the pruner does between cycles
        void next_cycle() { _pg->next_cycle(bits().mark_bits(_pg->block_cnt()).data()); }
    };
}
