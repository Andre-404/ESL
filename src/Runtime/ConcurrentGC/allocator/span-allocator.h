#pragma once
#include <algorithm>
#include <atomic>
#include <mutex>

#include "../gc-config.h"
#include "radix-tree.h"

namespace gc::detail {
    // Hands out runs of pages from the heap's address space and backs them with memory
    class span_allocator {
        std::mutex _mtx;
        uint8_t* _base;
        void* _meta;
        radix_tree _tree;
        bitmap _committed; // one bit per commit granule, not per page
        std::atomic<std::size_t> _in_use;         // granules currently allocated
        std::atomic<std::size_t> _in_use_peak {0}; // most granules allocated since the scavenger last looked
        std::atomic<std::size_t> _resident;       // granules physically backed
        int64_t _scavenge_idx; // Idx in granules

        bool ensure_committed(std::size_t start, std::size_t n);
        void* claim_span(std::size_t start, std::size_t n);
        void free(void* addr, std::size_t npages);

        std::size_t page_idx(const void* p) const {
            return std::size_t((const uint8_t*)p - _base) / config::page_sz;
        }

        // Lets tests drive the scavenge cursor. Same arrangement as radix_tree_test_peer.
        friend struct span_allocator_test_peer;

        class free_batch {
            span_allocator& _pa;
            std::unique_lock<std::mutex> _lock;
        public:
            free_batch(span_allocator& pa) : _pa(pa), _lock(_pa._mtx) {}
            ~free_batch() {
                _pa._tree.flush_free();
            }
            void add(void* addr, std::size_t npages) {
                if (!addr || npages == 0) return;
                _pa.free(addr, npages);

                int64_t pidx = _pa.page_idx(addr) + npages;
                _pa._scavenge_idx = std::max<int64_t>(_pa._scavenge_idx, (pidx - 1) / config::commit_granule);
            }
        };

    public:
        // base is the heap_max_sz of address space the pages are handed out of
        explicit span_allocator(uint8_t* base);
        ~span_allocator();
        span_allocator(const span_allocator&) = delete;
        span_allocator& operator=(const span_allocator&) = delete;

        void* alloc(std::size_t npages);
        // Like alloc, but the run starts on an align_pages boundary (a power of two <= 64,
        // with npages <= align_pages)
        void* alloc_aligned(std::size_t npages, std::size_t align_pages);

        free_batch begin_free() { return { *this }; }

        uint8_t* base() const { return _base; }

        // Decommits up to max_granules free granules, walking down from the highest freed one
        std::size_t scavenge(std::size_t max_granules);
        std::size_t in_use() const { return _in_use.load(std::memory_order_relaxed); }
        // The scavenger only looks now and then, a spike between two looks still has to count
        std::size_t take_in_use_peak() { return _in_use_peak.exchange(in_use(), std::memory_order_relaxed); }
        std::size_t resident() const { return _resident.load(std::memory_order_relaxed); }
    };
}
