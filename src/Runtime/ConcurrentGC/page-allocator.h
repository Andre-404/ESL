#pragma once
#include <atomic>
#include <mutex>
#include <chrono>
#include <cmath>
#include <stop_token>
#include <thread>

#include "gc-bits.h"
#include "gc-config.h"
#include "pg-meta.h"
#include "radix-tree.h"

namespace gc::detail {
    struct scavenge_policy {
        double headroom      = 0.0625; // keep demand*(1+headroom) resident before scavenging
        double thrash_gain   = 0.5;    // widen the band by this * observed refault rate
        double decay_per_sec = std::log(2) / 5;  // demand-peak half-life ~5s
        double base_cpu      = 0.01;   // steady-state scavenger CPU budget
        double max_cpu       = 0.08;   // budget cap when far above goal
        double cpu_gain      = 0.07;   // extra budget per unit of excess/goal
        std::size_t min_retain = 64;   // never scavenge below this many resident granules
                                        // (64 granules is 4 MiB at 8K pages, 32 MiB at 64K)
        double quantum_ns    = 1e6;    // aim for ~1ms of decommit work per wake
        double ewma_alpha    = 0.1;    // smoothing for the measured ns-per-granule
        double init_ns_per_granule = 3000; // first-quantum cost guess
        std::chrono::nanoseconds idle_sleep{ std::chrono::milliseconds(50) };
        std::chrono::nanoseconds max_sleep{ std::chrono::seconds(1) };
    };

    class page_allocator {
        std::mutex _mtx;
        uint8_t* _base;
        void* _meta;
        pg_meta* _pg_headers;
        radix_tree _tree;
        bitmap _committed; // one bit per commit granule, not per page
        std::atomic<std::size_t> _hdrs_committed; // pg headers are never decommited, is that okay?
        std::atomic<std::size_t> _in_use;         // granules currently allocated
        std::atomic<std::size_t> _resident;       // granules physically backed
        int64_t _scavenge_idx; // Idx in granules
        scavenge_policy _policy;
        std::jthread _scavenger;


        bool ensure_committed(std::size_t start, std::size_t n);
        bool commit_headers(std::size_t start, std::size_t n);
        void* claim_span(std::size_t start, std::size_t n);
        void free(void* addr, std::size_t npages);

        std::size_t page_idx(const void* p) const {
            return std::size_t((const uint8_t*)p - _base) / config::page_sz;
        }

        std::size_t scavenge(std::size_t max_granules);
        void scavenge_loop(std::stop_token st);

        // The background thread is otherwise scavenge's only caller, which leaves its cursor
        // arithmetic untestable. Same arrangement as radix_tree_test_peer.
        friend struct page_allocator_test_peer;

        class free_batch {
            page_allocator& _pa;
            std::unique_lock<std::mutex> _lock;
        public:
            free_batch(page_allocator& pa) : _pa(pa), _lock(_pa._mtx) {}
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
        explicit page_allocator(scavenge_policy policy = {});
        ~page_allocator();
        page_allocator(const page_allocator&) = delete;
        page_allocator& operator=(const page_allocator&) = delete;

        void* alloc(std::size_t npages);
        // Like alloc, but the run starts on an align_pages boundary (a power of two <= 64,
        // with npages <= align_pages)
        void* alloc_aligned(std::size_t npages, std::size_t align_pages);

        free_batch begin_free() { return { *this }; }

        uint8_t* base() const { return _base; }
        pg_meta* pgs_base() const { return _pg_headers; }
        bool pg_active(pg_meta* pg) {
            auto off = pg - _pg_headers;
            if (off < 0 || (std::size_t)off >= _hdrs_committed.load(std::memory_order_acquire)) return false;
            return (_pg_headers + off)->is_active();
        }

        // Used by tests
        std::size_t resident() const { return _resident.load(std::memory_order_relaxed); }
    };

    class pg_allocator {
        page_allocator _pages;
        gc_bits _bits;

    public:
        pg_allocator() : _pages(), _bits((uint8_t*)(_pages.pgs_base() + config::total_pages)) {};

        uint8_t* heap_base() const { return _pages.base(); }
        pg_meta* meta_base() const { return _pages.pgs_base(); }
        // The alloc/mark bitmap arena the headers point into; the collector drives its flip.
        gc_bits& bits() { return _bits; }
        bool pg_active(pg_meta* pg) { return _pages.pg_active(pg); }

        pg_meta* alloc_pg(size_t block_sz, size_t num_pgs);
        void free_pgs(pg_meta* start);
    };
}
