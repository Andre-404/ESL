#pragma once
#include <assert.h>
#include <atomic>
#include <cstring>
#include <utility>

#include "gc-config.h"
#include "page-allocator.h"
#include "tstack.h"


namespace gc::detail {
    // Deliberately not thread safe
    class pg_list {
        pg_meta* _start;
        std::atomic<bool> _maybe_has_pages;
    public:
        pg_list() : _start(nullptr), _maybe_has_pages(false) {}

        void push(pg_meta* start) {
            assert(start);
            auto end = start;
            while (end->next()) end = end->next();

            end->link(_start);
            _start = start;
            _maybe_has_pages.store(true, std::memory_order_release);
        }

        pg_meta* pop() {
            auto pg = std::exchange(_start, _start ? _start->next() : nullptr);
            _maybe_has_pages.store(_start != nullptr, std::memory_order_relaxed);
            if (pg) pg->unlink();
            return pg;
        }

        template<typename F>
        void mutate(F mutator) {
            _start = mutator(_start);
            _maybe_has_pages.store(_start != nullptr, std::memory_order_release);
        }

        bool approx_empty() {
            return !_maybe_has_pages.load(std::memory_order_acquire);
        }
    };

    class pg_manager {
        pg_allocator _allocator;
        tstack<pg_meta> _pending_free;
        std::mutex _part_mtx;
        // +1 for big objects
        std::array<pg_list, config::szclass_cnt+1> _partial;
        // For sorting before pg allocator handoff
        std::array<pg_meta*, config::heap_max_sz / config::free_batch_sz> _buckets;

        class batch {
            pg_meta* _start;
            pg_meta* _end;
            tstack<pg_meta>& _sink;

        public:
            batch(tstack<pg_meta>& sink) : _start(nullptr), _end(nullptr), _sink(sink) {}

            ~batch() { if (_end) _sink.lfpush_range(_start, _end); }
            
            void add(pg_meta* pg) {
                if (!_end) _end = pg;
                pg->link(_start);
                _start = pg;
            }
        };
    public:
        pg_manager() : _buckets(), _partial() {}

        // Can't use head_from_ptr immediatelly since ptr might land outside of allocated meta array
        pg_meta* pg_from_ptr(void* ptr) {
            auto pg = pg_meta::pg_from_ptr(ptr);
            return _allocator.pg_active(pg) ? pg_meta::head_from_ptr(ptr) : nullptr;
        }

        pg_meta* new_pg(size_t obj_sz);

        // Thread safe
        void transfer_ownership(pg_meta* first) {
            if (!first) return;
            auto _ = std::lock_guard { _part_mtx };
            _partial[first->szclass()].push(first);
        }

        batch start_batch() { return { _pending_free }; }

        template<typename F>
        void mutate_owned(F mutator) {
            auto _ = std::lock_guard { _part_mtx };
            for (auto& list : _partial) list.mutate(mutator);
        }

        void free_pgs();

        gc_bits& bits() { return _allocator.bits(); }
    };
}