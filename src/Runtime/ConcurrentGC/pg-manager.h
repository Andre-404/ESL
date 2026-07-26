#pragma once
#include <assert.h>
#include <atomic>
#include <cstring>

#include "gc-config.h"
#include "page-allocator.h"
#include "tstack.h"


namespace gc::detail {
    class ts_cache {
        class l2_cache {
            std::array<pg_meta*, config::l2_sz> _data;
        public:
            l2_cache() {
                memset(_data.data(), 0, sizeof(pg_meta*)*config::l2_sz);
            }

            pg_meta* lookup(size_t idx) {
                auto ref = std::atomic_ref { _data.at(idx) };
                return ref.load(std::memory_order_acquire);
            }

            void set(size_t idx, pg_meta* data) {
                auto ref = std::atomic_ref { _data.at(idx) };
                ref.store(data, std::memory_order_release);
            }
        };

        std::array<l2_cache*, config::l1_sz> _top_level;
        uintptr_t _base;

        l2_cache& get_or_create_l2(size_t idx);
        l2_cache* get_l2(size_t idx) {
            auto ref = std::atomic_ref { _top_level.at(idx) };
            auto existing = ref.load(std::memory_order_acquire);
            return existing;
        }

        // Indexed by page number within the heap reservation, not by absolute address:
        // l1_bits + l2_bits only span the page index now, so absolute addresses would
        // alias distinct pages onto the same slot.
        std::pair<size_t, size_t> get_offset(uintptr_t ptr) const {
            auto idx = (ptr - _base) >> config::pg_bits;
            auto l1_idx = (idx >> config::l2_bits) & (config::l1_sz - 1);
            auto l2_idx = idx & (config::l2_sz - 1);
            return { l1_idx, l2_idx };
        }

        void store(uintptr_t ptr, pg_meta* data) {
            auto [l1_idx, l2_idx] = get_offset(ptr);
            get_or_create_l2(l1_idx).set(l2_idx, data);
        }

        class cache_iter {
            size_t _l1_idx;
            size_t _l2_idx;
            l2_cache* _l2_cache;
            ts_cache& _cache;

            void next_l2() {
                while (_l1_idx < config::l1_sz) {
                    if ((_l2_cache = _cache.get_l2(_l1_idx))) break;
                    _l1_idx++;
                }
            }
        public:
            cache_iter(ts_cache& cache) : _l1_idx(0), _l2_idx(0), _l2_cache(nullptr), _cache(cache) {
                next_l2();
            }
            cache_iter& operator++() {
                _l2_idx++;
                if (_l2_idx >= config::l2_sz) {
                    _l2_idx = 0;
                    _l1_idx++;
                    next_l2();
                }
                return *this;
            }

            pg_meta* operator*() const { return _l2_cache ? _l2_cache->lookup(_l2_idx) : nullptr; }
            bool operator==(std::default_sentinel_t) const { return _l1_idx == config::l1_sz; }

            cache_iter& begin() { return *this; }
            std::default_sentinel_t end() const { return {}; }
        };
    public:
        explicit ts_cache(uintptr_t base) : _base(base) {
            memset(_top_level.data(), 0, sizeof(l2_cache*) * config::l1_sz);
        }

        void add(uintptr_t pos, pg_meta* pg) {
            store(pos, pg);
        }
        void remove(pg_meta* pg) {
            store(uintptr_t(pg), nullptr);
        }

        pg_meta* get_pg(uintptr_t ptr) {
            auto [l1_idx, l2_idx] = get_offset(ptr);
            auto l2 = get_l2(l1_idx);
            if (!l2) return nullptr;
            return l2->lookup(l2_idx);
        }

        cache_iter get_iter() { return cache_iter { *this }; }
    };


    class pg_list {
        std::mutex _mtx;
        pg_meta* _start;
         std::atomic<bool> _maybe_has_pages;
    public:
        pg_list() : _start(nullptr), _maybe_has_pages(false) {}

        // We could compute end somewhere else but then we'd need to do it again for counting
        void push(pg_meta* start) {
            assert(start);
            auto end = start;
            while (end->next()) end = end->next();

            auto lk = std::lock_guard { _mtx };
            end->link(_start);
            _start = start;
            _maybe_has_pages.store(true, std::memory_order_release);
        }

        pg_meta* pop() {
             if (!_maybe_has_pages.load(std::memory_order_acquire)) return nullptr;
            auto lk = std::lock_guard { _mtx };
            if (!_start) { 
                _maybe_has_pages.store(false, std::memory_order_relaxed);
                return nullptr;
            }
            auto tmp = _start;
            _start = tmp->next();
            if (!_start) _maybe_has_pages.store(false, std::memory_order_relaxed);
            tmp->unlink();
            return tmp;
        }

        template<typename F>
        void mutate(F mutator) {
            auto lk = std::lock_guard { _mtx };
            _start = mutator(_start);
            _maybe_has_pages.store(_start != nullptr, std::memory_order_release);
        }
    };

    class pg_manager {
        pg_allocator _allocator;
        tstack<pg_meta> _pending_free;
        // +1 for big objects
        std::array<pg_list, config::szclass_cnt+1> _partial;
        ts_cache _active;
    public:
        // _allocator is declared first, so its reservation exists before _active indexes on it
        pg_manager() : _active(uintptr_t(_allocator.heap_base())) {}

        pg_meta* pg_from_ptr(uintptr_t ptr) { return _active.get_pg(ptr); }

        pg_meta* get_new_pg(uint8_t sz_class);

        pg_meta* get_big_pg(size_t obj_sz);

        // Thread safe
        void transfer_ownership(pg_meta* first) {
            if (!first) return;
            auto sz_class = config::sz_to_class(first->block_sz());
            _partial[sz_class].push(first);
        }

        void schedule_free(pg_meta* pg);

        template<typename F>
        void mutate_owned(F mutator) {
            for (auto& list : _partial) list.mutate(mutator);
        }

        template<typename F>
        void foreach_active_pg(F func) {
            for (auto pg : _active.get_iter()) if(pg) func(pg);
        }

        void free_pgs();
    };
}