#pragma once
#include <assert.h>
#include <atomic>
#include <functional>
#include <cstring>

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

        l2_cache& get_or_create_l2(size_t idx);
        l2_cache* get_l2(size_t idx) {
            auto ref = std::atomic_ref { _top_level.at(idx) };
            auto existing = ref.load(std::memory_order_acquire);
            return existing;
        }

        static std::pair<size_t, size_t> get_offset(uintptr_t ptr) {
            auto offset = ptr;
            auto l1_idx = (offset >> (config::pg_bits + config::l2_bits)) & (config::l1_sz - 1);
            auto l2_idx = (offset >> config::pg_bits) & (config::l2_sz - 1);
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
        ts_cache() {
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
        std::atomic<size_t> _sz_hint;
    public:
        pg_list() : _start(nullptr), _sz_hint(0) {}

        // We could compute end somewhere else but then we'd need to do it again for counting
        void push(pg_meta* start) {
            assert(start);
            auto to_add = 1;
            auto end = start;
            while (end->next()) {
                end = end->next();
                to_add++;
            }
            _sz_hint.fetch_add(to_add, std::memory_order_release);

            auto lk = std::lock_guard { _mtx };
            end->link(_start);
            _start = start;
        }

        pg_meta* pop() {
            // Doesn't have to be correct, only used as a fast path since these lists will mostly sit empty
            // And even if they transiently get a member we don't care, it will get used by another allocation
            if (_sz_hint.load(std::memory_order_relaxed) == 0) return nullptr;
            auto lk = std::lock_guard { _mtx };
            if (!_start) return nullptr;
            --_sz_hint;
            auto tmp = _start;
            _start = tmp->next();
            tmp->unlink();
            return tmp;
        }

        template<typename F>
        void mutate(F mutator) {
            auto lk = std::lock_guard { _mtx };
            _start = mutator(_start);
            // TODO: inefficient
            auto tmp = _start;
            _sz_hint = 0;
            while (tmp) {
                _sz_hint.fetch_add(1, std::memory_order_relaxed);
                tmp = tmp->next();
            }
        }
    };

    class pg_manager {
        pg_allocator _allocator;
        tstack<pg_meta> _empty;
        std::atomic<size_t> _empty_cnt;
        std::array<pg_list, config::szclass_cnt> _partial;
        pg_list _big;
        size_t _empty_limit;
        ts_cache _active;
    public:
        pg_manager() : _empty_limit(0), _active() {}

        void set_empty_limit(size_t in_bytes) { _empty_limit = in_bytes / config::page_sz; }

        pg_meta* pg_from_ptr(uintptr_t ptr) { return _active.get_pg(ptr); }

        pg_meta* get_new_pg(uint8_t sz_class);

        pg_meta* get_big_pg(size_t obj_sz);

        // Thread safe
        void release_pgs(pg_meta* first) {
            if (!first) return;
            auto sz_class = config::sz_to_class(first->block_sz());
            if (sz_class == -1) {
                _big.push(first);
                return;
            }
            _partial[sz_class].push(first);
        }

        void dealloc_pgs(pg_meta* head);

        template<typename F>
        void mutate_owned(F mutator) {
            for (auto& list : _partial) list.mutate(mutator);
            _big.mutate(mutator);
        }

        template<typename F>
        void foreach_active_pg(F func) {
            for (auto pg : _active.get_iter()) if(pg) func(pg);
        }
    };
}