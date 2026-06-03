#pragma once
#include <atomic>
#include <functional>

#include "page-allocator.h"
#include "tstack.h"


namespace gc::detail {
    class ts_cache {
            class l2_cache {
                std::array<pg_meta*, config::l2_sz> _data;
            public:
                l2_cache() = default;

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
                cache_iter(ts_cache& cache) : _l1_idx(0), _l2_idx(0), _l2_cache(nullptr), _cache(cache) {}
                cache_iter& operator++() {
                    if (_l2_idx == config::l2_sz) {
                        _l2_idx = 0;
                        _l1_idx++;
                        next_l2();
                    } else _l2_idx++;
                    return *this;
                }

                pg_meta* operator*() const { return _l2_cache ? _l2_cache->lookup(_l2_idx) : nullptr; }
                bool operator==(std::default_sentinel_t) const { return _l1_idx == config::l1_sz; }

                cache_iter& begin() { return *this; }
                std::default_sentinel_t end() const { return {}; }
            };
        public:
            ts_cache() = default;

            void add(pg_meta* pg) {
                store((uintptr_t)pg, pg);
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
    class pg_manager {
        pg_allocator _allocator;
        tstack<pg_meta> _empty;
        std::atomic<size_t> _empty_cnt;
        std::array<tstack<pg_meta>, config::szclass_cnt> _partial;
        std::function<void(pg_meta*)> _cleanup;
        ts_cache _active;
    public:
        template<typename F>
        pg_manager(F cleanup) : _active(), _cleanup(cleanup) {}

        pg_meta* pg_from_ptr(uintptr_t ptr) {
            return _active.get_pg(ptr);
        }

        pg_meta* get_new_pg(uint8_t sz_class);

        void release_pgs(uint8_t sz_class, pg_meta* first, pg_meta* last) {
            _partial[sz_class].lfpush_range(first, last);
        }

        // Note: this must never happen during the mark phase since we could be trying to read the page meta from a dead pg
        void dealloc_pgs(pg_meta* head, size_t empty_limit);

        // Note: these functions are not thread safe
        // TODO: add copy partials, update partials or expose iterator
        template<typename F>
        void prune_partial(size_t empty_limit, F prune) {
            for (auto& stack : _partial) {
                auto [empty, retain] = prune(stack.peek());
                stack.reset_head(retain);
                dealloc_pgs(empty, empty_limit);
            }
        }

        template<typename F>
        void foreach_active_pg(F func) {
            for (auto pg : _active.get_iter()) func(pg);
        }
    };
}