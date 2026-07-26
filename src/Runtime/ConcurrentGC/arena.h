#pragma once

#include "pg-manager.h"
#include "szclass-allocator.h"
#include "gc-config.h"


namespace gc::detail {
    class arena {
        std::array<szclass_allocator, config::szclass_cnt> _allocators;
        int64_t _debt;
        pg_meta* _big_objs;

        managed* alloc_big(size_t sz, pg_manager& manager) {
            auto res = manager.get_big_pg(pg_meta::header_bytes(sz) + sz);
            if (!res) [[unlikely]] return nullptr;
            res->link(_big_objs);
            _big_objs = res;
            return pg_meta::pg_slots_iter { res, 0 }.get();
        }

        [[gnu::cold]] managed* alloc_slow(size_t szclass, pg_manager& manager) {
            managed* res = nullptr;
            // Need to loop because get_new_pg can in some cases return a fully allocated page from partial
            do {
                if (auto new_pg = manager.get_new_pg(szclass)) 
                    _allocators[szclass].push_pg(new_pg);
                else [[unlikely]] return nullptr; // This should never happen

                res = _allocators[szclass].alloc();
            } while (res == nullptr);
            return res;
        }
    public:
        arena() : _debt(0), _big_objs(nullptr) {}
        [[gnu::hot, gnu::always_inline]] managed* alloc(size_t sz, pg_manager& manager) {
            // TODO: account for fragmentation here
            _debt += sz;
            auto szclass = config::sz_to_class(sz);
            if (szclass == config::large_class) return alloc_big(sz, manager);
            if (auto res = _allocators[szclass].alloc()) return res;
            auto res = alloc_slow(szclass, manager);
            if (!res) _debt -= sz;
            return res;
        }

        int64_t get_debt() const { return _debt; }
        void remove_debt(size_t to_remove) { _debt -= to_remove; }

        void flush_alloc_caches() {
            for (auto& alloc : _allocators) alloc.flush_alloc_cache();
        }
        template<typename F>
        void mutate_owned(F mutator) {
            for (auto& alloc : _allocators) alloc.mutate(mutator);
            _big_objs = mutator(_big_objs);
        }
    };
}