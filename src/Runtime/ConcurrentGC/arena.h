#pragma once

#include "pg-manager.h"
#include "szclass-allocator.h"
#include "gc-config.h"


namespace gc::detail {
    class arena {
        std::array<szclass_allocator, config::szclass_cnt> _allocators;
        size_t _debt;
    public:
        arena() : _debt(0) {}
        managed* alloc(size_t sz, pg_manager& manager) {
            auto szclass = config::sz_to_class[sz];
            _debt += sz;
            if (auto res = _allocators[szclass].alloc()) return res;
            _allocators[szclass].push_pg(manager.get_new_pg(szclass));
            return _allocators[szclass].alloc();
        }

        size_t get_debt() const { return _debt; }
        void remove_debt(size_t to_remove) { _debt -= to_remove; }

        void flush_alloc_caches() {
            for (auto& alloc : _allocators) alloc.flush_alloc_cache();
        }
        template<typename F>
        void iter_caches(F consume) {
            for (auto& alloc : _allocators) consume(alloc.list_start());
        }
        template<typename F>
        void prune_caches(F prune) {
            for (auto& alloc : _allocators) alloc.prune(prune);
        }
    };
}