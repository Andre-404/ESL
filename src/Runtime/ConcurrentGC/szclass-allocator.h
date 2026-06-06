#pragma once

#include "pg-meta.h"
#include "obj-allocator.h"

namespace gc::detail {
    class szclass_allocator {
        pg_meta* _start;
        obj_allocator _allocator;

        bool find_next_free() {
            auto pg = cur();
            while (pg->live_count() == pg->block_cnt() && pg->next()) pg = pg->next();
            _allocator = obj_allocator { *pg };
            return pg->live_count() != pg->block_cnt();
        }

        pg_meta* cur() const { return _allocator.get_pg(); }
    public:
        szclass_allocator() : _start(nullptr) {};

        [[nodiscard]] managed* alloc() {
            if (!_allocator.get_pg()) [[unlikely]] return nullptr;

            if (auto res = _allocator.allocate()) [[likely]] return res;

            if (!find_next_free()) [[unlikely]] return nullptr;
            return _allocator.allocate();
        }

        void push_pg(pg_meta* pg) {
            // Dont break an existing link
            assert(!(_allocator.get_pg() && _allocator.get_pg()->next()));
            pg->unlink(); // Sanity check
            if (_allocator.get_pg()) {
                cur()->link(pg);
                _allocator = obj_allocator { *pg };
                return;
            }
            _start = pg;
            _allocator = obj_allocator { *_start };
        }

        void flush_alloc_cache() {
            if (_allocator.get_pg()) _allocator.flush_cache();
        }

        template<typename F>
        void mutate(F mutator) {
            auto new_start = mutator(_start);
            _start = new_start;
            if (_start) _allocator = obj_allocator { *_start };
        }
    };
}