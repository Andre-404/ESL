#pragma once

#include "pg-meta.h"
#include "obj-allocator.h"

namespace gc::detail {
    class szclass_allocator {
        pg_meta* _start;
        obj_allocator _allocator;

        bool find_next_free() {
            auto pg = cur()->next();
            if (!pg) return false;
            pg->reach();
            _allocator = obj_allocator { *pg };
            return true;
        }

        pg_meta* cur() const { return _allocator.get_pg(); }

        [[gnu::cold]] managed* alloc_slow() {
            _allocator.flush_cache();
            managed* res = nullptr;
            do {
                if (!find_next_free()) [[unlikely]] return nullptr;
                res = _allocator.allocate();
            } while(res == nullptr);
            return res;
        }
    public:
        szclass_allocator() : _start(nullptr) {};

        [[nodiscard, gnu::hot, gnu::always_inline]] managed* alloc() {
            if (!cur()) [[unlikely]] return nullptr;

            if (auto res = _allocator.allocate()) [[likely]] return res;
            return alloc_slow();
        }

        void push_pg(pg_meta* pg) {
            // Dont break an existing link
            assert(!(cur() && cur()->next()));
            pg->unlink(); // Sanity check
            pg->reach();
            if (cur()) cur()->link(pg);
            else _start = pg;
            _allocator = obj_allocator { *pg };
        }

        void flush_alloc_cache() {
            if (cur()) _allocator.flush_cache();
        }

        template<typename F>
        void mutate(F&& mutator) {
            _start = mutator(_start);
            if (_start) { _allocator = obj_allocator { *_start }; _start->reach(); }
            else _allocator = obj_allocator {};
        }
    };
}