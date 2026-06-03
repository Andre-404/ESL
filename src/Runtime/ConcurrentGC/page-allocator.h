#pragma once
#include "pg-meta.h"


namespace gc::detail {
    class pg_allocator {
        public:
        pg_allocator() = default;

        pg_meta* alloc_pg(size_t block_sz);
        void dealloc_pg(pg_meta* pg);
    };
}