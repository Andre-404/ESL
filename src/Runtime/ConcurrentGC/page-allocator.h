#pragma once
#include "pg-meta.h"


namespace gc::detail {
    class pg_allocator {
        public:
        pg_allocator() = default;

        pg_meta* alloc_pg(size_t block_sz, size_t num_pgs);
        void dealloc_pgs(pg_meta* start, pg_meta* end);
    };
}