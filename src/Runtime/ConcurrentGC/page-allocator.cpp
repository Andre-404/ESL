#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

#include "page-allocator.h"

using namespace gc::detail;

// TODO: handle rare case of failure when allocating

pg_meta* pg_allocator::alloc_pg(size_t block_sz, size_t num_pgs) {
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, config::page_sz * num_pgs, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
    void *page;
    posix_memalign(&page, config::page_sz, config::page_sz);
    memset(page, 0, config::page_sz*num_pgs);
#endif
    return new(page) pg_meta(block_sz);
}

void pg_allocator::dealloc_pgs(pg_meta* start, pg_meta* end) {
#ifdef _WIN32
    for (auto pg = start; pg != end; pg = start->next()) VirtualFree(pg, 0, MEM_RELEASE);
    VirtualFree(end, 0, MEM_RELEASE);
#else
    for (auto pg = start; pg != end;) {
        auto tmp = pg->next();
        free(pg);
        pg = tmp;
    }
    free(end);
#endif
}
