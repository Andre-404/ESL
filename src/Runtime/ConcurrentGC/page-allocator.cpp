#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

#include "page-allocator.h"

using namespace gc::detail;

pg_meta* pg_allocator::alloc_pg(size_t block_sz) {
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, config::page_sz, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
    void *page;
    posix_memalign(&page, config::page_sz, config::page_sz);
    memset(page, 0, PAGE_SIZE);
#endif
    return new(page) pg_meta(block_sz);
}

void pg_allocator::dealloc_pg(pg_meta* pg) {
#ifdef _WIN32
    VirtualFree(pg, 0, MEM_RELEASE);
#else
    free((void*)pg);
#endif
}
