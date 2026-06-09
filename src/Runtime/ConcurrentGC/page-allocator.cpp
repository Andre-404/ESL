#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

#include "page-allocator.h"

using namespace gc::detail;

#ifndef _WIN32
void* alloc_aligned(size_t multiplier) {
    size_t alignment = gc::config::page_sz;
    size_t size = multiplier * alignment;

    size_t total_alloc = size + alignment;
    void* ptr = mmap(NULL, total_alloc, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (ptr == MAP_FAILED) return NULL;

    uintptr_t addr = (uintptr_t)ptr;
    uintptr_t aligned_addr = (addr + (alignment - 1)) & ~(alignment - 1);

    size_t prefix_len = aligned_addr - addr;
    if (prefix_len > 0) munmap(ptr, prefix_len);

    size_t suffix_len = total_alloc - prefix_len - size;
    if (suffix_len > 0) munmap((void*)(aligned_addr + size), suffix_len);

    return (void*)aligned_addr;
}

size_t num_pgs(pg_meta* pg) {
    return is_large_pg(pg) ? ((large_pg_sz(pg->block_sz()) + gc::config::page_sz - 1) / gc::config::page_sz) : 1;
}


#endif

// TODO: handle rare case of failure when allocating

pg_meta* pg_allocator::alloc_pg(size_t block_sz, size_t num_pgs) {
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, config::page_sz * num_pgs, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
    void *page = alloc_aligned(num_pgs);
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
        munmap(pg, num_pgs(pg) * config::page_sz);
        pg = tmp;
    }
    if (end) munmap(end, num_pgs(end) * config::page_sz);
#endif
}
