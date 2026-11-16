#pragma once
#include <sys/mman.h>

#include "os-memory.h"

namespace gc::detail::os {
    void* reserve(std::size_t bytes, std::size_t align) {
        auto total = bytes + align;
        auto p = mmap(nullptr, total, PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0
        );
        if (p == MAP_FAILED) return nullptr;

        auto addr = uintptr_t(p);
        auto aligned = (addr + align - 1) & ~uintptr_t(align - 1);
        if (auto prefix = aligned - addr) munmap(p, prefix);
        if (auto suffix = total - (aligned - addr) - bytes)
            munmap((void*)(aligned + bytes), suffix);
        return (void*)aligned;
    }

    void* map_rw(std::size_t bytes) {
        auto p = mmap(nullptr, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        return p == MAP_FAILED ? nullptr : p;
    }

    // madvise(POPULATE_WRITE) would make it eager, and seems to kill perf.
    bool commit(void*, std::size_t) {
        return true;
    }

    bool decommit(void* addr, std::size_t bytes) {
        return madvise(addr, bytes, MADV_DONTNEED) == 0;
    }

    void release(void* p, std::size_t bytes) {
        if (p) munmap(p, bytes);
    }
}
