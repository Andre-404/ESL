#pragma once
#include <windows.h>

#include "os-memory.h"

namespace gc::detail::os {
    namespace {
        std::size_t alloc_granularity() {
            static const std::size_t g = [] {
                auto si = SYSTEM_INFO {};
                GetSystemInfo(&si);
                return std::size_t(si.dwAllocationGranularity);
            }();
            return g;
        }
    }

    void* reserve(std::size_t bytes, std::size_t align) {
        if (align <= alloc_granularity())
            return VirtualAlloc(nullptr, bytes, MEM_RESERVE, PAGE_NOACCESS);

        for (auto attempt = 0; attempt < 16; ++attempt) {
            auto probe = VirtualAlloc(nullptr, bytes + align, MEM_RESERVE, PAGE_NOACCESS);
            if (!probe) return nullptr;
            VirtualFree(probe, 0, MEM_RELEASE);

            auto aligned = (uintptr_t(probe) + align - 1) & ~uintptr_t(align - 1);
            if (auto p = VirtualAlloc((void*)aligned, bytes, MEM_RESERVE, PAGE_NOACCESS)) return p;
        }
        return nullptr;
    }

    void* map_rw(std::size_t bytes) {
        return VirtualAlloc(nullptr, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    }

    bool commit(void* addr, std::size_t bytes) {
        return VirtualAlloc(addr, bytes, MEM_COMMIT, PAGE_READWRITE) != nullptr;
    }

    bool decommit(void* addr, std::size_t bytes) {
        return VirtualAlloc(addr, bytes, MEM_DECOMMIT, PAGE_READWRITE) != nullptr;
    }

    // MEM_RELEASE frees the whole reservation and requires a zero length, so `bytes` goes
    // unused here; it is in the signature for the POSIX side, which needs it.
    void release(void* p, std::size_t bytes) {
        (void)bytes;
        if (p) VirtualFree(p, 0, MEM_RELEASE);
    }
}
