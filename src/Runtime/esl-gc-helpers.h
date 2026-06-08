#pragma once
#include "ConcurrentGC/gc-api.h"


namespace gc {
    inline tcb_handle* read_tcb() {
        uint64_t r15_val;

        __asm__ volatile("mov %%r15, %0" : "=r" (r15_val));
        return reinterpret_cast<tcb_handle*>(r15_val);
    }

    inline void set_tcb(tcb_handle* handle) {
        __asm__ volatile("mov %0, %%r15" : : "r" (handle));
    }

    inline void write_b(managed* obj) {
        write_barrier(read_tcb(), obj);
    }

    inline managed* allocate(size_t sz) {
        return alloc(sz, read_tcb());
    }
}
