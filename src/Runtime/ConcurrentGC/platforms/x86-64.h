#pragma once
// IWYU pragma: private, include "platform-specific.h"
#include <cstdint>

// r14 holds the good pointer color and r15 the tcb, so everything that calls these must be built with both reserved
namespace gc::platform {
    inline uint64_t read_color() {
        uint64_t c;
        asm volatile("mov %%r14, %0" : "=r"(c));
        return c;
    }

    inline void write_color(uint64_t c) {
        asm volatile("mov %0, %%r14" : : "r"(c));
    }

    inline void* read_tcb() {
        void *sp;
        asm volatile("mov %%r15, %0" : "=r"(sp));
        return sp;
    }

    inline void set_tcb(void* handle) {
        __asm__ volatile("mov %0, %%r15" : : "r" (handle));
    }
}
