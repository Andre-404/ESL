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

    template<typename T, typename... Args>
    [[nodiscard]] T* esl_make_gc(size_t extra_bytes, Args&&... args) {
        return make_gc<T>(read_tcb(), extra_bytes, std::forward<Args>(args)...);
    }
}
