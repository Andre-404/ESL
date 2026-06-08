#pragma once
#include <cstdint>

#include "TCB.h"

namespace gc {
    void init_gc(uint8_t& flag);
    [[nodiscard]] tcb_handle* create_tcb(void* start_args, size_t arg_cnt);
    void init_thd(tcb_handle* handle, void* stack_start);
    void delete_tcb(tcb_handle* handle);
    void register_root(size_t* root);
    void enter_blocked(tcb_handle* handle);
    void exit_blocked(tcb_handle* handle);
    [[nodiscard]] managed* alloc(size_t sz, tcb_handle* handle);

    void write_barrier(tcb_handle* handle, managed* obj);
    void poll_safepoint(tcb_handle* handle);
    // These two functions exist because their check will be inlined into llvm for better perf
    // But the above functions are still needed for some parts of ESL's object handling
    void exec_at_safepoint(tcb_handle* handle);
    void push_wbbuf(tcb_handle* handle);

    // For object handling
    managed* to_moved_ptr(managed* obj);
}
