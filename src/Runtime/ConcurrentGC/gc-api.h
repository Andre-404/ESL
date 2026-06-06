#pragma once
#include <cstdint>

#include "TCB.h"


namespace gc {
    void init_gc(uint8_t& flag);
    void write_barrier(tcb_handle* handle);
    void poll_safepoint(tcb_handle* handle);
    // These 2 functions exists because the common check for them will be inlined into llvm
    // In the interest of having a complete gc the above functions are also provided which do the check + action
    void exec_at_safepoint(tcb_handle* handle);
    void push_wbbuf(tcb_handle* handle);
}
