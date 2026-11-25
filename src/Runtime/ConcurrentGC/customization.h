#pragma once
#include <cstddef>

#include "managed.h"
#include "support.h"


namespace gc {
    uint8_t* to_possible_ptr(size_t word);
    managed* to_accurate_ptr(size_t word);
    size_t ptr_to_word(managed* ptr);
    size_t obj_size(managed* obj);
    bool obj_traceable(managed* obj);
    // Loads every slot with read_ref(slot, std::memory_order_acquire) to heal them
    void obj_trace(managed* obj, function_ref<void(managed*)> trace);
    bool obj_is_pinned(managed* obj);

    class tcb_handle;
    // Runs cleanup(arg) on the cleanup runner, a running mutator whose handle is thd (see add_cleanup)
    void invoke_cleanup(tcb_handle* thd, size_t cleanup, size_t arg);
}