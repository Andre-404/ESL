#pragma once

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
    void obj_destroy(managed* obj);
    // Unpinned objects are moved as raw bytes, a whole block at a time
}