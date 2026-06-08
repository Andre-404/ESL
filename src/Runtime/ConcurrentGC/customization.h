#pragma once

#include "managed.h"
#include <functional>

namespace gc {
    uint8_t* to_possible_ptr(size_t word);
    managed* to_accurate_ptr(size_t word);
    size_t ptr_to_word(managed* ptr);
    size_t obj_size(managed* obj);
    bool obj_traceable(managed* obj);
    void obj_trace(managed* obj, std::function<void(managed*)>& trace);
    void obj_destroy(managed* obj);
    // Extension for copying gc
    void obj_copy(managed* src, managed* dest);
    void obj_update_ptrs(managed* obj);
}