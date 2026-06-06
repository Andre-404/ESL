#pragma once

#include "managed.h"
#include <functional>

namespace gc {
    inline size_t word_to_ptr(size_t word) { return 0;}
    inline size_t obj_size(managed* obj) { return 0; }
    inline bool obj_traceable(managed* obj) { return false; }
    inline void obj_trace(managed* obj, const std::function<void(managed*)>& trace) {}
    inline void obj_destroy(managed* obj) {}
    // Extension for copying gc
    inline void obj_copy(managed* src, managed* dest) {};
    inline void obj_update_ptrs(managed* obj) {};
}