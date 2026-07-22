#include "customization-helper.h"


namespace gc {
    using namespace detail;
    bool   obj_traceable(managed* m)        { return test_custom::hooks.obj_traceable(m); }
    size_t obj_size(managed* m)             { return test_custom::hooks.obj_size(m); }
    void   obj_copy(managed* s, managed* d) { test_custom::hooks.obj_copy(s, d); }
    void   obj_update_ptrs(managed* m)      { test_custom::hooks.obj_update_ptrs(m); }
    managed* to_accurate_ptr(size_t w)      { return test_custom::hooks.to_accurate_ptr(w); }
    uint8_t* to_possible_ptr(size_t w)      { return test_custom::hooks.to_possible_ptr(w); }
    size_t ptr_to_word(managed* p)          { return test_custom::hooks.ptr_to_word(p); }

    // obj_trace receives the mark callback by reference so the marker can
    // observe child-pushes mid-trace. The hooks still take a std::function&, so wrap the
    // function_ref the gc now passes; it forwards to the live callback either way.
    void obj_trace(managed* m, function_ref<void(managed*)> trace) {
        std::function<void(managed*)> cb = [&](managed* o) { trace(o); };
        test_custom::hooks.obj_trace(m, cb);
    }
}