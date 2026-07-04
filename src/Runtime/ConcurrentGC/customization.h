#pragma once

#include "managed.h"
#include <functional>


template<class Sig> class function_ref;

template<class R, class... Args>
class function_ref<R(Args...)> {
    void* _obj;
    R (*_fn)(void*, Args...);
public:
    template<class F>
        requires (!std::is_same_v<std::remove_cvref_t<F>, function_ref>)
    function_ref(F&& f) noexcept : _obj(const_cast<void*>(static_cast<const void*>(std::addressof(f)))),
    _fn(+[](void* o, Args... a) -> R {
        return (*static_cast<std::remove_reference_t<F>*>(o))(std::forward<Args>(a)...);
    }) {}

    R operator()(Args... a) const { return _fn(_obj, std::forward<Args>(a)...); }
};

namespace gc {
    uint8_t* to_possible_ptr(size_t word);
    managed* to_accurate_ptr(size_t word);
    size_t ptr_to_word(managed* ptr);
    size_t obj_size(managed* obj);
    bool obj_traceable(managed* obj);
    void obj_trace(managed* obj, function_ref<void(managed*)> trace);
    void obj_destroy(managed* obj);
    // Extension for copying gc
    void obj_copy(managed* src, managed* dest);
    void obj_update_ptrs(managed* obj);
}