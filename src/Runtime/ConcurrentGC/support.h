#pragma once

#include <type_traits>
#include <utility>

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

// Calls f as const, so a mutable lambda doesn't bind and f can't change what it captured by value
template<class R, class... Args>
class function_ref<R(Args...) const> {
    const void* _obj;
    R (*_fn)(const void*, Args...);
public:
    template<class F>
        requires (!std::is_same_v<std::remove_cvref_t<F>, function_ref> && std::is_invocable_r_v<R, const F&, Args...>)
    function_ref(const F& f) noexcept : _obj(std::addressof(f)),
    _fn(+[](const void* o, Args... a) -> R {
        return (*static_cast<const F*>(o))(std::forward<Args>(a)...);
    }) {}

    R operator()(Args... a) const { return _fn(_obj, std::forward<Args>(a)...); }
};
