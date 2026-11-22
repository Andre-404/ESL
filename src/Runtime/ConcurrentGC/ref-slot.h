#pragma once
#include <atomic>
#include <cstddef>
#include <type_traits>

#include "managed.h"
#include "platform-specific.h"

// A reference slot is memory that can hold a pointer to a managed object: a field of a managed
// object, an element of its trailing array, a registered root.
// Mutators and the collector access slots concurrently, so once its object is constructed a slot is
// only ever loaded and stored atomically. A plain access that races lets the compiler load the
// slot twice and act on two different values, or split a store.
//   load_raw, store_raw  the collector's own access, sees the color bit
//   read_ref, store_ref  a mutator's access (gc-api.h), strips and adds the color. Only works for pointer slots
namespace gc {
    // The color bit a good reference carries in its slot, r14 holds it as is
    [[gnu::always_inline]] inline size_t good_color() { return platform::read_color(); }

    template<typename T>
    concept managed_ptr = std::is_pointer_v<T> && std::is_base_of_v<managed, std::remove_pointer_t<T>>;
    // A pointer to a managed object, or a word that customization.h's to_accurate_ptr() decides about
    template<typename T>
    concept ref_slot = std::is_same_v<T, size_t> || managed_ptr<T>;

    constexpr auto relaxed = std::memory_order_relaxed;

    // Relaxed is enough when the value is used on its own or nothing else is running (stw). The
    // concurrent marker passes acquire: it reads through the pointer it loaded, and acquire pairs
    // with the release fence make_gc puts after the constructor
    template<ref_slot T>
    [[gnu::always_inline]] inline T load_raw(T& slot, std::memory_order order = relaxed) {
        return std::atomic_ref<T> { slot }.load(order);
    }

    template<ref_slot T>
    [[gnu::always_inline]] inline void store_raw(T& slot, std::type_identity_t<T> value) {
        std::atomic_ref<T> { slot }.store(value, relaxed);
    }

    // Slow paths for a reference with the bad color, return it uncolored and fixed (ref-slot.cpp).
    // Leaves with preserve_all, so callers keep their registers across them
    [[gnu::cold, clang::preserve_all]] size_t heal_ptr(size_t* slot, size_t seen);
    [[gnu::cold, clang::preserve_all]] size_t heal_word(size_t* slot, size_t seen);
}
