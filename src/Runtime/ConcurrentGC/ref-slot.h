#pragma once
#include <atomic>
#include <cstddef>
#include <type_traits>

#include "managed.h"

// A reference slot is memory that can hold a pointer to a managed object: a field of a managed
// object, an element of its trailing array, a registered root.
// Mutators and the collector access slots concurrently, so once its object is constructed a slot is
// only ever loaded and stored atomically. A plain access that races lets the compiler load the
// slot twice and act on two different values, or split a store.
//   load_raw, store_raw  the collector's own access, never gets a barrier
//   read_ref, store_ref  a mutator's access, might include barriers in the future
namespace gc {
    // A pointer to a managed object, or a word that customization.h's to_accurate_ptr() decides about
    template<typename T>
    concept ref_slot = std::is_same_v<T, size_t> ||
        (std::is_pointer_v<T> && std::is_base_of_v<managed, std::remove_pointer_t<T>>);

    // Relaxed is enough when the value is used on its own or nothing else is running (stw). The
    // concurrent marker passes acquire: it reads through the pointer it loaded, and acquire pairs
    // with the release fence make_gc puts after the constructor
    template<ref_slot T>
    [[nodiscard, gnu::always_inline]] inline T load_raw(
        T& slot, std::memory_order order = std::memory_order_relaxed
    ) noexcept {
        return std::atomic_ref<T> { slot }.load(order);
    }

    template<ref_slot T>
    [[gnu::always_inline]] inline void store_raw(
        T& slot, std::type_identity_t<T> value, std::memory_order order = std::memory_order_relaxed
    ) noexcept {
        std::atomic_ref<T> { slot }.store(value, order);
    }

    // Same thing as gc_read_val/gc_read_ptr in jitted code (runtime_bridge::build_ir_helpers)
    template<ref_slot T>
    [[nodiscard, gnu::always_inline]] inline T read_ref(T& slot) noexcept {
        return load_raw(slot, std::memory_order_relaxed);
    }

    // Same thing as gc_store_val/gc_store_ptr. The write barrier stays its own call after it
    template<ref_slot T>
    [[gnu::always_inline]] inline void store_ref(T& slot, std::type_identity_t<T> value) noexcept {
        store_raw(slot, value, std::memory_order_relaxed);
    }
}
