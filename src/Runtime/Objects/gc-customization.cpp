#include "../ConcurrentGC/customization.h"
#include "objects.h"
#include "../Values/valueHelpersInline.h"

namespace {
    template<typename T>
    T* strip(T* ptr) { return (T*)((size_t)ptr & ~gc::config::color_bit); }

    // Tracing runs alongside the mutators: every slot is loaded exactly once, with acquire because
    // the marker goes on to read the object behind it (see load_raw)
    template<typename T>
    void trace_ptr(T*& slot, trace_fn cb) {
        if (auto ptr = strip(gc::load_raw(slot, std::memory_order_acquire))) cb(ptr);
    }
    void trace_vals(std::span<Value> slots, trace_fn cb) {
        for (auto& slot : slots) {
            auto val = gc::load_raw(slot, std::memory_order_acquire);
            if (gc::is_ref_word(val)) cb(decodeObj(val));
        }
    }
    // Updating is done during STW, after the color flip: every reference slot gets the new color,
    // a null pointer too
    template<typename T>
    void update_ptr(T*& slot) {
        auto ptr = strip(gc::load_raw(slot));
        gc::store_ref(slot, ptr ? (T*)gc::to_moved_ptr(ptr) : nullptr);
    }
    void update_vals(std::span<Value> slots) {
        for (auto& slot : slots) {
            auto val = gc::load_raw(slot);
            if (gc::is_ref_word(val))
                gc::store_ref(slot, encodeObj((rt_obj*)gc::to_moved_ptr(decodeObj(val))));
        }
    }
}

namespace object {
    void rt_arr::gc_trace(trace_fn cb) { trace_ptr(_storage, cb); }
    void rt_arr::gc_update_ptrs() { update_ptr(_storage); }

    void rt_arr_store::gc_trace(trace_fn cb) { if (has_obj()) trace_vals(get_data(), cb); }
    void rt_arr_store::gc_update_ptrs() { if (has_obj()) update_vals(get_data()); }

    void rt_closure::gc_trace(trace_fn cb) { trace_vals(get_env(), cb); }
    void rt_closure::gc_update_ptrs() { update_vals(get_env()); }

    void rt_inst::gc_trace(trace_fn cb) { trace_vals(get_fields(), cb); }
    void rt_inst::gc_update_ptrs() { update_vals(get_fields()); }

    // Keys hash by content so no rehash is needed, the slot store updates its own values
    void rt_hashmap::gc_trace(trace_fn cb) { trace_ptr(_ctrl, cb); trace_ptr(_slots, cb); }
    void rt_hashmap::gc_update_ptrs() { update_ptr(_ctrl); update_ptr(_slots); }
}

namespace gc {
    bool obj_traceable(managed* m) {
        auto type = m->get_type_id();
        return type == +rt_type::ARRAY
            || type == +rt_type::ARRAY_STORAGE_HEADER
            || type == +rt_type::CLOSURE
            || type == +rt_type::INSTANCE
            || type == +rt_type::HASH_MAP;
    }
    size_t obj_size(managed* m) {
        auto obj = reinterpret_cast<object::rt_obj*>(m);
        return obj->get_sz();
    }
    void obj_copy(managed* s, managed* d) {
        auto obj = reinterpret_cast<object::rt_obj*>(s);
        switch (obj->type()) {
            case rt_type::ARRAY:
            case rt_type::CLOSURE:
            case rt_type::INSTANCE:
            case rt_type::STRING:
            case rt_type::HASH_MAP:
            case rt_type::BUFFER:
            case rt_type::ARRAY_STORAGE_HEADER: {
                // POD gets memcpy
                memcpy(d, s, obj->get_sz());
                break;
            }
            case rt_type::FILE:
            case rt_type::MUTEX:
            case rt_type::CHANNEL:
            case rt_type::WAIT_GROUP:
            case rt_type::SOCKET:
                assert(false && "obj_copy called on pinned obj?");
                break;
        }
    }
    void obj_update_ptrs(managed* m) {
        auto obj = reinterpret_cast<object::rt_obj*>(m);
        switch (obj->type()) {
            case rt_type::ARRAY: ((rt_arr *)obj)->gc_update_ptrs(); break;
            case rt_type::ARRAY_STORAGE_HEADER: ((rt_arr_store *)obj)->gc_update_ptrs(); break;
            case rt_type::CLOSURE: ((rt_closure *)obj)->gc_update_ptrs(); break;
            case rt_type::INSTANCE: ((rt_inst *)obj)->gc_update_ptrs(); break;
            case rt_type::HASH_MAP: ((rt_hashmap *)obj)->gc_update_ptrs(); break;
            default: break; // Not traceable
        }
    }
    managed* to_accurate_ptr(size_t w) {
        if (is_ref_word(w)) return decodeObj(w);
        return nullptr;
    }
    uint8_t* to_possible_ptr(size_t w) {
        if (is_ref_word(w)) return (uint8_t*)decodeObj(w);
        if ((w >> 49) == 0) return (uint8_t*)(w & ~config::color_bit);
        return nullptr;
    }
    size_t ptr_to_word(managed* p) {
        return encodeObj((object::rt_obj*)p);
    }

    void obj_trace(managed* m, function_ref<void(managed*)> cb) {
        auto obj = reinterpret_cast<object::rt_obj*>(m);
        switch (obj->type()) {
            case rt_type::ARRAY: ((rt_arr *)obj)->gc_trace(cb); break;
            case rt_type::ARRAY_STORAGE_HEADER: ((rt_arr_store *)obj)->gc_trace(cb); break;
            case rt_type::CLOSURE: ((rt_closure *)obj)->gc_trace(cb); break;
            case rt_type::INSTANCE: ((rt_inst *)obj)->gc_trace(cb); break;
            case rt_type::HASH_MAP: ((rt_hashmap *)obj)->gc_trace(cb); break;
            default: assert(false && "nontraceable objects should never get to here");
        }
    }
}
