#include "../ConcurrentGC/customization.h"
#include "objects.h"
#include "../Values/valueHelpersInline.h"

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
                assert(false && "obj_copy called on pinned obj?");
                break;
        }
    }
    // Done during STW, no need to worry about syncronization
    void obj_update_ptrs(managed* m) {
        auto obj = reinterpret_cast<object::rt_obj*>(m);
        switch (obj->type()) {
            case rt_type::ARRAY: {
                auto arr = (rt_arr *)obj;
                if (auto store = arr->get_store())
                    arr->set_store((rt_arr_store*)gc::to_moved_ptr(store));
                break;
            }
            case rt_type::ARRAY_STORAGE_HEADER: {
                auto arr = (rt_arr_store *)obj;
                if(!arr->has_obj()) break;
                auto s = arr->get_data();
                for (auto& v : arr->get_data()) {
                    if (isObj(v)) v = encodeObj((rt_obj*)gc::to_moved_ptr(decodeObj(v)));
                }
                break;
            }
            case rt_type::CLOSURE: {
                auto cl = (rt_closure *)obj;
                for (auto& v : cl->get_env()) {
                    if (isObj(v)) v = encodeObj((rt_obj*)gc::to_moved_ptr(decodeObj(v)));
                }
                break;
            }
            case rt_type::INSTANCE: {
                auto inst = (rt_inst *)obj;
                for (auto& v : inst->get_fields()) {
                    if (isObj(v)) v = encodeObj((rt_obj*)gc::to_moved_ptr(decodeObj(v)));
                }
                break;
            }
            // Keys hash by content so no rehash is needed, the slot store updates its own values
            case rt_type::HASH_MAP: {
                auto map = (rt_hashmap *)obj;
                if (auto ctrl = map->get_ctrl())
                    map->set_ctrl((rt_buffer*)gc::to_moved_ptr(ctrl));
                if (auto slots = map->get_slots())
                    map->set_slots((rt_arr_store*)gc::to_moved_ptr(slots));
                break;
            }
            default: break; // Not traceable
        }
    }
    managed* to_accurate_ptr(size_t w) {
        if (isObj(w)) return decodeObj(w);
        return nullptr;
    }
    uint8_t* to_possible_ptr(size_t w) {
        if (isObj(w)) return (uint8_t*)decodeObj(w);
        if ((w & 0xffff'0000'0000'0000) == 0) return (uint8_t*)w;
        return nullptr;
    }
    size_t ptr_to_word(managed* p) {
        return encodeObj((object::rt_obj*)p);
    }

    void obj_trace(managed* m, function_ref<void(managed*)> cb) {
        auto obj = reinterpret_cast<object::rt_obj*>(m);
        switch (obj->type()) {
            case rt_type::ARRAY: {
                auto arr = (rt_arr *)obj;
                if (arr->get_store()) cb(arr->get_store());
                break;
            }
            case rt_type::ARRAY_STORAGE_HEADER: {
                auto store = (rt_arr_store *)obj;
                if(!store->has_obj()) break;
                for (auto val : store->get_data()) {
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            case rt_type::CLOSURE: {
                auto cl = (rt_closure *)obj;
                for (auto val : cl->get_env()) {
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            case rt_type::INSTANCE: {
                auto inst = (rt_inst *)obj;
                for (auto val : inst->get_fields()) {
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            case rt_type::HASH_MAP: {
                auto map = (rt_hashmap *)obj;
                if (map->get_ctrl()) cb(map->get_ctrl());
                if (map->get_slots()) cb(map->get_slots());
                break;
            }
            default: assert(false && "nontraceable objects should never get to here");
        }
    }
}