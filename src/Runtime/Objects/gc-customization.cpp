#include "../ConcurrentGC/customization.h"
#include "objects.h"
#include "../Values/valueHelpersInline.cpp"

namespace gc {
    bool obj_traceable(managed* m) {
        using ty = object::ObjType;
        auto type = m->get_type_id();
        return type == +ty::ARRAY
            || type == +ty::CLOSURE
            || type == +ty::INSTANCE
            || type == +ty::HASH_MAP;
    }
    size_t obj_size(managed* m) {
        auto obj = reinterpret_cast<object::Obj*>(m);
        return obj->getSize();
    }
    void obj_copy(managed* s, managed* d) {
        auto obj = reinterpret_cast<object::Obj*>(s);
        switch (obj->type()) {
            case ObjType::ARRAY:
            case ObjType::CLOSURE:
            case ObjType::INSTANCE:
            case ObjType::STRING:
            case ObjType::CLASS:
                // TODO: map needs to become pod
            case ObjType::HASH_MAP:
            case ObjType::ARRAY_STORAGE_HEADER: {
                // POD gets memcpy
                memcpy(d, s, obj->getSize());
                break;
            }
            case ObjType::DEALLOCATED: break;
            case ObjType::FILE:
            case ObjType::MUTEX:
            case ObjType::CHANNEL:
            case ObjType::WAIT_GROUP:
                assert(false && "obj_copy called on pinned obj?");
                break;
        }
    }
    void obj_update_ptrs(managed* m) {
        auto obj = reinterpret_cast<object::Obj*>(m);
        switch (obj->type()) {
            case ObjType::ARRAY: {
                auto arr = (ObjArray *)obj;
                // If array doesn't contain objects(arrays of nums are common) don't try to mark contents of arr
                arr->storage = (ObjArrayStorage*)gc::to_moved_ptr(arr->storage);
                if(!arr->containsObjects) break;
                Value* data = arr->getData();
                for(int i = 0; i < arr->size; i++){
                    auto val = data[i];
                    if (isObj(val))
                        data[i] = encodeObj((Obj*)gc::to_moved_ptr(decodeObj(val)));
                }
                break;
            }
            case ObjType::CLOSURE: {
                auto cl = (ObjClosure *)obj;
                for (int i = 0; i < cl->freevarCount; i++) {
                    auto val = cl->getFreevarArr()[i];
                    if (isObj(val))
                        cl->getFreevarArr()[i] = encodeObj((Obj*)gc::to_moved_ptr(decodeObj(val)));
                }
                break;
            }
            case ObjType::INSTANCE: {
                auto inst = (ObjInstance *)obj;
                Value* fields = inst->getFields();
                for (int i = 0; i < inst->fieldArrLen; i++) {
                    auto val = fields[i];
                    if (isObj(val))
                        fields[i] = encodeObj((Obj*)gc::to_moved_ptr(decodeObj(val)));
                }
                break;
            }
            // TODO: i think we're gonna need to completely rehash map on every move? right now it can't be allocated so its fine
            case ObjType::HASH_MAP: {
                auto map = (ObjHashMap *)obj;
                for (auto &field: map->fields) {
                }
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
        return encodeObj((object::Obj*)p);
    }

    void obj_trace(managed* m, std::function<void(managed*)>& cb) {
        auto obj = reinterpret_cast<object::Obj*>(m);
        switch (obj->type()) {
            case ObjType::ARRAY: {
                auto arr = (ObjArray *)obj;
                // If array doesn't contain objects(arrays of nums are common) don't try to mark contents of arr
                cb(arr->storage);
                if(!arr->containsObjects) break;
                Value* data = arr->getData();
                arr->containsObjects = 0;
                for(int i = 0; i < arr->size; i++){
                    auto val = data[i];
                    if (isObj(val)){
                        cb(decodeObj(val));
                        arr->containsObjects = 1;
                    }
                }
                break;
            }
            case ObjType::CLOSURE: {
                auto cl = (ObjClosure *)obj;
                for (int i = 0; i < cl->freevarCount; i++) {
                    auto val = cl->getFreevarArr()[i];
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            case ObjType::INSTANCE: {
                auto inst = (ObjInstance *)obj;
                Value* fields = inst->getFields();
                for (int i = 0; i < inst->fieldArrLen; i++) {
                    auto val = fields[i];
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            case ObjType::HASH_MAP: {
                auto map = (ObjHashMap *)obj;
                for (auto &field: map->fields) {
                    cb(field.first);
                    auto val = field.second;
                    if (isObj(val)) cb(decodeObj(val));
                }
                break;
            }
            default: assert(false && "nontraceable objects should never get to here");
        }
    }
}