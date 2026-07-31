#pragma once
#include "../../Includes/unorderedDense.h"
#include "../../common.h"
#include <atomic>
#include <cstring>
#include "../esl-gc-helpers.h"

namespace object {

    enum class rt_type : uint8_t {
        STRING,
        CLOSURE,
        INSTANCE,
        ARRAY,
        ARRAY_STORAGE_HEADER,
        HASH_MAP,
        HASHMAP_STORAGE_HEADER,
        FILE,
        MUTEX,
        CHANNEL,
        WAIT_GROUP,
        SOCKET
    };
    inline constexpr unsigned operator+ (rt_type const val) { return static_cast<byte>(val); }

    class rt_obj : public gc::managed {
    public:
        rt_obj(rt_type type, bool is_pinned) : gc::managed(+type, is_pinned ? gc::move_state::pinned : gc::move_state::none) {}

        rt_type type() { return static_cast<rt_type>(get_type_id()); }
        size_t get_sz();
        string to_str(std::shared_ptr<ankerl::unordered_dense::set<object::rt_obj*>> stack);
    };

    // This is a header which is followed by the bytes of the string
    class rt_string : public rt_obj {
        uint32_t _size;
    public:
        struct hash {
            uint64_t operator()(const object::rt_string* str) const noexcept;
        };

        struct equality {
            bool operator()(object::rt_string* x, object::rt_string* y) const {
                return x->compare(y);
            }
        };
        
        rt_string(uint32_t sz, char* str) : rt_obj(rt_type::STRING, false), _size(sz) {
            strcpy(get_str(), str);
        }

        char* get_str() const { return (char*)(this + 1);}
        uint32_t sz() const { return _size; }

        bool compare(rt_string* other);

        bool compare(const string other);

        rt_string* concat(rt_string* other);

        static rt_string* create(char* str);

    };

    class rt_arr_store : public rt_obj {
        byte _contains_obj; // Optimization flag
        uint32_t _capacity;

    public:
        // TODO: move this to cpp and add init vals
        rt_arr_store(uint32_t cap, std::span<Value> init);

        std::span<Value> get_data() { return { (Value*)((this+1)), _capacity }; }
        void set_has_obj() {
            std::atomic_ref<byte> { _contains_obj }.store(1, std::memory_order_release);
        }
        bool has_obj() {
            auto ref = std::atomic_ref<byte> { _contains_obj };
            return ref.load(std::memory_order_acquire) != 0;
        }

        static rt_arr_store* alloc(uint32_t capacity, std::span<Value> init);
    };

    class rt_arr : public rt_obj {
        uint32_t _size;
        rt_arr_store* _storage;
    public:
        rt_arr(size_t size);

        void gc_init();

        std::span<Value> get_data() { return { _storage->get_data().data(), _size }; }
        // Used by gc customization
        template<typename F>
        void upd_storage(F get_new) { _storage = get_new(_storage); }
        rt_arr_store* get_store() { return _storage; }
        void push(Value item);
    };

    using compiled_fn = void*;
    using name_field_map = int (*)(rt_string*);

    // Multiple closures with different freevars can point to the same function
    class rt_closure : public rt_obj {
        byte _arity;
        byte _env_cnt; // number of variables captured as part of the func env
        compiled_fn _func;
        char* _name;
    public:
        rt_closure(byte arity, byte env_cnt, compiled_fn f, char* name) 
            : rt_obj(rt_type::CLOSURE, false), _arity(arity),
            _env_cnt(env_cnt), _func(f), _name(name) {}

        std::span<Value> get_env() { return {reinterpret_cast<Value *>(this + 1), _env_cnt }; }
        
        char* name() { return _name; }
        byte arity() { return _arity; }
    };

    // Never created at runtime, only emitted by the compiler.
    // Compiler appends the method array directly behind the class and
    // tags those closures as objects, and mask_payload_obj reserves the low 4 bits of a Value for the
    // type tag, so anything taggable must be 16 byte aligned. Without this the array starts at offset
    // 40 and every other method gets a corrupted tag.
    struct alignas(16) comp_class{
        uint16_t methodArrLen;
        uint16_t fieldsArrLen;
        uint32_t classHierarchyStart;
        uint32_t classHierarchyEnd;
        const char* name;
        name_field_map getMethod;
        name_field_map getField;
    };

    class rt_inst : public rt_obj {
        comp_class* _klass;
    public:
        rt_inst(comp_class* klass, Value* field_init) : rt_obj(rt_type::INSTANCE, false), _klass(klass) {
            memcpy(get_fields().data(), field_init, get_fields().size() * sizeof(Value));
        }

        std::span<Value> get_fields() { return { (Value*)(this+1), _klass->fieldsArrLen }; }
        comp_class* get_class() { return _klass; }
    };

    class rt_hashmap : public rt_obj {
        ankerl::unordered_dense::map<object::rt_string*, Value> fields;
    public:
        rt_hashmap();
    };


}