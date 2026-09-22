#pragma once
#include "../../Includes/unorderedDense.h"
#include "../../common.h"
#include <atomic>
#include <bit>
#include <cstring>
#include <span>
#include "../esl-gc-helpers.h"

namespace object {

    enum class rt_type : uint8_t {
        STRING,
        CLOSURE,
        INSTANCE,
        ARRAY,
        ARRAY_STORAGE_HEADER,
        HASH_MAP,
        BUFFER,
        FILE,
        MUTEX,
        CHANNEL,
        WAIT_GROUP,
        SOCKET
    };
    inline constexpr unsigned operator+ (rt_type const val) { return static_cast<byte>(val); }

    class rt_obj : public gc::managed {
    public:
        static constexpr bool pinned = false;
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

        bool compare(const string& other);

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

        void reserve(uint32_t min_cap);
    public:
        rt_arr(size_t size);

        void gc_init();

        std::span<Value> get_data() { return { _storage->get_data().data(), _size }; }
        // Used by gc customization
        void set_store(rt_arr_store* new_store) { _storage = new_store; }
        rt_arr_store* get_store() { return _storage; }

        uint32_t size() const { return _size; }
        uint32_t capacity() { return _storage->get_data().size(); }

        void push(Value item);
        // Returns the removed element, nil if the array is empty
        Value pop();
        // Grows with copies of fill, or shrinks and zeroes the dropped slots
        void resize(uint32_t new_size, Value fill);
        // index is clamped to _size, so index == _size appends
        void insert(uint32_t index, Value item);
        // No-op if index is out of range
        void erase(uint32_t index);
        // Zeroes the entire storage(not just the live part) and sets size to 0
        void clear();
    };


    // Raw bytes owned by the gc. Movable like every other rt_obj, but never scanned,
    // so it must not hold references to other managed objects
    class rt_buffer : public rt_obj {
        uint32_t _size;
    public:
        rt_buffer(uint32_t size, byte fill) : rt_obj(rt_type::BUFFER, false), _size(size) {
            memset(get_data().data(), fill, size);
        }

        std::span<byte> get_data() { return { (byte*)(this + 1), _size }; }
        uint32_t size() const { return _size; }

        static rt_buffer* alloc(uint32_t size, byte fill = 0);
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

    // Swiss table(see abseil's flat_hash_map / go's swisstable)
    // Every helper returns a bitset with the high bit of each matching byte set.
    // Control byte: 0b0hhhhhhh full(low 7 bits of the hash), 0b10000000 empty, 0b11111110 deleted
    namespace swiss {
        constexpr uint32_t group_sz = 8;
        constexpr byte ctrl_empty = 0x80;
        constexpr byte ctrl_deleted = 0xFE;
        constexpr uint64_t lsb = 0x0101010101010101ull;
        constexpr uint64_t msb = 0x8080808080808080ull;

        inline uint64_t load_group(const byte* ctrl, uint32_t group) {
            uint64_t v;
            memcpy(&v, ctrl + group * group_sz, sizeof(v));
            return v;
        }
        inline void set_group(byte* ctrl, uint32_t group, uint64_t g) {
            memcpy(ctrl + group * group_sz, &g, sizeof(g));
        }
        // Can report false positives in bytes above a real match(borrow propagation),
        // callers always confirm with a key comparison. Never matches an empty or deleted byte
        inline uint64_t match_h2(uint64_t group, byte h2) {
            uint64_t x = group ^ (lsb * h2);
            return (x - lsb) & ~x & msb;
        }
        inline uint64_t match_empty(uint64_t group) { return group & ~(group << 6) & msb; }
        inline uint64_t match_empty_or_deleted(uint64_t group) { return group & ~(group << 7) & msb; }
        inline uint64_t match_full(uint64_t group) { return ~group & msb; }
        // Slot(within the group) of the lowest set bit of a match bitset
        inline uint32_t first_slot(uint64_t bits) { return std::countr_zero(bits) >> 3; }
        inline uint64_t reset_group(uint64_t group) {
            auto x = group & msb;
            return (~x + (x >> 7)) & ~lsb;
        }
    }
    
    // Control bytes live in an unscanned rt_buffer, slots live in a traced rt_arr_store laid out as
    // [key0, val0, key1, val1, ...]
    // Empty slots are always zeroed since the whole slot store gets traced.
    class rt_hashmap : public rt_obj {
        uint32_t _count;
        uint32_t _growth_left;  // Empty slots not counting tombstones
        uint32_t _capacity;     // Must be power of 2 multiple of swiss::group_sz
        rt_buffer* _ctrl;
        rt_arr_store* _slots;

        byte* ctrl() { return _ctrl->get_data().data(); }
        Value* slots() { return _slots->get_data().data(); }

        // Slot holding key, or -1
        int64_t find_slot(rt_string* key, uint64_t hash);
        void rehash(uint32_t new_cap);
        void in_place_rehash();
    public:
        // Capacity is sized so that expected entries fit without a rehash
        rt_hashmap(uint32_t expected = 0);

        void gc_init();

        uint32_t size() const { return _count; }
        uint32_t capacity() const { return _capacity; }

        // Pointer to the value slot, invalidated by any later mutation of the map or gc cycle
        Value* find(rt_string* key);
        bool contains(rt_string* key) { return find(key) != nullptr; }
        void insert_or_assign(rt_string* key, Value val);
        bool erase(rt_string* key);
        void clear();

        // fn(Value key, Value val), key is always an encoded rt_string
        template<typename F>
        void for_each(F fn) {
            auto c = ctrl();
            auto s = slots();
            for (uint32_t g = 0; g < _capacity / swiss::group_sz; g++) {
                for (auto m = swiss::match_full(swiss::load_group(c, g)); m; m &= m - 1) {
                    auto i = g * swiss::group_sz + swiss::first_slot(m);
                    fn(s[2 * i], s[2 * i + 1]);
                }
            }
        }

        rt_buffer* get_ctrl() { return _ctrl; }
        rt_arr_store* get_slots() { return _slots; }
        void set_ctrl(rt_buffer* c) { _ctrl = c; }
        void set_slots(rt_arr_store* s) { _slots = s; }
    };


}