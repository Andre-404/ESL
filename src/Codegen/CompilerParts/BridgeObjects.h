#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../Runtime/Objects/objects.h"

// The layouts themselves live in runtime_bridge, which checks them against the real C++ types at
// startup

class runtime_bridge;

// Must be different from object::rt_type because we have comp class here as well
enum class bridge_ty : uint8_t {
    rt_obj,
    rt_string,
    rt_arr,
    rt_arr_store,
    rt_closure,
    rt_closure_aligned,
    rt_inst,
    comp_class,
    _count
};
constexpr size_t operator+ (bridge_ty const t) { return static_cast<size_t>(t); }

// Carries its own alignment because LLVM rejects atomic accesses that don't have one
class field_ref {
    llvm::IRBuilder<>* _b = nullptr;
    llvm::Type* _ty = nullptr;
    llvm::Value* _ptr = nullptr;
    llvm::Align _align;
    const char* _name = "";
public:
    field_ref() = default;
    field_ref(llvm::IRBuilder<>& b, llvm::Type* ty, llvm::Value* ptr, llvm::Align align, const char* name)
        : _b(&b), _ty(ty), _ptr(ptr), _align(align), _name(name) {}

    llvm::Type* type() const { return _ty; }
    llvm::Value* ptr() const { return _ptr; }
    llvm::Align align() const { return _align; }
    explicit operator bool() const { return _ptr != nullptr; }

    llvm::LoadInst* load() const;
    llvm::StoreInst* store(llvm::Value* val) const;
    llvm::LoadInst* load(llvm::AtomicOrdering ordering) const;
    llvm::StoreInst* store(llvm::Value* val, llvm::AtomicOrdering ordering) const;

    // So `auto [ty, ptr] = closure.arity();` works
    template<std::size_t I> auto get() const {
        if constexpr (I == 0) return _ty; else return _ptr;
    }
};

namespace std {
    template<> struct tuple_size<field_ref> : integral_constant<size_t, 2> {};
    template<size_t I> struct tuple_element<I, field_ref> {
        using type = conditional_t<I == 0, llvm::Type*, llvm::Value*>;
    };
}

// Holds the untagged pointer
class rt_ref {
    runtime_bridge* _br = nullptr;
    llvm::Value* _ptr = nullptr;
protected:
    rt_ref(runtime_bridge& br, llvm::Value* raw) : _br(&br), _ptr(raw) {}

    // Field `idx` of layout `ty`. Type and alignment come from the DataLayout, never from a literal
    field_ref at(bridge_ty ty, unsigned idx, const char* name) const;
    // Element `idx` of the flexible array member that starts right behind layout `ty`
    // Very common in ESL object layouts (instances, arr storage headers, strings, in the future hashmaps)
    field_ref trailing(bridge_ty ty, llvm::Type* elem, llvm::Value* idx, const char* name) const;
public:
    rt_ref() = default;

    llvm::Value* ptr() const { return _ptr; }
    runtime_bridge& br() const { return *_br; }
    llvm::IRBuilder<>& b() const;
    explicit operator bool() const { return _ptr != nullptr; }
};

// Base for everything carrying a gc::managed header and therefore a rt_type tag
class gc_obj_ref : public rt_ref {
protected:
    using rt_ref::rt_ref;
    // NaN-boxes this pointer with `tag`
    llvm::Value* encode(object::rt_type tag) const;
public:
    field_ref state() const;    // gc::managed::_state
    field_ref type_id() const;  // gc::managed::_type_id
};

// NaN-box masks for a tagged object type: {expected, mask, useNEQ}
std::tuple<uint64_t, uint64_t, bool> obj_type_masks(object::rt_type tag);

class RtArrStore;
class RtClosure;
class CompClass;

// object::rt_string — header, _size, string that trails after header
class RtString : public gc_obj_ref {
public:
    static constexpr object::rt_type tag = object::rt_type::STRING;
    static constexpr bridge_ty layout = bridge_ty::rt_string;
    using gc_obj_ref::gc_obj_ref;

    static RtString from_raw(runtime_bridge& br, llvm::Value* raw) { return { br, raw }; }
    llvm::Value* to_val() const { return encode(tag); }

    field_ref sz() const;
    llvm::Value* str() const;

    static llvm::Constant* const_build(runtime_bridge& br, const std::string& str);
};

// object::rt_arr — header, _size, _storage
class RtArr : public gc_obj_ref {
public:
    static constexpr object::rt_type tag = object::rt_type::ARRAY;
    static constexpr bridge_ty layout = bridge_ty::rt_arr;
    using gc_obj_ref::gc_obj_ref;

    static RtArr from_raw(runtime_bridge& br, llvm::Value* raw) { return { br, raw }; }
    llvm::Value* to_val() const { return encode(tag); }

    field_ref size() const;
    field_ref storage_ref() const;  // the slot itself, for stores
    RtArrStore storage() const;     // loads it and hands back the handle
};

// object::rt_arr_store — header, _contains_obj, _capacity, then the Values
class RtArrStore : public gc_obj_ref {
public:
    static constexpr object::rt_type tag = object::rt_type::ARRAY_STORAGE_HEADER;
    static constexpr bridge_ty layout = bridge_ty::rt_arr_store;
    using gc_obj_ref::gc_obj_ref;

    static RtArrStore from_raw(runtime_bridge& br, llvm::Value* raw) { return { br, raw }; }
    llvm::Value* to_val() const { return encode(tag); }

    field_ref contains_obj() const;  // atomic, acquire/release
    field_ref capacity() const;
    field_ref data(llvm::Value* idx) const;
    field_ref data(unsigned idx) const;
    llvm::Value* data_ptr() const;  // base of the trailing Value[]

    // Mirrors rt_arr_store::set_has_obj
    void set_has_obj() const;
};

// object::rt_closure — header, _arity, _env_cnt, _func, _name, then the captured env
class RtClosure : public gc_obj_ref {
public:
    static constexpr object::rt_type tag = object::rt_type::CLOSURE;
    static constexpr bridge_ty layout = bridge_ty::rt_closure;
    using gc_obj_ref::gc_obj_ref;

    static RtClosure from_raw(runtime_bridge& br, llvm::Value* raw) { return { br, raw }; }
    llvm::Value* to_val() const { return encode(tag); }

    field_ref arity() const;
    field_ref env_cnt() const;
    field_ref func() const;
    field_ref name() const;
    field_ref env(llvm::Value* idx) const;
    field_ref env(unsigned idx) const;

    static llvm::Constant* const_build(runtime_bridge& br, uint8_t arity, uint8_t env_cnt,
                                       llvm::Constant* fn, llvm::Constant* name);
    // The 16-byte aligned form used for the method array behind a class
    static llvm::Constant* const_build_aligned(runtime_bridge& br, uint8_t arity, uint8_t env_cnt,
                                               llvm::Constant* fn, llvm::Constant* name);
};

// object::rt_inst — header, _klass, then the fields
class RtInst : public gc_obj_ref {
public:
    static constexpr object::rt_type tag = object::rt_type::INSTANCE;
    static constexpr bridge_ty layout = bridge_ty::rt_inst;
    using gc_obj_ref::gc_obj_ref;

    static RtInst from_raw(runtime_bridge& br, llvm::Value* raw) { return RtInst(br, raw); }
    llvm::Value* to_val() const { return encode(tag); }

    field_ref klass_ref() const;  // the slot itself
    CompClass klass() const;      // loads it and hands back the handle
    field_ref fields(llvm::Value* idx) const;
    field_ref fields(unsigned idx) const;
};

// object::comp_class — compiler-emitted, the method array (each closure aligned to 16 bytes)
// sits directly behind it, which is why comp_class is alignas(16).
class CompClass : public rt_ref {
public:
    static constexpr bridge_ty layout = bridge_ty::comp_class;
    using rt_ref::rt_ref;

    static CompClass from_raw(runtime_bridge& br, llvm::Value* raw) { return CompClass(br, raw); }

    field_ref method_arr_len() const;
    field_ref fields_arr_len() const;
    field_ref hierarchy_start() const;
    field_ref hierarchy_end() const;
    field_ref name() const;
    field_ref get_method_fn() const;
    field_ref get_field_fn() const;

    RtClosure method(llvm::Value* idx) const;

    // The name -> index lookups the class carries
    llvm::Value* lookup_field(llvm::Constant* field_name) const;
    llvm::Value* lookup_method(llvm::Constant* field_name) const;
    // Signature of getField/getMethod: int(rt_string*)
    static llvm::FunctionType* lookup_fn_ty(runtime_bridge& br);

    static llvm::Constant* const_build(
        runtime_bridge& br, uint16_t method_len, uint16_t field_len,
        uint32_t hierarchy_start, uint32_t hierarchy_end, llvm::Constant* name,
        llvm::Constant* get_method, llvm::Constant* get_field, llvm::ArrayRef<llvm::Constant*> methods
    );
};
