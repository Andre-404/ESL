#include "BridgeObjects.h"
#include "RuntimeBridge.h"
#include "../../Runtime/Values/valueHelpers.h"

// ---------------------------------------------------------------------------------------------
// field_ref
// ---------------------------------------------------------------------------------------------

llvm::LoadInst* field_ref::load() const {
    return _b->CreateAlignedLoad(_ty, _ptr, _align, _name);
}
llvm::StoreInst* field_ref::store(llvm::Value* val) const {
    return _b->CreateAlignedStore(val, _ptr, _align);
}
llvm::LoadInst* field_ref::load(llvm::AtomicOrdering ordering) const {
    auto ld = _b->CreateAlignedLoad(_ty, _ptr, _align, _name);
    ld->setAtomic(ordering);
    return ld;
}
llvm::StoreInst* field_ref::store(llvm::Value* val, llvm::AtomicOrdering ordering) const {
    auto st = _b->CreateAlignedStore(val, _ptr, _align);
    st->setAtomic(ordering);
    return st;
}

// ---------------------------------------------------------------------------------------------
// rt_ref
// ---------------------------------------------------------------------------------------------

llvm::IRBuilder<>& rt_ref::b() const { return _br->b(); }

field_ref rt_ref::at(bridge_ty ty, unsigned idx, const char* name) const {
    auto st = _br->sty(ty);
    auto fieldTy = st->getElementType(idx);
    auto ptr = b().CreateStructGEP(st, _ptr, idx, name);
    return { b(), fieldTy, ptr, _br->field_align(ty, idx), name };
}

field_ref rt_ref::trailing(bridge_ty ty, llvm::Type* elem, llvm::Value* idx, const char* name) const {
    // The flexible array member starts at the end of the struct proper. Getting there with a byte
    // offset rather than `GEP <ty>, p, 1` keeps it correct even if the struct grows tail padding.
    auto base = b().CreateConstInBoundsGEP1_64(b().getInt8Ty(), _ptr, _br->trailing_off(ty));
    auto ptr = b().CreateInBoundsGEP(elem, base, idx, name);
    auto align = _br->DL().getABITypeAlign(elem);
    return { b(), elem, ptr, align, name };
}

// ---------------------------------------------------------------------------------------------
// gc_obj_ref
// ---------------------------------------------------------------------------------------------

field_ref gc_obj_ref::state() const { return at(bridge_ty::rt_obj, 0, "obj.state"); }
field_ref gc_obj_ref::type_id() const { return at(bridge_ty::rt_obj, 1, "obj.type"); }

llvm::Value* gc_obj_ref::encode(object::rt_type tag) const {
    return br().call("encode_obj", { ptr(), b().getInt64(+tag) }, "encoded.obj");
}

std::tuple<uint64_t, uint64_t, bool> obj_type_masks(object::rt_type tag) {
    return { mask_signature_obj | +tag, mask_signature | mask_payload_type, false };
}

// ---------------------------------------------------------------------------------------------
// RtString
// ---------------------------------------------------------------------------------------------

field_ref RtString::sz() const { return at(layout, 1, "str.size"); }

llvm::Value* RtString::str() const {
    return b().CreateConstInBoundsGEP1_64(
        b().getInt8Ty(), ptr(), br().trailing_off(layout), "str.chars"
    );
}

llvm::Constant* RtString::const_build(runtime_bridge& br, const std::string& str) {
    auto obj = llvm::ConstantStruct::get(br.sty(layout), {
        br.const_header(gc::move_state::unmanaged, tag),
        br.b().getInt32(str.size())
    });
    // String is stored directly after the header
    return llvm::ConstantStruct::getAnon(
        { obj, llvm::ConstantDataArray::getString(br.ctx(), str) }
    );
}

// ---------------------------------------------------------------------------------------------
// RtArr / RtArrStore
// ---------------------------------------------------------------------------------------------

field_ref RtArr::size() const { return at(layout, 1, "arr.size"); }
field_ref RtArr::storage_ref() const { return at(layout, 2, "arr.storage"); }

RtArrStore RtArr::storage() const {
    return RtArrStore::from_raw(br(), storage_ref().load());
}

field_ref RtArrStore::contains_obj() const { return at(layout, 1, "store.contains.obj"); }
field_ref RtArrStore::capacity() const { return at(layout, 2, "store.capacity"); }

llvm::Value* RtArrStore::data_ptr() const {
    return b().CreateConstInBoundsGEP1_64(
        b().getInt8Ty(), ptr(), br().trailing_off(layout), "store.data"
    );
}
field_ref RtArrStore::data(llvm::Value* idx) const {
    return trailing(layout, br().eslValType(), idx, "arr.elem");
}
field_ref RtArrStore::data(unsigned idx) const {
    return data(b().getInt32(idx));
}

void RtArrStore::set_has_obj() const {
    // Release pairs with the acquire in rt_arr_store::has_obj, which is what the GC reads
    // before deciding whether the elements are worth tracing.
    contains_obj().store(b().getInt8(1), llvm::AtomicOrdering::Release);
}

// ---------------------------------------------------------------------------------------------
// RtClosure
// ---------------------------------------------------------------------------------------------

field_ref RtClosure::arity() const { return at(layout, 1, "closure.arity"); }
field_ref RtClosure::env_cnt() const { return at(layout, 2, "closure.env.cnt"); }
field_ref RtClosure::func() const { return at(layout, 3, "closure.fn"); }
field_ref RtClosure::name() const { return at(layout, 4, "closure.name"); }

field_ref RtClosure::env(llvm::Value* idx) const {
    return trailing(layout, br().eslValType(), idx, "closure.capvar");
}
field_ref RtClosure::env(unsigned idx) const { return env(b().getInt32(idx)); }

llvm::Constant* RtClosure::const_build(
    runtime_bridge& br, uint8_t arity, uint8_t env_cnt, llvm::Constant* fn, llvm::Constant* name
) {
    return llvm::ConstantStruct::get(br.sty(layout), {
        br.const_header(gc::move_state::unmanaged, tag),
        br.b().getInt8(arity),
        br.b().getInt8(env_cnt),
        fn,
        name
    });
}

llvm::Constant* RtClosure::const_build_aligned(
    runtime_bridge& br, uint8_t arity, uint8_t env_cnt, llvm::Constant* fn, llvm::Constant* name
) {
    return llvm::ConstantStruct::get(br.sty(bridge_ty::rt_closure_aligned),
        { const_build(br, arity, env_cnt, fn, name), br.b().getInt64(0) }
    );
}

// ---------------------------------------------------------------------------------------------
// RtInst
// ---------------------------------------------------------------------------------------------

field_ref RtInst::klass_ref() const { return at(layout, 1, "inst.class"); }

CompClass RtInst::klass() const {
    return CompClass::from_raw(br(), klass_ref().load());
}

field_ref RtInst::fields(llvm::Value* idx) const {
    return trailing(layout, br().eslValType(), idx, "inst.field");
}
field_ref RtInst::fields(unsigned idx) const { return fields(b().getInt32(idx)); }

// ---------------------------------------------------------------------------------------------
// CompClass
// ---------------------------------------------------------------------------------------------

field_ref CompClass::method_arr_len() const { return at(layout, 0, "class.method.len"); }
field_ref CompClass::fields_arr_len() const { return at(layout, 1, "class.field.len"); }
field_ref CompClass::hierarchy_start() const { return at(layout, 2, "class.hierarchy.start"); }
field_ref CompClass::hierarchy_end() const { return at(layout, 3, "class.hierarchy.end"); }
field_ref CompClass::name() const { return at(layout, 4, "class.name"); }
field_ref CompClass::get_method_fn() const { return at(layout, 5, "class.get.method"); }
field_ref CompClass::get_field_fn() const { return at(layout, 6, "class.get.field"); }

RtClosure CompClass::method(llvm::Value* idx) const {
    // Methods are stored inline behind the class, so the pointer to a slot *is* the closure.
    // This is why comp_class must stay 16 byte aligned: these get tagged as objects.
    auto slot = trailing(layout, br().sty(bridge_ty::rt_closure_aligned), idx, "class.method");
    return RtClosure::from_raw(br(), slot.ptr());
}

llvm::FunctionType* CompClass::lookup_fn_ty(runtime_bridge& br) {
    return llvm::FunctionType::get(br.b().getInt32Ty(), { br.eslValType() }, false);
}

llvm::Value* CompClass::lookup_field(llvm::Constant* field_name) const {
    return b().CreateCall(lookup_fn_ty(br()), get_field_fn().load(), field_name, "field.idx");
}
llvm::Value* CompClass::lookup_method(llvm::Constant* field_name) const {
    return b().CreateCall(lookup_fn_ty(br()), get_method_fn().load(), field_name, "method.idx");
}

llvm::Constant* CompClass::const_build(
    runtime_bridge& br, uint16_t method_len, uint16_t field_len,
    uint32_t hierarchy_start, uint32_t hierarchy_end, llvm::Constant* name,
    llvm::Constant* get_method, llvm::Constant* get_field, llvm::ArrayRef<llvm::Constant*> methods
) {
    auto& b = br.b();
    llvm::Constant* klass = llvm::ConstantStruct::get(br.sty(layout), {
        b.getInt16(method_len), b.getInt16(field_len),
        b.getInt32(hierarchy_start), b.getInt32(hierarchy_end),
        name, get_method, get_field,
        b.getInt64(0)  // tail padding, so sizeof matches alignas(16) comp_class
    });
    // Always wrapped, even with no methods: callers reach the array as element 1 of the result,
    // and a zero length array keeps that index valid.
    auto methodArr = llvm::ConstantArray::get(
        llvm::ArrayType::get(br.sty(bridge_ty::rt_closure_aligned), methods.size()), methods
    );
    return llvm::ConstantStruct::getAnon({ klass, methodArr });
}
