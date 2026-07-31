#include "InstBuilder.h"
#include "IRUtils.h"
#include "../../Runtime/Values/valueHelpers.h"


// create_inst memcpys this over the fresh instance's field array, so every field starts nulled and
// instantiation doesn't have to write them one by one.
// TODO: memoize this so that we don0t create unnecessary copies of arrays of same length
llvm::GlobalVariable* InstBuilder::createFieldTemplate(int fieldN) const {
    vector<llvm::Constant*> fields(fieldN);
    std::ranges::fill(fields, _tyhelp.ConstCastToESLVal(_b.getInt64(mask_signature_null)));
    auto fieldArr = llvm::ConstantArray::get(
        llvm::ArrayType::get(_tyhelp.getESLValType(), fieldN), fields
    );
    return _ct.storeConstObj(fieldArr);
}

// Methods live inline in the class' method array, so the address of a slot is the closure itself.
llvm::Constant* InstBuilder::constMethodVal(llvm::Constant* vtable, uint64_t methodIdx) const {
    auto slot = llvm::ConstantExpr::getInBoundsGetElementPtr(
        _bridge.sty(bridge_ty::rt_closure_aligned), vtable, _b.getInt32(methodIdx)
    );
    return _ct.constObjToVal(slot, RtClosure::tag);
}

llvm::Value* InstBuilder::optimizeInstGet(known_access access_data, llvm::Constant* vtable) const {
    auto& [inst, field, ty] = access_data;
    if(ty.methods.contains(field))
        return constMethodVal(vtable, ty.methods.at(field).second);
    
    if(ty.fields.contains(field))
        return _bridge.inst(inst).fields(ty.fields.at(field).second).load();
    
    // TODO: error
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* InstBuilder::instGetUnoptimized(unknown_access access_data){
    auto& [possible_inst, field] = access_data;
    auto inst = _rt.checked_inst("Expected an instance, got '{}'.", possible_inst);
    auto klass = inst.klass();
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, field);

    auto F = _b.GetInsertBlock()->getParent();

    auto errorBB = llvm::BasicBlock::Create(_b.getContext(), "error", F);
    auto fieldBB = llvm::BasicBlock::Create(_b.getContext(), "fields", F);
    auto methodBB = llvm::BasicBlock::Create(_b.getContext(), "methods", F);
    auto mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge", F);

    _rt.createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx), {
        { 0, errorBB, 0 }, { 1, fieldBB, 1<<31 }, { 2, methodBB, 1<<31 }
    });

    _b.SetInsertPoint(fieldBB);
    auto loaded_field = inst.fields(fieldIdx).load();
    _b.CreateBr(mergeBB);

    _b.SetInsertPoint(methodBB);
    // The slot address is the closure, so it just needs tagging
    auto method = klass.method(methodIdx).to_val();
    _b.CreateBr(mergeBB);

    _b.SetInsertPoint(errorBB);
    _rt.createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field, possible_inst);
    _b.CreateUnreachable();

    _b.SetInsertPoint(mergeBB);
    auto phi = _b.CreatePHI(_tyhelp.getESLValType(), 2);
    phi->addIncoming(loaded_field, fieldBB);
    phi->addIncoming(method, methodBB);
    return phi;
}

// first el: field index, second el: method index
// Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
std::pair<llvm::Value*, llvm::Value*> InstBuilder::instGetUnoptIdx(CompClass klass, const std::string &field) const {
    auto fieldConst = _ct.createESLString(field);
    return { klass.lookup_field(fieldConst), klass.lookup_method(fieldConst) };
}

llvm::Value* InstBuilder::instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx) const {
    auto cmp1 = _b.CreateAnd(fieldIdx, _b.getInt32(1u << 31u));
    auto cmp2 = _b.CreateAnd(methodIdx, _b.getInt32(1u << 30u));
    auto dest = _b.CreateOr(cmp1, cmp2);
    return _b.CreateTrunc(_b.CreateLShr(dest, 30, "res"), _b.getInt8Ty());
}

llvm::Value* InstBuilder::getOptInstFieldPtr(known_access access_data) const {
    auto& [inst, field, ty] = access_data;
    if(ty.fields.contains(field))
        return _bridge.inst(inst).fields(ty.fields.at(field).second).ptr();
    
    // TODO: error
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    // Unreachable (at least it should be)
    __builtin_unreachable();
}

llvm::Value* InstBuilder::getUnoptInstFieldPtr(unknown_access access_data) const {
    auto& [possible_inst, field] = access_data;
    auto inst = _rt.checked_inst("Expected an instance, got '{}'.", possible_inst);

    auto fieldIdx = inst.klass().lookup_field(_ct.createESLString(field));

    create_if(_b, _b.CreateICmpEQ(fieldIdx, _b.getInt32(-1)),
        [&]() {
            _rt.createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field, possible_inst);
            _b.CreateUnreachable();
        },
        [](){}
    );
    return inst.fields(fieldIdx).ptr();
}

// Modifies callArgs to have correct args
llvm::Value* InstBuilder::optimizeInvoke(
    known_access access_data, llvm::Constant* vtable, uint8_t argc, const callback& cb
) const {
    auto& [inst, field, ty] = access_data;
    if(ty.methods.contains(field)){
        auto closure = constMethodVal(vtable, ty.methods.at(field).second);
        auto fn = _tyhelp.ty_to_fn(ty.methods.at(field).first);
        auto arr = std::array<llvm::Value*, 2>{ closure, inst };
        return cb(fn, arr);
    }
    if(ty.fields.contains(field)){
        auto closure = _bridge.inst(inst).fields(ty.fields.at(field).second).load();

        auto fn = _rt.createUnoptFunCall(closure, argc);
        auto arr = std::array<llvm::Value*, 1> { closure };
        return cb(fn, arr);
    }
    // TODO: error since we're invoking a method/field that doesnt exist
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* InstBuilder::unoptimizedInvoke(unknown_access access_data, uint8_t argc, const callback& cb) const {
    auto [encodedInst, field_name] = access_data;
    auto inst = _rt.checked_inst("Expected an instance, got '{}'.", encodedInst);
    auto klass = inst.klass();
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, field_name);

    auto F = _b.GetInsertBlock()->getParent();

    auto errorBB = llvm::BasicBlock::Create(_b.getContext(), "error");
    auto fieldBB = llvm::BasicBlock::Create(_b.getContext(), "fields");
    auto methodBB = llvm::BasicBlock::Create(_b.getContext(), "methods");
    auto mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge");

    _rt.createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx),
        { { 0, errorBB, 0 }, { 1, fieldBB, 1<<31 }, { 2, methodBB, 1<<31 }}
    );

    F->insert(F->end(), fieldBB);
    _b.SetInsertPoint(fieldBB);
    auto field = inst.fields(fieldIdx).load();
    std::array<llvm::Value*, 1> prepend = { field };
    auto callres1 = cb(_rt.createUnoptFunCall(field, argc), prepend);
    fieldBB = _b.GetInsertBlock();
    _b.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    _b.SetInsertPoint(methodBB);
    // The slot address is the closure, so it just needs tagging
    auto method = klass.method(methodIdx).to_val();
    std::array<llvm::Value*, 2> prepend2 = { method, encodedInst };
    // 'this' is prepended too, and a method's recorded arity counts it, so it's one more than the
    // number of arguments written at the call site
    auto callres2 = cb(_rt.createUnoptFunCall(method, argc + 1), prepend2);

    methodBB = _b.GetInsertBlock();
    _b.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    _b.SetInsertPoint(errorBB);
    _rt.createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field_name, encodedInst);
    _b.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    _b.SetInsertPoint(mergeBB);
    auto phi = _b.CreatePHI(callres1->getType(), 2);
    phi->addIncoming(callres1, fieldBB);
    phi->addIncoming(callres2, methodBB);
    return phi;
}
