#include "InstBuilder.h"
#include "../../Runtime/Values/valueHelpers.h"
#include "llvm/IR/Module.h"
#include "../../Includes/fmt/format.h"


// Creates an instance template with already nulled fields that is memcpy-ed when using new
llvm::GlobalVariable* InstBuilder::createInstanceTemplate(llvm::Constant* klass, int fieldN) const {
    vector<llvm::Constant*> fields(fieldN);
    std::ranges::fill(fields, _tyhelp.ConstCastToESLVal(_b.getInt64(mask_signature_null)));
    // Template array that is already nulled
    llvm::Constant* fieldArr = llvm::ConstantArray::get(llvm::ArrayType::get(_tyhelp.getESLValType(), fieldN), fields);
    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(_b.getContext(), "ObjInstance"), {
            _ct.createObjHeader(+object::ObjType::INSTANCE), _b.getInt32(fieldN), klass });
    // Struct and array should be one after another without any padding(?)
    auto inst =  llvm::ConstantStruct::get(llvm::StructType::create(_b.getContext(),
        { _tyhelp.internal_obj_ty("ObjInstance"), llvm::ArrayType::get(_tyhelp.getESLValType(), fieldN) }),{obj, fieldArr});
    return _ct.storeConstObj(inst);
}

llvm::Value* InstBuilder::optimizeInstGet(known_access access_data, llvm::Constant* vtable) const {
    auto& [inst, field, ty] = access_data;
    if(ty.methods.contains(field)){
        // Index into the array of ObjClosure contained in the class
        auto arrIdx = _b.getInt32(ty.methods.at(field).second);

        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        llvm::Constant* val = llvm::ConstantExpr::getInBoundsGetElementPtr(_tyhelp.internal_obj_ty("ObjClosureAligned"), vtable, arrIdx);
        return _ct.constObjToVal(val, +object::ObjType::CLOSURE);
    }
    if(ty.fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = _b.getInt32(ty.fields.at(field).second);

        inst = _b.CreateCall(get_func("decodeObj"), {inst});
        inst = _b.CreateBitCast(inst, _tyhelp.internal_obj_ty("ObjInstancePtr"));
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(1) });
        ptr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, { arrIdx });
        return _b.CreateLoad(_tyhelp.getESLValType(), ptr);
    }
    // TODO: error
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* InstBuilder::instGetUnoptimized(unknown_access access_data){
    auto& [possible_inst, field] = access_data;
    auto [inst, klass] = instGetClassPtr(possible_inst);
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, field);

    llvm::Function *F = _b.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(_b.getContext(), "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(_b.getContext(), "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(_b.getContext(), "methods");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge");

    _rt.createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx), {
        { 0, errorBB, 0 }, { 1, fieldBB, 1<<31 }, { 2, methodBB, 1<<31 }
    });

    F->insert(F->end(), fieldBB);
    _b.SetInsertPoint(fieldBB);

    // Fields are stored just behind the instance
    auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst, {_b.getInt32(1)});
    // Loads the field
    ptr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, fieldIdx);
    auto loaded_field =  _b.CreateLoad(_tyhelp.getESLValType(), ptr);
    _b.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    _b.SetInsertPoint(methodBB);
    // Methods are just behind class
    ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClass"), klass, {_b.getInt32(1)});
    llvm::Value* val = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClosureAligned"), ptr, methodIdx);
    auto method = _b.CreateCall(get_func("encodeObj"), {val, _b.getInt64(+object::ObjType::CLOSURE)});
    _b.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    _b.SetInsertPoint(errorBB);
    _rt.createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field, possible_inst);
    _b.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    _b.SetInsertPoint(mergeBB);
    auto phi = _b.CreatePHI(_tyhelp.getESLValType(), 2);
    phi->addIncoming(loaded_field, fieldBB);
    phi->addIncoming(method, methodBB);
    return phi;
}

// first el: field index, second el: method index
// Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
std::pair<llvm::Value*, llvm::Value*> InstBuilder::instGetUnoptIdx(llvm::Value* klass, const std::string &field) const {
    auto fnTy = llvm::FunctionType::get(_b.getInt32Ty(), { _tyhelp.getESLValType() }, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClass"), klass, { _b.getInt32(0), _b.getInt32(6) });
    auto methodFunc = _b.CreateLoad(fnPtrTy, ptr);
    ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClass"), klass, {_b.getInt32(0), _b.getInt32(7)});
    auto fieldsFunc = _b.CreateLoad(fnPtrTy, ptr);

    llvm::Constant* fieldConst = _ct.createESLString(field);
    llvm::Value* fieldIdx = _b.CreateCall(fnTy, fieldsFunc, fieldConst);
    llvm::Value* methodIdx = _b.CreateCall(fnTy, methodFunc, fieldConst);
    return std::make_pair(fieldIdx, methodIdx);
}

std::pair<llvm::Value*, llvm::Value*> InstBuilder::instGetClassPtr(llvm::Value* inst) const {
    _rt.createTypeCheckUnary("Expected an instance, got '{}'.", inst, TypeHelper::getObjectTypeMasks(object::ObjType::INSTANCE));

    inst = _b.CreateCall(get_func("decodeObj"), {inst});
    inst = _b.CreateBitCast(inst, _tyhelp.internal_obj_ty("ObjInstancePtr"));

    auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(0), _b.getInt32(2) });
    auto klass = _b.CreateLoad(_tyhelp.internal_obj_ty("ObjClassPtr"), ptr);
    return std::make_pair(inst, klass);
}

llvm::Value* InstBuilder::instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx) const {
    auto cmp1 = _b.CreateAnd(fieldIdx, _b.getInt32(1u << 31u));
    auto cmp2 = _b.CreateAnd(methodIdx, _b.getInt32(1u << 30u));
    auto dest = _b.CreateOr(cmp1, cmp2);
    return _b.CreateTrunc(_b.CreateLShr(dest, 30, "res"), _b.getInt8Ty());
}

llvm::Value* InstBuilder::getOptInstFieldPtr(known_access access_data) const {
    auto& [inst, field, ty] = access_data;
    if(ty.fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = _b.getInt32(ty.fields.at(field).second);

        inst = _b.CreateCall(get_func("decodeObj"), {inst});
        inst = _b.CreateBitCast(inst, _tyhelp.internal_obj_ty("ObjInstancePtr"));
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(1) });
        ptr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, {arrIdx});
        return ptr;
    }
    // TODO: error
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    // Unreachable (at least it should be)
    __builtin_unreachable();
}

llvm::Value* InstBuilder::getUnoptInstFieldPtr(unknown_access access_data) const {
    auto& [possible_inst, field] = access_data;
    auto [inst, klass] = instGetClassPtr(possible_inst);

    // Gets the function which determines index of field given a string
    llvm::FunctionType* fnTy = llvm::FunctionType::get(_b.getInt32Ty(), { _tyhelp.getESLValType() }, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    llvm::Value* ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClass"), klass, { _b.getInt32(0), _b.getInt32(7) });
    auto fieldsFunc = _b.CreateLoad(fnPtrTy, ptr);

    llvm::Value* fieldIdx = _b.CreateCall(fnTy, fieldsFunc, _ct.createESLString(field));

    auto cmp = _b.CreateICmpEQ(fieldIdx, _b.getInt32(-1));

    create_if(cmp,
        [&]() {
            _rt.createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field, possible_inst);
            _b.CreateUnreachable();
        },
        [](){}
    );
    // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
    ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(1) });
    ptr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, fieldIdx);

    return ptr;
}

// Modifies callArgs to have correct args
llvm::Value* InstBuilder::optimizeInvoke(known_access access_data, llvm::Constant* vtable, uint8_t argc, const callback& cb) const {
    auto& [inst, field, ty] = access_data;
    if(ty.methods.contains(field)){
        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        auto closure = llvm::ConstantExpr::getInBoundsGetElementPtr(_tyhelp.internal_obj_ty("ObjClosureAligned"),
            vtable, _b.getInt32(ty.methods.at(field).second)
        );
        closure = llvm::ConstantExpr::getBitCast(closure, _b.getPtrTy());
        // Closures in class are stored as raw pointers, tag them and then pass to method
        closure = _ct.constObjToVal(closure, +object::ObjType::CLOSURE);
        llvm::Function* fn = _tyhelp.ty_to_fn(ty.methods.at(field).first);
        // Insert at begin + 1 to skip the thread data ptr
        std::array<llvm::Value*, 2> arr = { closure, inst };
        return cb(fn, arr);
    }
    if(ty.fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = _b.getInt32(ty.fields.at(field).second);

        inst = _b.CreateCall(get_func("decodeObj"), { inst });
        inst = _b.CreateBitCast(inst, _tyhelp.internal_obj_ty("ObjInstancePtr"));
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(1) });
        llvm::Value* fieldPtr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, {arrIdx});
        llvm::Value* closure = _b.CreateLoad(_tyhelp.getESLValType(), fieldPtr);

        auto fn = _rt.createUnoptFunCall(closure, argc);
        std::array arr = { closure };
        return cb(fn, arr);
    }
    // TODO: error since we're invoking a method/field that doesnt exist
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* InstBuilder::unoptimizedInvoke(unknown_access access_data, uint8_t argc, const callback& cb) const {
    auto [encodedInst, field_name] = access_data;
    auto [inst, klass] = instGetClassPtr(encodedInst);
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, field_name);

    llvm::Function *F = _b.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(_b.getContext(), "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(_b.getContext(), "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(_b.getContext(), "methods");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge");

    _rt.createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx),
        { { 0, errorBB, 0 }, { 1, fieldBB, 1<<31 }, { 2, methodBB, 1<<31 }}
    );

    F->insert(F->end(), fieldBB);
    _b.SetInsertPoint(fieldBB);
    // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
    auto ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjInstance"), inst,{ _b.getInt32(1) });
    auto fieldPtr = _b.CreateInBoundsGEP(_tyhelp.getESLValType(), ptr, fieldIdx);
    auto field =  _b.CreateLoad(_tyhelp.getESLValType(), fieldPtr);
    std::array<llvm::Value*, 1> prepend = { field };
    auto callres1 = cb(_rt.createUnoptFunCall(field, argc), prepend);
    fieldBB = _b.GetInsertBlock();
    _b.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    _b.SetInsertPoint(methodBB);
    // First load the ObjClosurePtr(we treat the offset into the ObjClosure array that the class has as a standalone pointer to that closure)
    ptr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClass"), klass, { _b.getInt32(1) });
    llvm::Value* val = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClosureAligned"), ptr, methodIdx);
    // Since this is a raw ObjClosure pointer we tag it first
    auto method = _b.CreateCall(get_func("encodeObj"), {val, _b.getInt64(+object::ObjType::CLOSURE)});

    std::array<llvm::Value*, 2> prepend2 = { method, encodedInst };
    auto callres2 = cb(_rt.createUnoptFunCall(field, argc), prepend2);

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


void InstBuilder::create_if(llvm::Value* cond, const std::function<void()>& then, const std::function<void()>& _else) const {
    llvm::Function *F = _b.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(_b.getContext(), "then", F);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(_b.getContext(), "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge");
    _b.CreateCondBr(cond, thenBB, elseBB);
    _b.SetInsertPoint(thenBB);
    then();
    if (!_b.GetInsertBlock()->back().isTerminator()) _b.CreateBr(mergeBB);

    F->insert(F->end(), elseBB);
    _b.SetInsertPoint(elseBB);
    _else();
    if (_b.GetInsertBlock()->empty() || !_b.GetInsertBlock()->back().isTerminator()) _b.CreateBr(mergeBB);
    F->insert(F->end(), mergeBB);
    _b.SetInsertPoint(mergeBB);
}

llvm::Function* InstBuilder::get_func(const string& name) const {
    auto* fn = _b.GetInsertBlock()->getModule()->getFunction(name);
    if(!fn){
        std::cerr<<fmt::format("Function {} hasn't been created yet.\n", name);
        exit(64);
    }
    return fn;
}
