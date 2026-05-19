#include "TypeHelper.h"
#include "../LLVMHelperFunctions.h"


TypeHelper::TypeHelper(llvm::IRBuilder<>& b, llvm::Module& mod, ankerl_map<string, std::pair<int, int>>& _hierarchy) : _b(b) {
    llvmHelpers::addHelperFunctionsToModule(mod, _b.getContext(), _b, _named_types);
    _class_hierarchy = _hierarchy;
}

// All of these functions are noops but are needed because of llvms type system
llvm::Value* TypeHelper::ESLValTo(llvm::Value* val, llvm::Type* ty) const {
    if(ty->isPointerTy())
        return _b.CreateBitCast(val, ty);
    return _b.CreateBitCast(_b.CreatePtrToInt(val, _b.getInt64Ty()), ty);
}
llvm::Constant* TypeHelper::ESLConstTo(llvm::Constant* constant, llvm::Type* ty) const {
    if(ty->isPointerTy())
        return llvm::ConstantExpr::getBitCast(constant, ty);
    auto i64Representation = llvm::ConstantExpr::getPtrToInt(constant, _b.getInt64Ty());
    return llvm::ConstantExpr::getBitCast(i64Representation, ty);
}

llvm::Value* TypeHelper::CastToESLVal(llvm::Value* val) const {
    if(val->getType()->isPointerTy())
        return _b.CreateBitCast(val, getESLValType());
    auto i64Representation = _b.CreateBitCast(val, _b.getInt64Ty());
    return _b.CreateIntToPtr(i64Representation, getESLValType());
}

llvm::Constant* TypeHelper::ConstCastToESLVal(llvm::Constant* constant) const {
    if(constant->getType()->isPointerTy())
        return llvm::ConstantExpr::getBitCast(constant, getESLValType());
    return llvm::ConstantExpr::getIntToPtr(constant, getESLValType());
}

llvm::Type* TypeHelper::getESLValType() const {
    return llvmHelpers::getESLValType(_b.getContext());
}

llvm::Type* TypeHelper::internal_obj_ty(const string& name) const {
    return _named_types.at(name);
}

llvm::FunctionType* TypeHelper::getFuncType(int argCount) const {
    vector params = { getESLValType() };
    // First argument is always the closure structure;
    for(int i = 0; i < argCount; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    return fty;
}

std::pair<int, int> TypeHelper::class_hierarchy(const string& classname) const {
    return _class_hierarchy.at(classname);
}