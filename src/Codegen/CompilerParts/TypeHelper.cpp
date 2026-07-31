#include "TypeHelper.h"
#include "RuntimeBridge.h"


TypeHelper::TypeHelper(llvm::IRBuilder<>& b, runtime_bridge& bridge, const ankerl_map<std::string, std::pair<int, int>>& _hierarchy)
    : _b(b), _bridge(bridge) {
    _func_attrs.push_back(llvm::Attribute::get(_b.getContext(), "uwtable", "sync"));
    _func_attrs.push_back(llvm::Attribute::get(_b.getContext(), "no-trapping-math", "true"));
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
    return runtime_bridge::eslValType(_b.getContext());
}

llvm::FunctionType* TypeHelper::getFuncType(int argCount) const {
    // First argument is always the closure structure which is an ESL val
    vector params = { getESLValType() };
    for(int i = 0; i < argCount; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    return fty;
}

void TypeHelper::set_fn_attrs(llvm::Function* fn) const {
    for(const auto& attr : _func_attrs) fn->addFnAttr(attr);
}

llvm::Function* TypeHelper::ty_to_fn(const types::tyPtr& ty) const {
    if (_fn_type_mapping.contains(ty)) return _fn_type_mapping.at(ty);
    return nullptr;
}

void TypeHelper::add_fn_mapping(const types::tyPtr& ty, llvm::Function* fn) {
    _fn_type_mapping[ty] = fn;
}

std::pair<int, int> TypeHelper::class_hierarchy(const std::string& classname) const {
    return _class_hierarchy.at(classname);
}