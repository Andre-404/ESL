#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../Includes/unorderedDense.h"
#include "../../TypedAST/Types.h"
#include "../../Runtime/Values/valueHelpers.h"
#include "RuntimeBridge.h"


class TypeHelper {
    template<class T, class K>
    using ankerl_map = ankerl::unordered_dense::map<T, K>;

    llvm::IRBuilder<>& _b;
    runtime_bridge& _bridge;

    ankerl_map<std::string, std::pair<int, int>> _class_hierarchy;
    vector<llvm::Attribute> _func_attrs;
    ankerl_map<types::tyPtr, llvm::Function*> _fn_type_mapping;
public:
    TypeHelper(llvm::IRBuilder<>& b, runtime_bridge& bridge, const ankerl_map<std::string, std::pair<int, int>>& _classHierarchy);
    llvm::Value* ESLValTo(llvm::Value* val, llvm::Type* ty) const;
    llvm::Constant* ESLConstTo(llvm::Constant* constant, llvm::Type* ty) const;
    llvm::Value* CastToESLVal(llvm::Value* val) const;
    llvm::Constant* ConstCastToESLVal(llvm::Constant* constant) const;
    llvm::Type* getESLValType() const;

    // Functions
    llvm::FunctionType* getFuncType(int argCount) const;
    void set_fn_attrs(llvm::Function* fn) const;
    llvm::Function* ty_to_fn(const types::tyPtr& ty) const;
    void add_fn_mapping(const types::tyPtr &ty, llvm::Function* fn);

    // Classes
    std::pair<int, int> class_hierarchy(const std::string& classname) const;

    // For objects use obj_type_masks(rt_type) from BridgeObjects.h, which lives next to
    // the tag definitions.
    static std::tuple<uint64_t, uint64_t, bool> getNumberTypeMasks(){
        return { mask_qnan, mask_qnan, true };
    }
    static std::tuple<uint64_t, uint64_t, bool> getBoolTypeMasks(){
        return { mask_signature_bool, mask_signature_bool, false };
    }
    static std::tuple<uint64_t, uint64_t, bool> getNullTypeMasks(){
        return { mask_signature_null, mask_signature_null, false };
    }
};