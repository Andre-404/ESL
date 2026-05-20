#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../Includes/unorderedDense.h"
#include "../../TypedAST/Types.h"
#include "../../Runtime/Values/valueHelpers.h"


class TypeHelper {
    template<class T, class K>
    using ankerl_map = ankerl::unordered_dense::map<T, K>;
public:
    TypeHelper(llvm::IRBuilder<>& b, llvm::Module& mod, const ankerl_map<std::string, std::pair<int, int>>& _classHierarchy);
    // ESL val casting
    llvm::Value* ESLValTo(llvm::Value* val, llvm::Type* ty) const;
    llvm::Constant* ESLConstTo(llvm::Constant* constant, llvm::Type* ty) const;
    llvm::Value* CastToESLVal(llvm::Value* val) const;
    llvm::Constant* ConstCastToESLVal(llvm::Constant* constant) const;
    llvm::Type* getESLValType() const;

    // Internal objects
    llvm::Type* internal_obj_ty(const std::string& name) const;

    // Functions
    llvm::FunctionType* getFuncType(int argCount) const;
    void set_fn_attrs(llvm::Function* fn) const;
    llvm::Function* ty_to_fn(const types::tyPtr& ty) const;
    void add_fn_mapping(const types::tyPtr &ty, llvm::Function* fn);

    // Classes
    std::pair<int, int> class_hierarchy(const std::string& classname) const;

    // Type masks
    static std::tuple<uint64_t, uint64_t, bool> getObjectTypeMasks(object::ObjType type){
        return {mask_signature_obj | +type, mask_signature_obj | mask_payload_type, false};
    }
    static std::tuple<uint64_t, uint64_t, bool> getNumberTypeMasks(){
        return {mask_qnan, mask_qnan, true};
    }
    static std::tuple<uint64_t, uint64_t, bool> getBoolTypeMasks(){
        return {mask_signature_bool, mask_signature_bool, false};
    }
    static std::tuple<uint64_t, uint64_t, bool> getNullTypeMasks(){
        return {mask_signature_null, mask_signature_null, false};
    }
private:
    llvm::IRBuilder<>& _b;

    ankerl_map<std::string, llvm::Type*> _named_types;
    ankerl_map<std::string, std::pair<int, int>> _class_hierarchy;
    vector<llvm::Attribute> _func_attrs;
    ankerl_map<types::tyPtr, llvm::Function*> _fn_type_mapping;
};