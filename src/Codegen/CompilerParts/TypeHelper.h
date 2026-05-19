#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../common.h"
#include "../../Includes/unorderedDense.h"


class TypeHelper {
    template<class T, class K>
    using ankerl_map = ankerl::unordered_dense::map<T, K>;
public:
    TypeHelper(llvm::IRBuilder<>& b, llvm::Module& mod, ankerl_map<string, std::pair<int, int>>& _classHierarchy);
    // ESL val casting
    llvm::Value* ESLValTo(llvm::Value* val, llvm::Type* ty) const;
    llvm::Constant* ESLConstTo(llvm::Constant* constant, llvm::Type* ty) const;
    llvm::Value* CastToESLVal(llvm::Value* val) const;
    llvm::Constant* ConstCastToESLVal(llvm::Constant* constant) const;
    llvm::Type* getESLValType() const;

    // Internal objects
    llvm::Type* internal_obj_ty(const string& name) const;

    // Functions
    llvm::FunctionType* getFuncType(int argCount) const;

    // Classes
    std::pair<int, int> class_hierarchy(const string& classname) const;
private:
    llvm::IRBuilder<>& _b;

    ankerl_map<string, llvm::Type*> _named_types;
    ankerl_map<string, std::pair<int, int>> _class_hierarchy;
};