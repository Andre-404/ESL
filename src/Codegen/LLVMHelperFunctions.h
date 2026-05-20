#pragma once
// Generates functions(alongside their signatures) for use in LLVM IR
// Most of these functions are called in LLVM IR to simplify the process of typechecking etc
#include "llvm/IR/IRBuilder.h"

#include "../Includes/unorderedDense.h"

namespace llvm{
    class LLVMContext;
    class Module;
}

namespace llvmHelpers {
    llvm::Type* getESLValType(llvm::LLVMContext& ctx);
    void addHelperFunctionsToModule(llvm::Module& module,
                                    llvm::LLVMContext& context,
                                    llvm::IRBuilder<>& builder,
                                    ankerl::unordered_dense::map<std::string, llvm::Type*>& types);
}