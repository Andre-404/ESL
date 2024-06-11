#pragma once
// Generates functions(alongside their signatures) for use in LLVM IR
// Most of these functions are called in LLVM IR to simplify the process of typechecking etc
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "JIT.h"
#include "../Includes/unorderedDense.h"
#include "../common.h"

namespace llvmHelpers {
    llvm::Type* getESLValType(llvm::LLVMContext& ctx);
    void addHelperFunctionsToModule(std::unique_ptr<llvm::Module>& module,
                                                                    std::unique_ptr<llvm::LLVMContext> &context,
                                                                    llvm::IRBuilder<>& builder,
                                                                    ankerl::unordered_dense::map<string, llvm::Type*>& types);

    void runModule(std::unique_ptr<llvm::Module> module, std::unique_ptr<llvm::LLVMContext> ctx,
                   std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT, std::unique_ptr<llvm::TargetMachine> machine,
                   bool shouldJIT);

}