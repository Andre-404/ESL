#pragma once
// Generates functions(alongside their signatures) for use in LLVM IR
// Most of these functions are called in LLVM IR to simplify the process of typechecking etc
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "JIT.h"

namespace llvmHelpers {
    void addHelperFunctionsToModule(llvm::Module* module, llvm::LLVMContext &context);

    void runModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::orc::KaleidoscopeJIT>& JIT, std::unique_ptr<llvm::LLVMContext>& ctx, bool optimize);

}