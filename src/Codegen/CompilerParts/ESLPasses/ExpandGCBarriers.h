#pragma once
#include "../RuntimeBridge.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace esl_passes {
// Lets the helpers marked late_inline_attr be inlined, once the optimizer is done moving them around/merging them
struct expand_gc_barriers : llvm::PassInfoMixin<expand_gc_barriers> {
    llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager&) {
        for (auto& F : M) {
            if (!F.hasFnAttribute(runtime_bridge::late_inline_attr)) continue;
            F.removeFnAttr(llvm::Attribute::NoInline);
            F.addFnAttr(llvm::Attribute::AlwaysInline);
        }
        return llvm::PreservedAnalyses::none();
    }
};
}
