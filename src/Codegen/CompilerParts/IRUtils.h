#pragma once
#include "llvm/IR/IRBuilder.h"

#include <functional>

// Emits an if/else and leaves the builder on the merge block. Branches that already terminated
// (a return, an unreachable after a runtime error) don't get a jump to merge.
inline void create_if(llvm::IRBuilder<>& b, llvm::Value* cond,
                      const std::function<void()>& then,
                      const std::function<void()>& _else) {
    llvm::Function* F = b.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(b.getContext(), "then", F);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(b.getContext(), "else");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(b.getContext(), "merge");

    b.CreateCondBr(cond, thenBB, elseBB);
    b.SetInsertPoint(thenBB);
    then();
    if (!b.GetInsertBlock()->back().isTerminator()) b.CreateBr(mergeBB);

    F->insert(F->end(), elseBB);
    b.SetInsertPoint(elseBB);
    _else();
    if (b.GetInsertBlock()->empty() || !b.GetInsertBlock()->back().isTerminator()) b.CreateBr(mergeBB);

    F->insert(F->end(), mergeBB);
    b.SetInsertPoint(mergeBB);
}
