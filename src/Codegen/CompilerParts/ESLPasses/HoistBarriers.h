#pragma once
#include "../RuntimeBridge.h"

#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/MustExecute.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace esl_passes {
// Hoists gc_read_* barriers out of loops where the slot being read is loop invariant
// The reason LLVM cant do this on its own is that the function gc_read_* cannot be reasoned
// about the same way as loads (just because LLVM knows that a ptr is dereferenable wont make it hoist the barrier)
// What we do is move the barrier up, and then guard it with a typecheck so that if the container passed
// isnt the one we expect we dont cause a segfault
struct hoist_barriers : llvm::PassInfoMixin<hoist_barriers> {
    // Whether makeLoopInvariant would succeed, without moving anything
    static bool invariant_once_hoisted(llvm::Value* v, const llvm::Loop* L, unsigned depth = 0) {
        if (L->isLoopInvariant(v)) return true;
        auto I = llvm::cast<llvm::Instruction>(v);
        return depth <= 8 && !I->mayReadFromMemory() && llvm::isSafeToSpeculativelyExecute(I)
            && llvm::all_of(I->operands(), [&](llvm::Use& op) { return invariant_once_hoisted(op, L, depth + 1); });
    }

    // Hoists the first barrier it can. Analyses are stale afterwards
    static bool hoist_one(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        auto& LI = FAM.getResult<llvm::LoopAnalysis>(F);
        if (LI.empty()) return false;
        auto& DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
        auto& AC = FAM.getResult<llvm::AssumptionAnalysis>(F);
        auto& MSSA = FAM.getResult<llvm::MemorySSAAnalysis>(F).getMSSA();
        for (auto& I : llvm::instructions(F)) {
            auto call = llvm::dyn_cast<llvm::CallInst>(&I);
            // When every call reads the same slot, IPO folds it into the body and drops the argument
            if (!call || !call->hasFnAttr(runtime_bridge::slot_read_attr) || call->arg_size() != 1) continue;
            auto slot = call->getArgOperand(0);
            auto clob = MSSA.getWalker()->getClobberingMemoryAccess(call)->getBlock();
            // The outermost loop the slot is invariant in and unwritten
            llvm::Loop* L = nullptr;
            for (auto cur = LI.getLoopFor(call->getParent()); cur && cur->getLoopPreheader(); cur = cur->getParentLoop()) {
                if (!invariant_once_hoisted(slot, cur) || cur->contains(clob)) break;
                L = cur;
            }
            if (!L) continue;
            auto pre = L->getLoopPreheader()->getTerminator();
            bool safe = llvm::isDereferenceableAndAlignedPointer(slot, runtime_bridge::ref_slot_align, llvm::APInt(64, 8),
                                                                 llvm::SimplifyQuery(F.getDataLayout(), &DT, &AC, pre));
            if (!safe) {
                llvm::ICFLoopSafetyInfo SI;
                SI.computeLoopSafetyInfo(L);
                safe = SI.isGuaranteedToExecute(*call, &DT, L);
            }
            auto md = call->getMetadata(runtime_bridge::fixed_slot_md);
            // The slot is computed from the owner, so it's invariant and gets hoisted along with it
            auto owner = safe || !md ? nullptr : runtime_bridge::decoded_from(slot);
            if (!safe && !owner) continue;

            bool changed = false;
            L->makeLoopInvariant(slot, changed);
            if (safe) {
                call->moveBefore(pre->getIterator());
                return true;
            }
            llvm::IRBuilder<> B(pre);
            auto expected = llvm::mdconst::extract<llvm::ConstantInt>(md->getOperand(0));
            auto mask = llvm::mdconst::extract<llvm::ConstantInt>(md->getOperand(1));
            auto ok = B.CreateICmpEQ(B.CreateAnd(B.CreatePtrToInt(owner, B.getInt64Ty()), mask), expected, "owner.tag");
            auto head = pre->getParent();
            auto then = llvm::SplitBlockAndInsertIfThen(ok, pre->getIterator(), false,
                                                        llvm::MDBuilder(F.getContext()).createLikelyBranchWeights());
            call->moveBefore(then->getIterator());
            // Null rather than poison, which would let LLVM reason the check away
            auto phi = llvm::PHINode::Create(call->getType(), 2, "hoisted", pre->getParent()->begin());
            call->replaceAllUsesWith(phi);
            phi->addIncoming(call, then->getParent());
            phi->addIncoming(llvm::Constant::getNullValue(call->getType()), head);
            return true;
        }
        return false;
    }

    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        bool changed = false;
        // Terminates since every round moves a call out of at least one loop
        while (hoist_one(F, FAM)) {
            changed = true;
            FAM.invalidate(F, llvm::PreservedAnalyses::none());
        }
        return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
    }
};
}
