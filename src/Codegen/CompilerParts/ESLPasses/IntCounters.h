#pragma once
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/TargetFolder.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

#include <cmath>
#include <optional>

namespace esl_passes {
// Rewrites a double loop counter, phi [a, preheader], [phi + s, latch] tested against n, as an i32 when every value
// it takes is exact. Other uses get a sitofp, which InstCombine folds into the fptosi that indexing and bit ops do,
// and the exit test becomes an icmp SCEV can count. Constant a and n are checked here; otherwise an innermost loop
// is versioned, and the original double loop runs when the preheader's check fails
struct int_counters : llvm::PassInfoMixin<int_counters> {
    using CmpInst = llvm::CmpInst;
    static constexpr const char* fallback_md = "esl.int_counters.fallback";

    struct counter {
        llvm::PHINode* phi;
        llvm::Instruction* inc;
        llvm::FCmpInst* exit;
        bool exit_on_inc, stays_on_true, inclusive;
        int32_t step;
        llvm::Value *start, *n;
    };
    // The counter's i32 start and bound, and the check that has to pass first (null if none)
    struct limits { llvm::Value *start, *bound, *ok; };

    static std::optional<int32_t> exact_i32(const llvm::APFloat& v) {
        if (!v.isInteger() || v.isNegZero()) return std::nullopt;
        double d = v.convertToDouble();
        if (d < INT32_MIN || d > INT32_MAX) return std::nullopt;
        return int32_t(d);
    }

    static std::optional<counter> find(llvm::Loop& L, llvm::PHINode& phi, llvm::LoopInfo& LI, llvm::DominatorTree& DT) {
        using namespace llvm::PatternMatch;
        auto pre = L.getLoopPreheader(), latch = L.getLoopLatch();
        if (!phi.getType()->isDoubleTy() || phi.getNumIncomingValues() != 2 || phi.getBasicBlockIndex(latch) < 0) return std::nullopt;
        const llvm::APFloat* s;
        auto inc = llvm::dyn_cast<llvm::Instruction>(phi.getIncomingValueForBlock(latch));
        if (!inc || !match(inc, m_FAdd(m_Specific(&phi), m_APFloat(s)))) return std::nullopt;
        auto step = exact_i32(*s);
        if (!step || *step == 0 || std::abs(*step) > (1 << 20)) return std::nullopt;

        llvm::SmallVector<llvm::BasicBlock*, 4> exits;
        L.getExitingBlocks(exits);
        for (auto exiting : exits) {
            auto br = llvm::dyn_cast<llvm::CondBrInst>(exiting->getTerminator());
            auto cmp = br ? llvm::dyn_cast<llvm::FCmpInst>(br->getCondition()) : nullptr;
            // Every trip through the latch passed this test. Other exits only cut the loop short
            if (!cmp || LI.getLoopFor(exiting) != &L || !DT.dominates(exiting, latch)) continue;
            auto pred = cmp->getPredicate();
            llvm::Value *x = cmp->getOperand(0), *n = cmp->getOperand(1);
            if (x != &phi && x != inc) {
                std::swap(x, n);
                pred = CmpInst::getSwappedPredicate(pred);
            }
            bool stays_on_true = L.contains(br->getSuccessor(0));
            if ((x != &phi && x != inc) || !L.isLoopInvariant(n) || stays_on_true == L.contains(br->getSuccessor(1))) continue;
            // n is checked not to be NaN, so ordered and unordered agree
            auto stay = CmpInst::getOrderedPredicate(stays_on_true ? pred : CmpInst::getInversePredicate(pred));
            if (CmpInst::getStrictPredicate(stay) != (*step > 0 ? CmpInst::FCMP_OLT : CmpInst::FCMP_OGT)) continue;
            bool inclusive = CmpInst::isNonStrictPredicate(stay);
            return counter{ &phi, inc, cmp, x == inc, stays_on_true, inclusive, *step, phi.getIncomingValueForBlock(pre), n };
        }
        return std::nullopt;
    }

    // The checks a counter needs, in the preheader before its terminator
    static std::optional<limits> limits_for(llvm::Loop& L, const counter& c, bool may_check) {
        using namespace llvm::PatternMatch;
        if (!may_check && !(llvm::isa<llvm::Constant>(c.start) && llvm::isa<llvm::Constant>(c.n))) return std::nullopt;
        // inc can run one step past the last value the test lets through
        int32_t limit = (c.step > 0 ? INT32_MAX : INT32_MIN) - 2 * c.step;
        // For an integer i: i < n is i < ceil(n), i <= n is i < floor(n) + 1. Mirrored when counting down
        bool use_ceil = (c.step > 0) != c.inclusive;
        double offset = c.inclusive ? (c.step > 0 ? 1 : -1) : 0;

        // TargetFolder folds intrinsics too, so a constant start or n gives constant checks: those that pass are dropped
        auto pre = L.getLoopPreheader();
        llvm::IRBuilder<llvm::TargetFolder> B(pre, pre->getTerminator()->getIterator(), llvm::TargetFolder(pre->getDataLayout()));
        llvm::Type *i32 = B.getInt32Ty(), *f64 = B.getDoubleTy();
        llvm::Value* ok = nullptr;
        auto check = [&](llvm::Value* v) { if (!match(v, m_One())) ok = ok ? B.CreateAnd(ok, v) : v; };
        // Exact iff converting back gives the same bits, which rules out NaN (so non-numbers too) and -0
        auto start = B.CreateIntrinsic(llvm::Intrinsic::fptosi_sat, { i32, f64 }, { c.start });
        check(B.CreateICmpEQ(B.CreateBitCast(B.CreateSIToFP(start, f64), B.getInt64Ty()), B.CreateBitCast(c.start, B.getInt64Ty())));
        check(B.CreateICmp(c.step > 0 ? CmpInst::ICMP_SLE : CmpInst::ICMP_SGE, start, B.getInt32(limit)));
        check(B.CreateFCmp(c.step > 0 ? CmpInst::FCMP_OLE : CmpInst::FCMP_OGE, c.n, llvm::ConstantFP::get(f64, limit)));
        // A constant ok failed with nothing but constants, so nothing was inserted. Mixed with a runtime check it is
        // an and that stays false, and only the fallback runs
        if (ok && llvm::isa<llvm::Constant>(ok)) return std::nullopt;
        llvm::Value* r = B.CreateUnaryIntrinsic(use_ceil ? llvm::Intrinsic::ceil : llvm::Intrinsic::floor, c.n);
        if (offset) r = B.CreateFAdd(r, llvm::ConstantFP::get(f64, offset));
        return limits{ start, B.CreateIntrinsic(llvm::Intrinsic::fptosi_sat, { i32, f64 }, { r }), ok };
    }

    // Copies L as the fallback, entered when ok is false. L keeps the preheader the checks are in
    static void version(llvm::Loop& L, llvm::Value* ok, llvm::LoopInfo& LI, llvm::DominatorTree& DT) {
        auto guard = L.getLoopPreheader();
        llvm::formLCSSA(L, DT, &LI, nullptr);
        auto ph = llvm::SplitBlock(guard, guard->getTerminator()->getIterator(), &DT, &LI, nullptr, "ctr.ph");
        llvm::ValueToValueMapTy VMap;
        llvm::SmallVector<llvm::BasicBlock*, 16> blocks;
        auto fallback = llvm::cloneLoopWithPreheader(ph, guard, &L, VMap, ".dbl", &LI, &DT, blocks);
        llvm::remapInstructionsInBlocks(blocks, VMap);
        // Both loops leave through the same exit blocks, and LCSSA put every use outside the loop in their phis
        llvm::SmallVector<llvm::BasicBlock*, 4> exits;
        L.getUniqueExitBlocks(exits);
        for (auto exit : exits)
            for (auto& phi : exit->phis())
                for (unsigned k = 0, e = phi.getNumIncomingValues(); k < e; ++k)
                    if (L.contains(phi.getIncomingBlock(k))) {
                        llvm::Value* v = phi.getIncomingValue(k);
                        if (auto mapped = VMap.lookup(v)) v = mapped;
                        phi.addIncoming(v, llvm::cast<llvm::BasicBlock>(VMap[phi.getIncomingBlock(k)]));
                    }
        guard->getTerminator()->eraseFromParent();
        llvm::IRBuilder<>(guard).CreateCondBr(ok, ph, fallback->getLoopPreheader(),
                                              llvm::MDBuilder(guard->getContext()).createLikelyBranchWeights());
        // 1, since getBooleanLoopAttribute reads the default 0 as false
        llvm::addStringMetadataToLoop(fallback, fallback_md, 1);
        DT.recalculate(*guard->getParent());
    }

    static void rewrite(const counter& c, llvm::Value* start, llvm::Value* bound) {
        auto header = c.phi->getParent();
        auto ctr = llvm::PHINode::Create(llvm::Type::getInt32Ty(header->getContext()), 2, "ctr", header->begin());
        llvm::IRBuilder<> B(c.inc);
        auto next = B.CreateNSWAdd(ctr, B.getInt32(c.step), "ctr.next");
        for (unsigned k = 0; k < 2; ++k)
            ctr->addIncoming(c.phi->getIncomingValue(k) == c.inc ? next : start, c.phi->getIncomingBlock(k));

        auto pred = c.step > 0 ? CmpInst::ICMP_SLT : CmpInst::ICMP_SGT;
        B.SetInsertPoint(c.exit);
        c.exit->replaceAllUsesWith(B.CreateICmp(c.stays_on_true ? pred : CmpInst::getInversePredicate(pred),
                                                c.exit_on_inc ? next : ctr, bound));
        c.exit->eraseFromParent();
        B.SetInsertPoint(header, header->getFirstInsertionPt());
        c.phi->replaceAllUsesWith(B.CreateSIToFP(ctr, B.getDoubleTy()));
        B.SetInsertPoint(c.inc);
        c.inc->replaceAllUsesWith(B.CreateSIToFP(next, B.getDoubleTy()));
        c.phi->eraseFromParent();
        c.inc->eraseFromParent();
    }

    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        auto& LI = FAM.getResult<llvm::LoopAnalysis>(F);
        auto& DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
        bool changed = false, versioned = false;
        for (auto L : LI.getLoopsInPreorder()) {
            if (!L->isLoopSimplifyForm() || llvm::getBooleanLoopAttribute(L, fallback_md)) continue;
            // Versioning copies the loop, so only innermost loops (where the time goes), and once each
            bool may_check = L->isInnermost();
            for (auto& phi : llvm::make_early_inc_range(L->getHeader()->phis())) {
                auto c = find(*L, phi, LI, DT);
                auto lim = c ? limits_for(*L, *c, may_check) : std::nullopt;
                if (!lim) continue;
                if (lim->ok) {
                    version(*L, lim->ok, LI, DT);
                    may_check = false;
                    versioned = true;
                }
                rewrite(*c, lim->start, lim->bound);
                changed = true;
            }
        }
        if (!changed) return llvm::PreservedAnalyses::all();
        return versioned ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::allInSet<llvm::CFGAnalyses>();
    }
};
}
