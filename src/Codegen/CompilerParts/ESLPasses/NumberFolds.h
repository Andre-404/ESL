#pragma once
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/PatternMatch.h"

#include <cmath>
#include <optional>

namespace esl_passes {
// NaN-boxing keeps every non-number in NaN space, so a tag test on the bits of a double LLVM knows isn't NaN
// has a fixed answer. Also turns frem of integers into srem
struct number_folds : llvm::PassInfoMixin<number_folds> {
    // The double v holds the bits of, through the casts that box and unbox a Value
    static const llvm::Value* double_of(const llvm::Value* v, const llvm::DataLayout& DL) {
        while (!v->getType()->isDoubleTy()) {
            auto op = llvm::Operator::getOpcode(v);
            if ((op != llvm::Instruction::BitCast && op != llvm::Instruction::PtrToInt && op != llvm::Instruction::IntToPtr)
                || DL.getTypeSizeInBits(v->getType()) != 64)
                return nullptr;
            v = llvm::cast<llvm::Operator>(v)->getOperand(0);
        }
        return v;
    }
    static bool never_nan(const llvm::Value* bits, const llvm::SimplifyQuery& SQ) {
        auto d = double_of(bits, SQ.DL);
        return d && llvm::isKnownNeverNaN(d, SQ);
    }
    static std::optional<llvm::APInt> const_bits(const llvm::Value* v) {
        using namespace llvm::PatternMatch;
        const llvm::APInt* c;
        if (match(v, m_APInt(c)) || match(v, m_IntToPtr(m_APInt(c)))) return *c;
        if (llvm::isa<llvm::ConstantPointerNull>(v)) return llvm::APInt(64, 0);
        return std::nullopt;
    }

    static std::optional<bool> fold_tag_test(const llvm::ICmpInst& cmp, const llvm::SimplifyQuery& SQ) {
        using namespace llvm::PatternMatch;
        // InstCombine has put any constant on the right
        auto pred = cmp.getPredicate();
        const llvm::Value *x = cmp.getOperand(0), *bits;
        auto c = const_bits(cmp.getOperand(1));
        if (!c || c->getBitWidth() != 64) return std::nullopt;
        // (bits & mask) == tag, when only a NaN has every exponent bit and that mantissa bit set
        const llvm::APInt* mask;
        if (cmp.isEquality() && match(x, m_And(m_Value(bits), m_APInt(mask)))) {
            llvm::APInt exp(64, 0x7FF0000000000000), mant(64, 0x000FFFFFFFFFFFFF);
            if (!exp.isSubsetOf(*c) || !c->isSubsetOf(*mask) || !c->intersects(mant) || !never_nan(bits, SQ)) return std::nullopt;
            return pred == llvm::CmpInst::ICMP_NE;
        }
        if (!never_nan(x, SQ)) return std::nullopt;
        // Otherwise compare every non-NaN bit pattern: a sign bit over [+0, +inf]
        llvm::ConstantRange pos(llvm::APInt(64, 0), llvm::APInt(64, 0x7FF0000000000001));
        llvm::ConstantRange neg(llvm::APInt(64, 0x8000000000000000), llvm::APInt(64, 0xFFF0000000000001));
        auto holds = [&](llvm::CmpInst::Predicate p) { return pos.icmp(p, llvm::ConstantRange(*c)) && neg.icmp(p, llvm::ConstantRange(*c)); };
        if (holds(pred)) return true;
        if (holds(llvm::CmpInst::getInversePredicate(pred))) return false;
        return std::nullopt;
    }

    // fmod of integers is their srem, signed like the dividend: -0.0 for a negative multiple
    static llvm::Value* fold_frem(llvm::Instruction& rem, const llvm::SimplifyQuery& SQ) {
        using namespace llvm::PatternMatch;
        llvm::Value* x;
        const llvm::APFloat* c;
        bool is_signed = match(&rem, m_FRem(m_SIToFP(m_Value(x)), m_APFloat(c)));
        if (!is_signed && !match(&rem, m_FRem(m_UIToFP(m_Value(x)), m_APFloat(c)))) return nullptr;
        unsigned width = x->getType()->getScalarSizeInBits();
        if (!rem.getType()->isDoubleTy() || !c->isInteger() || c->isZero()) return nullptr;
        bool nonneg = llvm::isKnownNonNegative(x, SQ);
        // x has to be exact as a double, the divisor fit in x's type
        double d = std::fabs(c->convertToDouble());
        if ((!is_signed && !nonneg) || llvm::ComputeNumSignBits(x, SQ.DL, SQ.AC, SQ.CxtI, SQ.DT) + 53 < width
            || d >= std::ldexp(1.0, width - 1))
            return nullptr;
        llvm::IRBuilder<> B(&rem);
        llvm::Value* r = B.CreateSIToFP(B.CreateSRem(x, llvm::ConstantInt::get(x->getType(), uint64_t(d))), rem.getType());
        return nonneg ? r : B.CreateCopySign(r, rem.getOperand(0));
    }

    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        llvm::SimplifyQuery SQ(F.getDataLayout(), &FAM.getResult<llvm::DominatorTreeAnalysis>(F),
                               &FAM.getResult<llvm::AssumptionAnalysis>(F));
        bool changed = false;
        for (auto& I : llvm::make_early_inc_range(llvm::instructions(F))) {
            auto Q = SQ.getWithInstruction(&I);
            llvm::Value* to = nullptr;
            if (auto cmp = llvm::dyn_cast<llvm::ICmpInst>(&I)) {
                if (auto r = fold_tag_test(*cmp, Q)) to = llvm::ConstantInt::getBool(I.getType(), *r);
            } else
                to = fold_frem(I, Q);
            if (!to) continue;
            I.replaceAllUsesWith(to);
            I.eraseFromParent();
            changed = true;
        }
        return changed ? llvm::PreservedAnalyses::allInSet<llvm::CFGAnalyses>() : llvm::PreservedAnalyses::all();
    }
};
}
