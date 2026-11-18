#include "Optimizer.h"
#include "RuntimeBridge.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"

namespace {
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

// LLVM only sees a slot_read_attr call as an argmem read of unknown size, so it couldn't tell a
// read of one field apart from a store to the next one. This gives AA the exact 8 bytes
class slot_read_aa : public llvm::AAResultBase {
    static std::optional<llvm::MemoryLocation> slot(const llvm::CallBase* call) {
        auto F = call->getCalledFunction();
        if (!F || !F->hasFnAttribute(runtime_bridge::slot_read_attr) || call->arg_size() != 1) return std::nullopt;
        return llvm::MemoryLocation(call->getArgOperand(0), llvm::LocationSize::precise(8), call->getAAMetadata());
    }
public:
    using llvm::AAResultBase::getModRefInfo;
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase* call, const llvm::MemoryLocation& loc, llvm::AAQueryInfo& AAQI) {
        auto s = slot(call);
        if (!s) return llvm::ModRefInfo::ModRef;
        return AAQI.AAR.alias(*s, loc, AAQI, call) == llvm::AliasResult::NoAlias ? llvm::ModRefInfo::NoModRef : llvm::ModRefInfo::Ref;
    }
    // What c1 does to the memory c2 accesses
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase* c1, const llvm::CallBase* c2, llvm::AAQueryInfo& AAQI) {
        if (auto s = slot(c2)) return AAQI.AAR.getModRefInfo(c1, *s, AAQI) & llvm::ModRefInfo::Mod;
        if (auto s = slot(c1)) return llvm::isModSet(AAQI.AAR.getModRefInfo(c2, *s, AAQI)) ? llvm::ModRefInfo::Ref : llvm::ModRefInfo::NoModRef;
        return llvm::ModRefInfo::ModRef;
    }
    bool invalidate(llvm::Function&, const llvm::PreservedAnalyses&, llvm::FunctionAnalysisManager::Invalidator&) { return false; }
};
struct slot_read_analysis : llvm::AnalysisInfoMixin<slot_read_analysis> {
    using Result = slot_read_aa;
    slot_read_aa run(llvm::Function&, llvm::FunctionAnalysisManager&) { return slot_read_aa(); }
    static inline llvm::AnalysisKey Key;
};

void add_gc_passes(llvm::ModulePassManager& MPM) {
    MPM.addPass(expand_gc_barriers());
    MPM.addPass(llvm::AlwaysInlinerPass());
    llvm::FunctionPassManager FPM;
    // Stores can only be forwarded into a read once it is a load again
    FPM.addPass(llvm::GVNPass());
    FPM.addPass(llvm::InstCombinePass());
    FPM.addPass(llvm::SimplifyCFGPass());
    MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}
}

void optimize(llvm::Module& module) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    // Before registerFunctionAnalyses, which keeps the first AAManager registered
    FAM.registerPass([] { return slot_read_analysis(); });
    FAM.registerPass([&] {
        auto AA = PB.buildDefaultAAPipeline();
        AA.registerFunctionAnalysis<slot_read_analysis>();
        return AA;
    });
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    add_gc_passes(MPM);
    MPM.run(module, MAM);
}
