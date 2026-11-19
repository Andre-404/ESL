#include "Optimizer.h"
#include "ESLPasses/ExpandGCBarriers.h"
#include "ESLPasses/HoistBarriers.h"
#include "ESLPasses/IntCounters.h"
#include "ESLPasses/NumberFolds.h"
#include "ESLPasses/PlaceSafepoints.h"
#include "ESLPasses/SlotReadAA.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"

namespace {
void add_gc_passes(llvm::ModulePassManager& MPM) {
    {
        llvm::FunctionPassManager FPM;
        // O3's last SimplifyCFG folds many preheaders into their guard blocks
        FPM.addPass(llvm::LoopSimplifyPass());
        FPM.addPass(esl_passes::hoist_barriers());
        MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    MPM.addPass(esl_passes::place_safepoints());
    MPM.addPass(esl_passes::expand_gc_barriers());
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
    FAM.registerPass([] { return esl_passes::slot_read_analysis(); });
    FAM.registerPass([&] {
        auto AA = PB.buildDefaultAAPipeline();
        AA.registerFunctionAnalysis<esl_passes::slot_read_analysis>();
        return AA;
    });
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    // Inside O3: the loop passes after this point need the counters already integers
    PB.registerScalarOptimizerLateEPCallback([](llvm::FunctionPassManager& FPM, llvm::OptimizationLevel) {
        FPM.addPass(esl_passes::int_counters());
        FPM.addPass(esl_passes::number_folds());
    });
    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    add_gc_passes(MPM);
    MPM.run(module, MAM);
}
