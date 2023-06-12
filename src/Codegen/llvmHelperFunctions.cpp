#include "llvmHelperFunctions.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "valueHelpersInline.cpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Passes/PassBuilder.h"

#include <unordered_set>
#include <iostream>



#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, __VA_ARGS__, isVarArg), llvm::Function::ExternalLinkage, name, module);
#define TYPE(type) llvm::Type::get ## type ## Ty(context)
#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif
#define EXPORT extern "C" DLLEXPORT

EXPORT double asNum(Value x){
    if(isNumber(x)){
        return decodeNumber(x);
    }
    exit(64);
}

EXPORT void print(Value x){
    std::cout<< "Value is: "<< valueHelpers::toString(x)<<std::endl;
}

void llvmHelpers::addHelperFunctionsToModule(llvm::Module* module, llvm::LLVMContext& context){
    CREATE_FUNC("asNum", false, TYPE(Double),  TYPE(Int64));
    CREATE_FUNC("print", false, TYPE(Void),  TYPE(Int64));
}

void llvmHelpers::runModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::orc::KaleidoscopeJIT>& JIT, std::unique_ptr<llvm::LLVMContext>& ctx, bool optimize){
    // Create the analysis managers.
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    llvm::PassBuilder PB;

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    // Create the pass manager.
    // This one corresponds to a typical -O2 optimization pipeline.
    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
    if(optimize) MPM.run(*module, MAM);

    auto RT = JIT->getMainJITDylib().createResourceTracker();

    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), std::move(ctx));
    llvm::ExitOnError()(JIT->addModule(std::move(TSM), RT));

    auto ExprSymbol = llvm::ExitOnError()(JIT->lookup("anon"));
    assert(ExprSymbol.getAddress() && "Function not found");

    void (*FP)() = ExprSymbol.getAddress().toPtr<void (*)()>();
    FP();
    llvm::ExitOnError()(RT->remove());
}