#include "llvmHelperFunctions.h"
#include "../common.h"
#include "valueHelpersInline.cpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <unordered_set>
#include <iostream>



#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, __VA_ARGS__, isVarArg), llvm::Function::ExternalLinkage, name, module)
#define TYPE(type) llvm::Type::get ## type ## Ty(ctx)

void buildLLVMNativeFunctions(llvm::Module* module, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder);

void llvmHelpers::addHelperFunctionsToModule(llvm::Module* module, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder){
    CREATE_FUNC("asNum", false, TYPE(Double),  TYPE(Int64));
    CREATE_FUNC("print", false, TYPE(Void),  TYPE(Int64));
    CREATE_FUNC("createStr", false, TYPE(Int64), TYPE(Int8Ptr));
    CREATE_FUNC("valueIsTrue", false, TYPE(Int1), TYPE(Int64));
    CREATE_FUNC("nativeEncodeBool", false, TYPE(Int64), TYPE(Int1));

    buildLLVMNativeFunctions(module, ctx, builder);
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

static void createFunc(llvm::FunctionType *FT, llvm::Module* module, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder){

}

void buildLLVMNativeFunctions(llvm::Module* module, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder){
    auto createFunc = [&](string name, llvm::FunctionType *FT){
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::PrivateLinkage, name, module);
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(ctx, "entry", F);
        builder.SetInsertPoint(BB);
        return F;
    };
    // Extremenly cursed, but using lambdas that are immediately called avoids naming conflicts
    // No encode/decodeObj right now, need to think of how to represent objects in llvm IR
    [&]{
        auto f = createFunc("encodeBool",llvm::FunctionType::get(TYPE(Int64), TYPE(Int1), false));
        auto arg = builder.CreateIntCast(f->getArg(0), TYPE(Int64), false);
        auto negArg = builder.CreateIntCast(builder.CreateNot(f->getArg(0)), TYPE(Int64), false);
        auto lhs = builder.CreateMul(arg, llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_TRUE));
        auto rhs = builder.CreateMul(negArg, llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_FALSE));

        builder.CreateRet(builder.CreateAdd(lhs, rhs,"bin add"));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("encodeNull",llvm::FunctionType::get(TYPE(Int64), false));
        builder.CreateRet(llvm::ConstantInt::get(TYPE(Int64), encodeNil()));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("decodeBool",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        builder.CreateRet(builder.CreateICmpEQ(f->getArg(0), llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_TRUE)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("isNumber",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto constant = llvm::ConstantInt::get(TYPE(Int64), MASK_QNAN);
        builder.CreateRet(builder.CreateICmpNE(builder.CreateAnd(arg, constant), constant));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("isBool",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto const0 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE);
        auto const1 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_TRUE);
        auto const2 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_FALSE);
        auto tmp = builder.CreateAnd(arg, const0);
        builder.CreateRet(builder.CreateOr(builder.CreateICmpEQ(tmp, const1), builder.CreateICmpEQ(tmp, const2)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("isNull",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto const0 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE);
        auto const1 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_NIL);
        builder.CreateRet(builder.CreateICmpEQ(builder.CreateAnd(arg, const0), const1));
        llvm::verifyFunction(*f);
    }();
    [&]{
        auto f = createFunc("isObj",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto const0 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE);
        auto const1 = llvm::ConstantInt::get(TYPE(Int64), MASK_SIGNATURE_OBJ);
        builder.CreateRet(builder.CreateICmpEQ(builder.CreateAnd(arg, const0), const1));
        llvm::verifyFunction(*f);
    }();
    //!((isBool(x) && !decodeBool(x)) || isNil(x));
    [&]{
        auto f = createFunc("isTruthy",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto c1 = builder.CreateCall(module->getFunction("isBool"), arg);
        auto c2 = builder.CreateNot(builder.CreateCall(module->getFunction("decodeBool"), arg));
        auto c3 = builder.CreateCall(module->getFunction("isNull"), arg);
        builder.CreateRet(builder.CreateNot(builder.CreateOr(builder.CreateAnd(c1, c2), c3)));
    llvm::verifyFunction(*f);
    }();
}