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
    llvm::Function::Create(llvm::FunctionType::get(returnType, {__VA_ARGS__}, isVarArg), llvm::Function::ExternalLinkage, name, module.get())
#define TYPE(type) llvm::Type::get ## type ## Ty(*ctx)

void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx, llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types);

void createLLVMTypes(std::unique_ptr<llvm::LLVMContext> &ctx, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    types["Obj"] = llvm::StructType::create(*ctx, {llvm::Type::getInt8Ty(*ctx), llvm::Type::getInt8Ty(*ctx)}, "Obj");
    types["ObjUpvalue"] = llvm::StructType::create(*ctx, {types["Obj"], llvm::Type::getInt64Ty(*ctx)}, "ObjUpval");
    types["ObjFunc"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int8Ptr), TYPE(Int8Ptr), TYPE(Int32)}, "ObjFunc");
    auto tmpFuncPtr = llvm::PointerType::getUnqual(types["ObjFunc"]);
    //Pointer to pointer
    auto tmpUpvalArr = llvm::PointerType::getUnqual(llvm::PointerType::getUnqual(types["ObjUpvalue"]));
    types["ObjClosure"] = llvm::StructType::create(*ctx, {types["Obj"], tmpFuncPtr, tmpUpvalArr, TYPE(Int32)}, "ObjClosure");
}

void llvmHelpers::addHelperFunctionsToModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx, llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    createLLVMTypes(ctx, types);
    CREATE_FUNC("asNum", false, TYPE(Double),  TYPE(Int64));
    CREATE_FUNC("print", false, TYPE(Void),  TYPE(Int64));
    CREATE_FUNC("createStr", false, TYPE(Int64), TYPE(Int8Ptr));
    CREATE_FUNC("tyErrSingle", false, TYPE(Void), TYPE(Int8Ptr), TYPE(Int8Ptr), TYPE(Int32), TYPE(Int64));
    CREATE_FUNC("tyErrDouble", false, TYPE(Void), TYPE(Int8Ptr), TYPE(Int8Ptr), TYPE(Int32), TYPE(Int64), TYPE(Int64));
    CREATE_FUNC("strAdd", false, TYPE(Int64), TYPE(Int64), TYPE(Int64), TYPE(Int8Ptr), TYPE(Int32));
    CREATE_FUNC("stopThread", false, TYPE(Void));
    CREATE_FUNC("createArr", false, TYPE(Int64), TYPE(Int32));
    CREATE_FUNC("getArrPtr", false, TYPE(Int64Ptr), TYPE(Int64));
    CREATE_FUNC("gcSafepoint", false, TYPE(Int1));
    CREATE_FUNC("createHashMap", true, TYPE(Int64), TYPE(Int32));
    CREATE_FUNC("createFunc", false, TYPE(Int64), TYPE(Int8Ptr), TYPE(Int32), TYPE(Int8Ptr));
    CREATE_FUNC("createUpvalue", false, llvm::PointerType::getUnqual(types["ObjUpvalue"]));
    CREATE_FUNC("getUpvalue", false, llvm::PointerType::getUnqual(types["ObjUpvalue"]), TYPE(Int64), TYPE(Int32));
    CREATE_FUNC("createClosure", true, TYPE(Int64), TYPE(Int64), TYPE(Int32));
    CREATE_FUNC("addGCRoot", false, TYPE(Void), llvm::PointerType::getUnqual(TYPE(Int64)));

    buildLLVMNativeFunctions(module, ctx, builder, types);
}

void llvmHelpers::runModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::orc::KaleidoscopeJIT>& JIT, std::unique_ptr<llvm::LLVMContext>& ctx, bool optimize){
    uintptr_t* mainThreadStackStart = getStackPointer();
    memory::gc->addStackStart(std::this_thread::get_id(), mainThreadStackStart);
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
    // This one corresponds to a typical -O3 optimization pipeline.
    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    if(optimize) MPM.run(*module, MAM);
    llvm::errs()<<"-------------- Optimized module --------------\n";
    module->print(llvm::errs(), nullptr);

    auto RT = JIT->getMainJITDylib().createResourceTracker();

    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), std::move(ctx));
    llvm::ExitOnError()(JIT->addModule(std::move(TSM), RT));

    llvm::orc::ExecutorSymbolDef ExprSymbol = llvm::ExitOnError()(JIT->lookup("mainFn"));
    assert(ExprSymbol.getAddress() && "Function not found");

    void (*FP)() = ExprSymbol.getAddress().toPtr<void (*)()>();
    double duration1 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
    FP();
    double duration2 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
    std::cout<<"Duration in ms: "<<duration2 - duration1<<"\n";
    llvm::ExitOnError()(RT->remove());
}

void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx, llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    auto createFunc = [&](string name, llvm::FunctionType *FT){
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::PrivateLinkage, name, module.get());
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx, "entry", F);
        builder.SetInsertPoint(BB);
        return F;
    };
    // Extremely cursed, but using lambdas that are immediately called avoids naming conflicts
    // No encode/decodeObj right now, need to think of how to represent objects in llvm IR
    [&]{
        llvm::Function* f = createFunc("encodeBool",llvm::FunctionType::get(TYPE(Int64), TYPE(Int1), false));
        // x ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE
        auto tmp = builder.CreateSelect(f->getArg(0), builder.getInt64(MASK_SIGNATURE_TRUE), builder.getInt64(MASK_SIGNATURE_FALSE));
        builder.CreateRet(tmp);
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("encodeNull",llvm::FunctionType::get(TYPE(Int64), false));
        builder.CreateRet(llvm::ConstantInt::get(TYPE(Int64), encodeNil()));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("decodeBool",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // At this point we know that arg is either MASK_SIGNATURE_TRUE or MASK_SIGNATURE_FALSE, so just cmp with MASK_SIGNATURE_TRUE
        builder.CreateRet(builder.CreateICmpEQ(f->getArg(0), builder.getInt64(MASK_SIGNATURE_TRUE)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNum",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // (x & MASK_QNAN) != MASK_QNAN
        auto arg = f->getArg(0);
        auto constant = builder.getInt64(MASK_QNAN);
        builder.CreateRet(builder.CreateICmpNE(builder.CreateAnd(arg, constant), constant));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isBool",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // x == MASK_SIGNATURE_TRUE || x == MASK_SIGNATURE_FALSE
        auto arg = f->getArg(0);

        auto const1 = builder.getInt64(MASK_SIGNATURE_TRUE);
        auto const2 = builder.getInt64(MASK_SIGNATURE_FALSE);
        builder.CreateRet(builder.CreateOr(builder.CreateICmpEQ(arg, const1), builder.CreateICmpEQ(arg, const2)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNull",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // x == MASK_SIGNATURE_NIL
        builder.CreateRet(builder.CreateICmpEQ(f->getArg(0), builder.getInt64(MASK_SIGNATURE_NIL)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isObj",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ
        auto arg = f->getArg(0);

        auto const0 = builder.getInt64(MASK_SIGNATURE);
        auto const1 = builder.getInt64(MASK_SIGNATURE_OBJ);
        builder.CreateRet(builder.CreateICmpEQ(builder.CreateAnd(arg, const0), const1));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isTruthy",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        // (isBool(x) && !decodeBool(x)) || isNil(x)
        // Even though x might not be bool it will still be decoded as a bool, but that's just an icmp so it's ok
        auto arg = f->getArg(0);

        auto c1 = builder.CreateCall(module->getFunction("isBool"), arg);
        auto c2 = builder.CreateNot(builder.CreateCall(module->getFunction("decodeBool"), arg));
        auto c3 = builder.CreateCall(module->getFunction("isNull"), arg);
        builder.CreateRet(builder.CreateNot(builder.CreateOr(builder.CreateAnd(c1, c2), c3)));
    llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("noop",llvm::FunctionType::get(TYPE(Void), false));
        // Used to avoid IR level when creating a gc safepoint
        builder.CreateRetVoid();
        llvm::verifyFunction(*f);
    }();
}
