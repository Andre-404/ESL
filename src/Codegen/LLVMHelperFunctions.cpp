#include "LLVMHelperFunctions.h"
#include "LLVMNativeFunctionImplementation.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Scalar/PlaceSafepoints.h"

#include <unordered_set>
#include <iostream>



#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, {__VA_ARGS__}, isVarArg), llvm::Function::ExternalLinkage, name, module.get())
#define TYPE(type) llvm::Type::get ## type ## Ty(*ctx)
#define PTR_TY(type) llvm::PointerType::getUnqual(type)

void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx, llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types);

void createLLVMTypes(std::unique_ptr<llvm::LLVMContext> &ctx, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    types["Obj"] = llvm::StructType::create(*ctx, {llvm::Type::getInt8Ty(*ctx), llvm::Type::getInt1Ty(*ctx)}, "Obj");
    types["ObjPtr"] = PTR_TY(types["Obj"]);
    types["ObjFreevar"] = llvm::StructType::create(*ctx, {types["Obj"], llvm::Type::getInt64Ty(*ctx)}, "ObjFreevar");
    types["ObjFreevarPtr"] = PTR_TY(types["ObjFreevar"]);
    // Pointer to pointer
    auto tmpFreevarArr = PTR_TY(types["ObjFreevarPtr"]);
    types["ObjClosure"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int8), TYPE(Int8), TYPE(Int8Ptr), TYPE(Int8Ptr), tmpFreevarArr}, "ObjClosure");
    types["ObjClosurePtr"] = PTR_TY(types["ObjClosure"]);
}

void llvmHelpers::addHelperFunctionsToModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx, llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    createLLVMTypes(ctx, types);
    // ret: double, args: Value
    CREATE_FUNC("asNum", false, TYPE(Double),  TYPE(Int64));
    CREATE_FUNC("print", false, TYPE(Void),  TYPE(Int64));
    // ret: Value, args: raw C string
    CREATE_FUNC("createStr", false, TYPE(Int64), TYPE(Int8Ptr));
    //
    CREATE_FUNC("tyErrSingle", false, TYPE(Void), TYPE(Int8Ptr), TYPE(Int8Ptr), TYPE(Int32), TYPE(Int64));
    CREATE_FUNC("tyErrDouble", false, TYPE(Void), TYPE(Int8Ptr), TYPE(Int8Ptr), TYPE(Int32), TYPE(Int64), TYPE(Int64));
    // ret: Value, args: lhs, rhs - both are known to be strings
    CREATE_FUNC("strAdd", false, TYPE(Int64), TYPE(Int64), TYPE(Int64));
    // Same as strAdd, but has additional information in case of error, additional args: file name, line
    CREATE_FUNC("strTryAdd", false, TYPE(Int64), TYPE(Int64), TYPE(Int64), TYPE(Int8Ptr), TYPE(Int32));
    // Invoked by gc.safepoint
    CREATE_FUNC("stopThread", false, TYPE(Void));
    // ret: Value, args: arr size
    CREATE_FUNC("createArr", false, TYPE(Int64), TYPE(Int32));
    CREATE_FUNC("getArrPtr", false, TYPE(Int64Ptr), TYPE(Int64));
    // What is this used for?
    CREATE_FUNC("gcSafepoint", false, TYPE(Int1));
    // First argument is number of field, which is then followed by n*2 Value-s
    // Pairs of Values for fields look like this: {Value(string), Value(val)}
    CREATE_FUNC("createHashMap", true, TYPE(Int64), TYPE(Int32));
    // Creates a freevar(no args needed, its initialized to nil)
    CREATE_FUNC("createFreevar", false, types["ObjFreevarPtr"]);
    // Gets a freevar from closure, args: closure structure, index to which freevar to use
    CREATE_FUNC("getFreevar", false, types["ObjFreevarPtr"], types["ObjClosurePtr"], TYPE(Int32));
    // Creates a function enclosed in a closure, args: function ptr, arity, name, num of freevars, followed by n freevars
    CREATE_FUNC("createClosure", true, TYPE(Int64), TYPE(Int8Ptr), TYPE(Int8), TYPE(Int8Ptr), TYPE(Int32));
    // Marks a pointer to a variable as a gc root, this is used for all global variables
    CREATE_FUNC("addGCRoot", false, TYPE(Void), PTR_TY(TYPE(Int64)));
    // ret: Value, args: ObjHashmap, ObjString that will be used as an index into the map
    CREATE_FUNC("hashmapGetV", false, TYPE(Int64), types["ObjPtr"], types["ObjPtr"]);
    // ret: void, args: ObjHashmap, ObjString(index into the map), Value to be inserted
    CREATE_FUNC("hashmapSetV", false, TYPE(Void), types["ObjPtr"], types["ObjPtr"], TYPE(Int64));

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
    llvm::PlaceSafepointsPass SafepointPass;

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
    if(optimize) {
        MPM.run(*module, MAM);
    }
    for(auto& it : module->functions()){
        SafepointPass.run(it, FAM);
    }

    llvm::errs()<<"-------------- Optimized module --------------\n";
    module->print(llvm::errs(), nullptr);

    auto RT = JIT->getMainJITDylib().createResourceTracker();

    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), std::move(ctx));
    llvm::ExitOnError()(JIT->addModule(std::move(TSM), RT));

    llvm::orc::ExecutorSymbolDef ExprSymbol = llvm::ExitOnError()(JIT->lookup("func.main"));
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
        // MASK_QNAN | (MASK_TYPE_TRUE* <i64>x)
        auto bitcastArg = builder.CreateZExt(f->getArg(0), builder.getInt64Ty());
        auto mul = builder.CreateMul(builder.getInt64(MASK_TYPE_TRUE), bitcastArg, "tmp", true, true);
        auto ret = builder.CreateOr(builder.getInt64(MASK_QNAN), mul);
        builder.CreateRet(ret);
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
    [&]{
        llvm::Function* f = createFunc("encodeObj",llvm::FunctionType::get(TYPE(Int64), PTR_TY(types["Obj"]),false));
        // MASK_SIGNATURE_OBJ | (Int64)x
        auto cast = builder.CreatePtrToInt(f->getArg(0), builder.getInt64Ty());
        auto const1 = builder.getInt64(MASK_SIGNATURE_OBJ);
        builder.CreateRet(builder.CreateOr(const1, cast));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("decodeObj",llvm::FunctionType::get(PTR_TY(types["Obj"]), TYPE(Int64),false));
        // (Obj*)(x & MASK_PAYLOAD_OBJ)
        auto const1 = builder.getInt64(MASK_PAYLOAD_OBJ);
        builder.CreateRet(builder.CreateIntToPtr(builder.CreateAnd(f->getArg(0), const1), types["ObjPtr"]));
        llvm::verifyFunction(*f);
    }();
    // gc.safepoint_poll is used by LLVM to place safepoint polls optimally, LLVM requires this function to have external linkage
    [&]{
        llvm::Function *F = llvm::Function::Create(llvm::FunctionType::get(TYPE(Void), false), llvm::Function::ExternalLinkage, "gc.safepoint_poll", module.get());
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx, "entry", F);
        builder.SetInsertPoint(BB);
        llvm::BasicBlock* runGCBB = llvm::BasicBlock::Create(*ctx, "runGC", F);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

        // Atomically load the value, volatile because LLVM would otherwise optimize this away
        auto load = builder.CreateLoad(builder.getInt8Ty(), module->getNamedGlobal("gcFlag"), true);
        load->setAtomic(llvm::AtomicOrdering::Monotonic);
        auto cond = builder.CreateIntCast(load, builder.getInt1Ty(), false);

        // Run gc if flag is true
        builder.CreateCondBr(cond, runGCBB, mergeBB);
        builder.SetInsertPoint(runGCBB);
        builder.CreateCall(module->getFunction("stopThread"));
        builder.CreateRetVoid();

        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }();

    [&]{
        llvm::Function *F = createFunc("decodeClosure",llvm::FunctionType::get(types["ObjClosurePtr"], TYPE(Int64),false));
        llvm::Value* tmp = builder.CreateCall(module->getFunction("decodeObj"), F->getArg(0));
        tmp = builder.CreateBitCast(tmp, types["ObjClosurePtr"]);
        builder.CreateRet(tmp);
        llvm::verifyFunction(*F);
    }();
    /*[&]{
        llvm::Function* f = createFunc("isString",llvm::FunctionType::get(TYPE(Int1), TYPE(Int64),false));
        auto arg = f->getArg(0);

        auto cond1 = builder.CreateCall(module->getFunction("isObj"));
        auto const0 = builder.getInt64(+ObjType::STRING);

        llvm::verifyFunction(*f);
    }();*/
}
