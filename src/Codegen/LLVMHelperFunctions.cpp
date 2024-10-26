#include "LLVMHelperFunctions.h"
#include "../Runtime/LLVMHelperExports.h"
#include "../Runtime/nativeFunctionsExports.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Scalar/PlaceSafepoints.h"
#include "llvm/Support/ModRef.h"

#include <unordered_set>
#include <iostream>

#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, {__VA_ARGS__}, isVarArg), llvm::Function::ExternalLinkage, name, module.get())
#define TYPE(type) llvm::Type::get ## type ## Ty(*ctx)
#define PTR_TY(type) llvm::PointerType::getUnqual(type)

using namespace llvmHelpers;

llvm::Type* llvmHelpers::getESLValType(llvm::LLVMContext& ctx){
    // Opaque pointer
    return llvm::PointerType::get(ctx, 1);
}

void createLLVMTypes(std::unique_ptr<llvm::LLVMContext> &ctx, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    // First 2 bytes are GC info
    auto padding = llvm::ArrayType::get(TYPE(Int8), 2);
    types["Obj"] = llvm::StructType::create(*ctx, {padding, TYPE(Int8)}, "Obj");
    types["ObjPtr"] = PTR_TY(types["Obj"]);
    types["ObjString"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int32), PTR_TY(TYPE(Int8))}, "ObjString");
    types["ObjStringPtr"] = PTR_TY(types["ObjString"]);
    types["ObjFreevar"] = llvm::StructType::create(*ctx, {types["Obj"], getESLValType(*ctx)}, "ObjFreevar");
    types["ObjFreevarPtr"] = PTR_TY(types["ObjFreevar"]);
    // Pointer to pointer
    auto tmpFreevarArr = PTR_TY(types["ObjFreevarPtr"]);
    types["ObjClosure"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int8), TYPE(Int8), PTR_TY(TYPE(Int8)),
                                                                     PTR_TY(TYPE(Int8)), tmpFreevarArr}, "ObjClosure");
    types["ObjClosurePtr"] = PTR_TY(types["ObjClosure"]);


    auto classType = llvm::StructType::create(*ctx, "ObjClass");
    types["ObjClassPtr"] = PTR_TY(classType);
    auto fnTy = llvm::FunctionType::get(TYPE(Int32), {getESLValType(*ctx)}, false);
    classType->setBody({types["Obj"], PTR_TY(TYPE(Int8)), TYPE(Int32), TYPE(Int32), PTR_TY(fnTy), PTR_TY(fnTy),
                                TYPE(Int64), TYPE(Int64), types["ObjClosurePtr"]});
    types["ObjClass"] = classType;

    types["ObjInstance"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int32), types["ObjClassPtr"]}, "ObjInstance");
    types["ObjInstancePtr"] = PTR_TY(types["ObjInstance"]);
}

void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx,
                              llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types);

void llvmHelpers::addHelperFunctionsToModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx,
                                             llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    createLLVMTypes(ctx, types);
    llvm::Type* eslValTy = getESLValType(*ctx);

    auto fn = CREATE_FUNC("tyErrSingle", false, TYPE(Void), PTR_TY(TYPE(Int8)), PTR_TY(TYPE(Int8)), TYPE(Int32), eslValTy);
    fn->addFnAttr(llvm::Attribute::NoReturn);
    fn->addFnAttr(llvm::Attribute::Cold);
    fn->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    fn->addFnAttr(llvm::Attribute::NoCallback);
    fn->addFnAttr(llvm::Attribute::NoFree);
    fn->addFnAttr(llvm::Attribute::NoRecurse);
    fn->addFnAttr(llvm::Attribute::MustProgress);
    fn = CREATE_FUNC("tyErrDouble", false, TYPE(Void), PTR_TY(TYPE(Int8)), PTR_TY(TYPE(Int8)), TYPE(Int32), eslValTy, eslValTy);
    fn->addFnAttr(llvm::Attribute::NoReturn);
    fn->addFnAttr(llvm::Attribute::Cold);
    fn->addFnAttr(llvm::Attribute::Memory);
    fn->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    fn->addFnAttr(llvm::Attribute::NoFree);
    fn->addFnAttr(llvm::Attribute::NoRecurse);
    fn->addFnAttr(llvm::Attribute::MustProgress);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    // Helper from C std lib
    CREATE_FUNC("printf", true, TYPE(Int32), PTR_TY(TYPE(Int8)));
    CREATE_FUNC("exit", false, TYPE(Void), TYPE(Int32));

    // ret: Value, args: lhs, rhs - both are known to be strings
    CREATE_FUNC("strAdd", false, eslValTy, eslValTy, eslValTy);
    // Same as strAdd, but has additional information in case of error, additional args: file name, line
    CREATE_FUNC("strTryAdd", false, eslValTy, eslValTy, eslValTy, PTR_TY(TYPE(Int8)), TYPE(Int32));
    CREATE_FUNC("strCmp", false, eslValTy, eslValTy, eslValTy);
    // Invoked by gc.safepoint
    CREATE_FUNC("stopThread", false, TYPE(Void));
    // ret: Value, args: arr size
    CREATE_FUNC("createArr", false, eslValTy, TYPE(Int32));
    CREATE_FUNC("getArrPtr", false, PTR_TY(eslValTy), eslValTy);
    CREATE_FUNC("getArrSize", false, TYPE(Int64), eslValTy);

    CREATE_FUNC("gcInit", false, TYPE(Void), PTR_TY(TYPE(Int64)), PTR_TY(TYPE(Int8)));
    CREATE_FUNC("gcInternStr", false ,TYPE(Void), eslValTy);
    // Marks a pointer to a variable as a gc root, this is used for all global variables
    CREATE_FUNC("addGCRoot", false, TYPE(Void), PTR_TY(eslValTy));
    fn = CREATE_FUNC("gcAlloc", false, types["ObjPtr"], TYPE(Int32));
    fn->addFnAttr(llvm::Attribute::get(*ctx, "allockind", "alloc"));
    fn->addFnAttr(llvm::Attribute::NoRecurse);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    fn->addFnAttr(llvm::Attribute::NoFree);
    fn->addFnAttr(llvm::Attribute::WillReturn);
    fn->addFnAttr(llvm::Attribute::MustProgress);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    llvm::AttributeList a;
    llvm::AttrBuilder b(*ctx);
    b.addAlignmentAttr(8);
    fn->addRetAttr(b.getAttribute(llvm::Attribute::Alignment));
    b.addDereferenceableAttr(8);
    fn->addRetAttr(b.getAttribute(llvm::Attribute::Dereferenceable));
    fn->addRetAttr(llvm::Attribute::NonNull);
    fn->addRetAttr(llvm::Attribute::NoUndef);
    // First argument is number of field, which is then followed by n*2 Value-s
    // Pairs of Values for fields look like this: {Value(string), Value(val)}
    CREATE_FUNC("createHashMap", true, eslValTy, TYPE(Int32));
    // Creates a freevar(no args needed, its initialized to nil)
    CREATE_FUNC("createFreevar", false, types["ObjFreevarPtr"]);
    // Gets a freevar from closure, args: closure structure, index to which freevar to use
    CREATE_FUNC("getFreevar", false, types["ObjFreevarPtr"], eslValTy, TYPE(Int32));
    // Creates a function enclosed in a closure, args: function ptr, arity, name, num of freevars, followed by n freevars
    CREATE_FUNC("createClosure", true, eslValTy, PTR_TY(TYPE(Int8)), TYPE(Int8), PTR_TY(TYPE(Int8)), TYPE(Int32));
    // ret: Value, args: ObjHashmap, ObjString that will be used as an index into the map
    CREATE_FUNC("hashmapGetV", false, eslValTy, types["ObjPtr"], types["ObjPtr"]);
    // ret: void, args: ObjHashmap, ObjString(index into the map), Value to be inserted
    CREATE_FUNC("hashmapSetV", false, TYPE(Void), types["ObjPtr"], types["ObjPtr"], eslValTy);

    buildLLVMNativeFunctions(module, ctx, builder, types);
}

void llvmHelpers::runModule(std::unique_ptr<llvm::Module> module, std::unique_ptr<llvm::LLVMContext> ctx,
                            std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT, std::unique_ptr<llvm::TargetMachine> machine,
                            bool shouldJIT){
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
    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    MPM.run(*module, MAM);
    for(auto& it : module->functions()) {
        SafepointPass.run(it, FAM);
    }


    #ifdef COMPILER_DEBUG
    llvm::errs()<<"-------------- Optimized module --------------\n";
    module->print(llvm::errs(), nullptr);
    #endif

    if(shouldJIT) {
        auto RT = JIT->getMainJITDylib().createResourceTracker();

        auto TSM = llvm::orc::ThreadSafeModule(std::move(module), std::move(ctx));
        llvm::ExitOnError()(JIT->addModule(std::move(TSM), RT));

        llvm::orc::ExecutorSymbolDef ExprSymbol = llvm::ExitOnError()(JIT->lookup("func.main"));
        assert(ExprSymbol.getAddress() && "Function not found");

        int (*FP)(int, char*) = ExprSymbol.getAddress().toPtr< int(*)(int, char*)>();
        FP(0, nullptr);
        llvm::ExitOnError()(RT->remove());
    }else{
        /*auto Filename = "output.o";
        std::error_code EC;
        llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

        if (EC) {
            llvm::errs() << "Could not open file: " << EC.message();
            return;
        }
        llvm::legacy::PassManager pass;

        auto FileType = llvm::CodeGenFileType::CGFT_ObjectFile;

        if (machine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
            llvm::errs() << "TargetMachine can't emit a file of this type";
            return;
        }

        pass.run(*module);
        dest.flush();*/
    }
}

static llvm::Value* ESLValToI64(llvm::Value* val, llvm::IRBuilder<>& builder){
    return builder.CreatePtrToInt(val, builder.getInt64Ty());
}
static llvm::Constant* ESLConstToI64(llvm::Constant* constant){
    return llvm::ConstantExpr::getPtrToInt(constant, llvm::Type::getInt64Ty(constant->getContext()));
}


void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx,
                              llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    auto createFunc = [&](string name, llvm::FunctionType *FT){
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::PrivateLinkage, name, module.get());
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx, "entry", F);
        builder.SetInsertPoint(BB);
        return F;
    };
    llvm::Type* eslValTy = getESLValType(*ctx);
    // Extremely cursed, but using lambdas that are immediately called avoids naming conflicts
    [&]{
        llvm::Function* f = createFunc("encodeBool",llvm::FunctionType::get(eslValTy, TYPE(Int1), false));
        // MASK_QNAN | (MASK_TYPE_TRUE* <i64>x)
        auto bitcastArg = builder.CreateZExt(f->getArg(0), builder.getInt64Ty());
        auto mul = builder.CreateMul(builder.getInt64(MASK_TYPE_TRUE), bitcastArg, "tmp", true, true);
        auto ret = builder.CreateOr(builder.getInt64(MASK_QNAN), mul);
        builder.CreateRet(builder.CreateIntToPtr(ret, eslValTy));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("encodeNull",llvm::FunctionType::get(eslValTy, false));
        builder.CreateRet(builder.CreateIntToPtr(llvm::ConstantInt::get(TYPE(Int64), encodeNil()), eslValTy));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("decodeBool",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // At this point we know that arg is either MASK_SIGNATURE_TRUE or MASK_SIGNATURE_FALSE, so just cmp with MASK_SIGNATURE_TRUE
        builder.CreateRet(builder.CreateICmpEQ(ESLValToI64(f->getArg(0), builder),builder.getInt64(MASK_SIGNATURE_TRUE)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNum",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // (x & MASK_QNAN) != MASK_QNAN
        auto arg = ESLValToI64(f->getArg(0), builder);
        auto constant = builder.getInt64(MASK_QNAN);
        builder.CreateRet(builder.CreateICmpNE(builder.CreateAnd(arg, constant), constant));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isBool",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // x == MASK_SIGNATURE_TRUE || x == MASK_SIGNATURE_FALSE
        auto arg = ESLValToI64(f->getArg(0), builder);

        auto const1 = builder.getInt64(MASK_SIGNATURE_TRUE);
        auto const2 = builder.getInt64(MASK_SIGNATURE_FALSE);
        builder.CreateRet(builder.CreateOr(builder.CreateICmpEQ(arg, const1), builder.CreateICmpEQ(arg, const2)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNull",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // x == MASK_SIGNATURE_NIL
        builder.CreateRet(builder.CreateICmpEQ(ESLValToI64(f->getArg(0), builder), builder.getInt64(MASK_SIGNATURE_NIL)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isObj",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ
        auto arg = ESLValToI64(f->getArg(0), builder);

        auto const0 = builder.getInt64(MASK_SIGNATURE);
        auto const1 = builder.getInt64(MASK_SIGNATURE_OBJ);
        builder.CreateRet(builder.CreateICmpEQ(builder.CreateAnd(arg, const0), const1));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isTruthy",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
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
        llvm::Function* f = createFunc("encodeObj",llvm::FunctionType::get(eslValTy, PTR_TY(types["Obj"]),false));
        // MASK_SIGNATURE_OBJ | (Int64)x
        auto cast = builder.CreatePtrToInt(f->getArg(0), builder.getInt64Ty());
        auto const1 = builder.getInt64(MASK_SIGNATURE_OBJ);
        builder.CreateRet(builder.CreateIntToPtr(builder.CreateOr(const1, cast), eslValTy));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("decodeObj",llvm::FunctionType::get(types["ObjPtr"], eslValTy,false));
        // (Obj*)(x & MASK_PAYLOAD_OBJ)
        auto const1 = builder.getInt64(MASK_PAYLOAD_OBJ);
        builder.CreateRet(builder.CreateIntToPtr(builder.CreateAnd(builder.CreatePtrToInt(f->getArg(0), builder.getInt64Ty()), const1), types["ObjPtr"]));
        llvm::verifyFunction(*f);
    }();
    // gc.safepoint_poll is used by LLVM to place safepoint polls optimally, LLVM requires this function to have external linkage
    [&]{
        llvm::Function *F = llvm::Function::Create(llvm::FunctionType::get(TYPE(Void), false), llvm::Function::ExternalLinkage, "gc.safepoint_poll", module.get());
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx, "entry", F);
        builder.SetInsertPoint(BB);
        llvm::BasicBlock* runGCBB = llvm::BasicBlock::Create(*ctx, "runGC", F);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

        // Atomically load the value
        auto load = builder.CreateLoad(builder.getInt64Ty(), module->getNamedGlobal("gcFlag"), false);
        load->setAtomic(llvm::AtomicOrdering::Monotonic);
        load->setAlignment(llvm::Align(8));
        auto cond = builder.CreateIntCast(load, builder.getInt1Ty(), false);
        cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(false)});
        // Run gc if flag is true
        builder.CreateCondBr(cond, runGCBB, mergeBB);
        builder.SetInsertPoint(runGCBB);
        auto call = builder.CreateCall(module->getFunction("stopThread"));
        builder.CreateRetVoid();

        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }();

    [&]{
        llvm::Function *F = createFunc("decodeClosure",llvm::FunctionType::get(types["ObjClosurePtr"], eslValTy,false));
        llvm::Value* tmp = builder.CreateCall(module->getFunction("decodeObj"), F->getArg(0));
        tmp = builder.CreateBitCast(tmp, types["ObjClosurePtr"]);
        builder.CreateRet(tmp);
        llvm::verifyFunction(*F);
    }();
    [&]{
        llvm::Function* F = createFunc("isObjType",llvm::FunctionType::get(TYPE(Int1), {eslValTy, TYPE(Int8)},false));
        auto arg = F->getArg(0);

        auto cond1 = builder.CreateCall(module->getFunction("isObj"), arg);
        auto objTy = F->getArg(1);
        llvm::BasicBlock* notObjBB = llvm::BasicBlock::Create(*ctx, "notObj");
        llvm::BasicBlock* checkTypeBB = llvm::BasicBlock::Create(*ctx, "checkType", F);
        cond1 = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond1, builder.getInt1(true)});
        builder.CreateCondBr(cond1, checkTypeBB, notObjBB);

        builder.SetInsertPoint(checkTypeBB);
        auto castPtr = builder.CreateCall(module->getFunction("decodeObj"), arg);
        vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
        auto ptr = builder.CreateInBoundsGEP(types["Obj"], castPtr, idxList);
        auto type = builder.CreateLoad(builder.getInt8Ty(), ptr);
        builder.CreateRet(builder.CreateICmpEQ(type, objTy));

        F->insert(F->end(), notObjBB);
        builder.SetInsertPoint(notObjBB);
        builder.CreateRet(builder.getInt1(false));

        llvm::verifyFunction(*F);
    }();

    [&]{
        llvm::Function* F = createFunc("isInstAndClass",llvm::FunctionType::get(TYPE(Int1), {eslValTy, TYPE(Int32), TYPE(Int32)},false));
        llvm::Value* inst = F->getArg(0);
        llvm::Value* subclassIdxStart = F->getArg(1);
        llvm::Value* subclassIdxEnd = F->getArg(2);

        auto cond1 = builder.CreateCall(module->getFunction("isObjType"),
                                        {inst, builder.getInt8(+ObjType::INSTANCE)});

        llvm::BasicBlock* notObjBB = llvm::BasicBlock::Create(*ctx, "notObj");
        llvm::BasicBlock* checkTypeBB = llvm::BasicBlock::Create(*ctx, "checkClassType", F);
        cond1 = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond1, builder.getInt1(true)});
        builder.CreateCondBr(cond1, checkTypeBB, notObjBB);

        builder.SetInsertPoint(checkTypeBB);
        inst = builder.CreateCall(module->getFunction("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, types["ObjInstancePtr"]);
        auto ptr = builder.CreateInBoundsGEP(types["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(2)});
        ptr = builder.CreateLoad(types["ObjClassPtr"], ptr);

        llvm::Value* idxStart = builder.CreateInBoundsGEP(types["ObjClass"], ptr, {builder.getInt32(0), builder.getInt32(2)});
        llvm::Value* idxEnd = builder.CreateInBoundsGEP(types["ObjClass"], ptr, {builder.getInt32(0), builder.getInt32(3)});
        idxStart = builder.CreateLoad(TYPE(Int32), idxStart, "idxStart");
        idxEnd = builder.CreateLoad(TYPE(Int32), idxEnd, "idxEnd");

        auto cmp = builder.CreateAnd(builder.CreateICmpSGE(subclassIdxStart, idxStart),
                                             builder.CreateICmpSLE(subclassIdxEnd, idxEnd));
        builder.CreateRet(cmp);
        F->insert(F->end(), notObjBB);
        builder.SetInsertPoint(notObjBB);
        builder.CreateRet(builder.getInt1(false));

        llvm::verifyFunction(*F);
    }();
}
