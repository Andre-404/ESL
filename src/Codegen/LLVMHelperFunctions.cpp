#include "LLVMHelperFunctions.h"
#include "../Runtime/LLVMHelperExports.h"
#include "../Runtime/nativeFunctionsExports.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ModRef.h"

#include <unordered_set>
#include <iostream>

#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, {__VA_ARGS__}, isVarArg), llvm::Function::ExternalLinkage, #name, module.get())
#define TYPE(type) llvm::Type::get ## type ## Ty(*ctx)
#define PTR_TY(type) llvm::PointerType::getUnqual(type)

// TODO: make this work only on windows, linux shouldn't have dllimport storage( i think)
llvm::Function* wrapFn(llvm::Function*  fn){
    fn->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    return fn;
}

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
    types["ObjClosure"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int8), TYPE(Int8), PTR_TY(TYPE(Int8)),
                                                                     PTR_TY(TYPE(Int8))}, "ObjClosure");
    types["ObjClosureAligned"] = llvm::StructType::create(*ctx, {types["ObjClosure"], TYPE(Int64)}, "ObjClosureAligned");
    types["ObjClosurePtr"] = PTR_TY(types["ObjClosure"]);


    auto classType = llvm::StructType::create(*ctx, "ObjClass");
    types["ObjClassPtr"] = PTR_TY(classType);
    auto fnTy = llvm::FunctionType::get(TYPE(Int32), {getESLValType(*ctx)}, false);
    // Last int64 is for padding
    classType->setBody({types["Obj"], TYPE(Int16), TYPE(Int16), TYPE(Int32), TYPE(Int32), PTR_TY(TYPE(Int8)), PTR_TY(fnTy), PTR_TY(fnTy), TYPE(Int64)});
    types["ObjClass"] = classType;

    types["ObjInstance"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int32), types["ObjClassPtr"]}, "ObjInstance");
    types["ObjInstancePtr"] = PTR_TY(types["ObjInstance"]);

    types["ObjArrayStorage"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int32)}, "ObjArrayStorage");
    types["ObjArrayStoragePtr"] = PTR_TY(types["ObjArrayStorage"]);

    types["ObjArray"] = llvm::StructType::create(*ctx, {types["Obj"], TYPE(Int8), TYPE(Int32), types["ObjArrayStoragePtr"]}, "ObjArray");
    types["ObjArrayPtr"] = PTR_TY(types["ObjArray"]);
}

void buildLLVMNativeFunctions(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx,
                              llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types);

void llvmHelpers::addHelperFunctionsToModule(std::unique_ptr<llvm::Module>& module, std::unique_ptr<llvm::LLVMContext> &ctx,
                                             llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<string, llvm::Type*>& types){
    createLLVMTypes(ctx, types);
    llvm::Type* eslValTy = getESLValType(*ctx);
    auto fn = wrapFn(CREATE_FUNC(runtimeError, false, TYPE(Void), PTR_TY(TYPE(Int8)), TYPE(Int8), TYPE(Int64), TYPE(Int64), TYPE(Int64)));
    fn->addFnAttr(llvm::Attribute::NoReturn);
    fn->addFnAttr(llvm::Attribute::Cold);
    fn->addFnAttr(llvm::Attribute::Memory);
    fn->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    fn->addFnAttr(llvm::Attribute::NoFree);
    fn->addFnAttr(llvm::Attribute::NoRecurse);
    fn->addFnAttr(llvm::Attribute::MustProgress);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    // Helper from C std lib
    wrapFn(CREATE_FUNC(printf, true, TYPE(Int32), PTR_TY(TYPE(Int8))));
    wrapFn(CREATE_FUNC(exit, false, TYPE(Void), TYPE(Int32)));

    // ret: Value, args: lhs, rhs - both are known to be strings
    wrapFn(CREATE_FUNC(strAdd, false, eslValTy, builder.getPtrTy(), eslValTy, eslValTy));
    wrapFn(CREATE_FUNC(strCmp, false, eslValTy, eslValTy, eslValTy));
    // Invoked by gc.safepoint
    wrapFn(CREATE_FUNC(stopThread, false, TYPE(Void), builder.getPtrTy()));
    // ret: Value, args: arr size
    wrapFn(CREATE_FUNC(createArr, false, eslValTy, builder.getPtrTy(), TYPE(Int32)));
    wrapFn(CREATE_FUNC(getArrPtr, false, PTR_TY(eslValTy), eslValTy));
    wrapFn(CREATE_FUNC(getArrSize, false, TYPE(Int64), eslValTy));

    wrapFn(CREATE_FUNC(gcInit, false, TYPE(Void), PTR_TY(TYPE(Int64))));
    wrapFn(CREATE_FUNC(gcInternStr, false ,TYPE(Void), eslValTy));
    // Marks a pointer to a variable as a gc root, this is used for all global variables
    wrapFn(CREATE_FUNC(addGCRoot, false, TYPE(Void), PTR_TY(eslValTy)));
    fn = wrapFn(CREATE_FUNC(gcAlloc, false, types["ObjPtr"], builder.getPtrTy(), TYPE(Int64)));
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
    wrapFn(CREATE_FUNC(createHashMap, true, eslValTy, builder.getPtrTy(), TYPE(Int32)));
    // Creates a function enclosed in a closure, args: function ptr, arity, name, num of freevars, followed by n freevars
    wrapFn(CREATE_FUNC(createClosure, true, eslValTy, builder.getPtrTy(), PTR_TY(TYPE(Int8)), TYPE(Int8), PTR_TY(TYPE(Int8)), TYPE(Int32)));
    // ret: Value, args: ObjHashmap, ObjString that will be used as an index into the map
    wrapFn(CREATE_FUNC(hashmapGetV, false, eslValTy, types["ObjPtr"], types["ObjPtr"]));
    // ret: void, args: ObjHashmap, ObjString(index into the map), Value to be inserted
    wrapFn(CREATE_FUNC(hashmapSetV, false, TYPE(Void), types["ObjPtr"], types["ObjPtr"], eslValTy));

    // ret: void, args: wrapper function ptr, alloca ptr with args
    wrapFn(CREATE_FUNC(createNewThread, false, TYPE(Void), builder.getPtrTy(), builder.getPtrTy()));
    // ret: void, args: frame address
    wrapFn(CREATE_FUNC(threadInit, false, TYPE(Void), builder.getPtrTy()));
    // ret: void, args: threadData ptr
    wrapFn(CREATE_FUNC(threadDestruct, false, TYPE(Void), builder.getPtrTy()));

    buildLLVMNativeFunctions(module, ctx, builder, types);
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
        // MASK_SIGNATURE_BOOL | x
        auto bitcastArg = builder.CreateZExt(f->getArg(0), builder.getInt64Ty());
        auto ret = builder.CreateOr(builder.getInt64(mask_signature_bool), bitcastArg);
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
        builder.CreateRet(builder.CreateICmpEQ(ESLValToI64(f->getArg(0), builder),builder.getInt64(mask_signature_true)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNum",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // (x & MASK_QNAN) != MASK_QNAN
        auto arg = ESLValToI64(f->getArg(0), builder);
        auto constant = builder.getInt64(mask_qnan);
        builder.CreateRet(builder.CreateICmpNE(builder.CreateAnd(arg, constant), constant));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isBool",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // (x & MASK_SIGNATURE_BOOL) == MASK_SIGNATURE_BOOL
        auto arg = ESLValToI64(f->getArg(0), builder);

        llvm::Value* argAnd = builder.CreateAnd(builder.getInt64(mask_signature_bool), arg);
        builder.CreateRet(builder.CreateICmpEQ(builder.getInt64(mask_signature_bool), argAnd));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isNull",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // x == MASK_SIGNATURE_NIL
        builder.CreateRet(builder.CreateICmpEQ(ESLValToI64(f->getArg(0), builder), builder.getInt64(mask_signature_null)));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isObj",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ
        auto arg = ESLValToI64(f->getArg(0), builder);

        auto const0 = builder.getInt64(mask_signature);
        auto const1 = builder.getInt64(mask_signature_obj);
        builder.CreateRet(builder.CreateICmpEQ(builder.CreateAnd(arg, const0), const1));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("isTruthy",llvm::FunctionType::get(TYPE(Int1), eslValTy,false));
        // !((isBool(x) && !decodeBool(x)) || isNil(x))
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
        llvm::Function* f = createFunc("encodeObj",llvm::FunctionType::get(eslValTy, {PTR_TY(types["Obj"]), builder.getInt64Ty()},false));
        // MASK_SIGNATURE_OBJ | type | (Int64)x
        auto cast = builder.CreatePtrToInt(f->getArg(0), builder.getInt64Ty());
        auto const1 = builder.getInt64(mask_signature_obj);
        llvm::Value* mask = builder.CreateOr(const1, f->getArg(1));
        builder.CreateRet(builder.CreateIntToPtr(builder.CreateOr(mask, cast), eslValTy));
        llvm::verifyFunction(*f);
    }();
    [&]{
        llvm::Function* f = createFunc("decodeObj",llvm::FunctionType::get(types["ObjPtr"], eslValTy,false));
        // (Obj*)(x & MASK_PAYLOAD_OBJ)
        auto const1 = builder.getInt64(mask_payload_obj);
        builder.CreateRet(builder.CreateIntToPtr(builder.CreateAnd(builder.CreatePtrToInt(f->getArg(0), builder.getInt64Ty()), const1), types["ObjPtr"]));
        llvm::verifyFunction(*f);
    }();
    // gc.safepoint_poll is used by LLVM to place safepoint polls optimally, LLVM requires this function to have external linkage
    [&]{
        llvm::Function *F = llvm::Function::Create(llvm::FunctionType::get(TYPE(Void), builder.getPtrTy(), false), llvm::Function::ExternalLinkage, "safepoint_poll", module.get());
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
        auto call = builder.CreateCall(module->getFunction("stopThread"), F->getArg(0));
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
        // (x & (MASK_SIGNATURE | MASK_PAYLOAD_TYPE)) == (MASK_SIGNATURE_OBJ | expectedType)
        auto arg = ESLValToI64(F->getArg(0), builder);

        llvm::Value* mask = builder.getInt64(mask_signature | mask_payload_type);

        llvm::Value* res = builder.CreateAnd(arg, mask, "val.type");
        llvm::Value* zextObjType = builder.CreateZExt(F->getArg(1), builder.getInt64Ty());
        llvm::Value* expected = builder.CreateOr(builder.getInt64(mask_signature_obj), zextObjType, "expected");
        builder.CreateRet(builder.CreateICmpEQ(res, expected, "is.obj"));

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
        auto ptr = builder.CreateInBoundsGEP(types["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(2)});
        ptr = builder.CreateLoad(types["ObjClassPtr"], ptr);

        llvm::Value* idxStart = builder.CreateInBoundsGEP(types["ObjClass"], ptr, {builder.getInt32(0), builder.getInt32(3)});
        llvm::Value* idxEnd = builder.CreateInBoundsGEP(types["ObjClass"], ptr, {builder.getInt32(0), builder.getInt32(4)});
        idxStart = builder.CreateLoad(TYPE(Int32), idxStart, "idxStart");
        idxEnd = builder.CreateLoad(TYPE(Int32), idxEnd, "idxEnd");

        auto cmp = builder.CreateAnd(builder.CreateICmpUGE(idxStart, subclassIdxStart),
                                             builder.CreateICmpULE(idxEnd, subclassIdxEnd));
        builder.CreateRet(cmp);
        F->insert(F->end(), notObjBB);
        builder.SetInsertPoint(notObjBB);
        builder.CreateRet(builder.getInt1(false));
        llvm::verifyFunction(*F);
    }();

    {
        llvm::Function *F = createFunc("decodeArray",llvm::FunctionType::get(types["ObjArrayPtr"], eslValTy,false));
        llvm::Value* tmp = builder.CreateCall(module->getFunction("decodeObj"), F->getArg(0));
        tmp = builder.CreateBitCast(tmp, types["ObjArrayPtr"]);
        builder.CreateRet(tmp);
        llvm::verifyFunction(*F);
    }
    {
        llvm::Function *F = createFunc("arrWriteBarrier",llvm::FunctionType::get(TYPE(Void), {types["ObjArrayPtr"], eslValTy}, false));
        llvm::Value* ptr = builder.CreateConstInBoundsGEP2_32(types["ObjArray"], F->getArg(0), 0, 1);
        llvm::Value* tmp = builder.CreateLoad(TYPE(Int8), ptr, "has.obj");
        llvm::Value* isObj = builder.CreateCall(module->getFunction("isObj"), F->getArg(1));
        isObj = builder.CreateZExt(isObj, TYPE(Int8));
        tmp = builder.CreateOr(tmp, isObj, "has.obj.new");
        builder.CreateStore(tmp, ptr);
        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }
}
