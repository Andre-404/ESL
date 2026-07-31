#include "LLVMHelperFunctions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Module.h"
#include "../Runtime/Objects/objects.h"
#include "../Runtime/Values/valueHelpers.h"
#include "../Runtime/Values/valueHelpersInline.h"

#include <llvm-23/llvm/Support/AtomicOrdering.h>
#include <string>

#define CREATE_FUNC(name, isVarArg, returnType, ...) \
    llvm::Function::Create(llvm::FunctionType::get(returnType, {__VA_ARGS__}, isVarArg), llvm::Function::ExternalLinkage, #name, module)
#define TYPE(type) llvm::Type::get ## type ## Ty(ctx)
#define PTR_TY(type) llvm::PointerType::getUnqual(type)

// TODO: make this work only on windows, linux shouldn't have dllimport storage(i think)
llvm::Function* wrapFn(llvm::Function*  fn){
    fn->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    return fn;
}

llvm::Type* llvmHelpers::getESLValType(llvm::LLVMContext& ctx){
    // Opaque pointer
    return llvm::PointerType::get(ctx, 1);
}

void createInternalTypes(llvm::LLVMContext &ctx, ankerl::unordered_dense::map<std::string, llvm::Type*>& types){
    types["Obj"] = llvm::StructType::create(ctx, {TYPE(Int8), TYPE(Int8)}, "Obj");
    types["ObjPtr"] = PTR_TY(types["Obj"]);
    types["ObjString"] = llvm::StructType::create(ctx, {types["Obj"], TYPE(Int32)}, "ObjString");
    types["ObjStringPtr"] = PTR_TY(types["ObjString"]);
    types["ObjClosure"] = llvm::StructType::create(ctx, {types["Obj"], TYPE(Int8), TYPE(Int8), PTR_TY(TYPE(Int8)),
                                                                     PTR_TY(TYPE(Int8))}, "ObjClosure");
    types["ObjClosureAligned"] = llvm::StructType::create(ctx, {types["ObjClosure"], TYPE(Int64)}, "ObjClosureAligned");
    types["ObjClosurePtr"] = PTR_TY(types["ObjClosure"]);


    auto classType = llvm::StructType::create(ctx, "ObjClass");
    types["ObjClassPtr"] = PTR_TY(classType);
    auto fnTy = llvm::FunctionType::get(TYPE(Int32), { llvmHelpers::getESLValType(ctx)}, false);
    // Last int64 is for padding
    classType->setBody({types["Obj"], TYPE(Int16), TYPE(Int16), TYPE(Int32), TYPE(Int32), PTR_TY(TYPE(Int8)), PTR_TY(fnTy), PTR_TY(fnTy), TYPE(Int64)});
    types["ObjClass"] = classType;

    types["ObjInstance"] = llvm::StructType::create(ctx, {types["Obj"], TYPE(Int32), types["ObjClassPtr"]}, "ObjInstance");
    types["ObjInstancePtr"] = PTR_TY(types["ObjInstance"]);

    types["ObjArrayStorage"] = llvm::StructType::create(ctx, {types["Obj"], TYPE(Int32)}, "ObjArrayStorage");
    types["ObjArrayStoragePtr"] = PTR_TY(types["ObjArrayStorage"]);

    types["ObjArray"] = llvm::StructType::create(ctx, {types["Obj"], TYPE(Int8), TYPE(Int32), types["ObjArrayStoragePtr"]}, "ObjArray");
    types["ObjArrayPtr"] = PTR_TY(types["ObjArray"]);
}

void buildLLVMNativeFunctions(llvm::Module& module, llvm::LLVMContext& ctx,
                              llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<std::string, llvm::Type*>& types);

void llvmHelpers::addHelperFunctionsToModule(llvm::Module& module, llvm::LLVMContext& ctx,
                                             llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<std::string, llvm::Type*>& types){
    createInternalTypes(ctx, types);
    llvm::Type* eslValTy = getESLValType(ctx);
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
    wrapFn(CREATE_FUNC(strAdd, false, eslValTy, eslValTy, eslValTy));
    fn = wrapFn(CREATE_FUNC(strCmp, false, eslValTy, eslValTy, eslValTy));
    fn->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    // Invoked by gc.safepoint
    wrapFn(CREATE_FUNC(safepoint, false, TYPE(Void)));
    // ret: Value, args: arr size
    wrapFn(CREATE_FUNC(createArr, false, eslValTy, TYPE(Int32)));

    wrapFn(CREATE_FUNC(gcInit, false, PTR_TY(TYPE(Int8)), PTR_TY(TYPE(Int64))));
    wrapFn(CREATE_FUNC(gcInternStr, false ,TYPE(Void), eslValTy));
    // Marks a pointer to a variable as a gc root, this is used for all global variables
    wrapFn(CREATE_FUNC(addGCRoot, false, TYPE(Void), PTR_TY(eslValTy)));
    fn = wrapFn(CREATE_FUNC(gcAlloc, false, types["ObjPtr"], TYPE(Int64)));
    fn->addFnAttr(llvm::Attribute::get(ctx, "allockind", "alloc"));
    fn->addFnAttr(llvm::Attribute::NoRecurse);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    fn->addFnAttr(llvm::Attribute::NoFree);
    fn->addFnAttr(llvm::Attribute::WillReturn);
    fn->addFnAttr(llvm::Attribute::MustProgress);
    fn->addFnAttr(llvm::Attribute::NoCallback);
    llvm::AttrBuilder b(ctx);
    b.addAlignmentAttr(8);
    fn->addRetAttr(b.getAttribute(llvm::Attribute::Alignment));
    b.addDereferenceableAttr(8);
    fn->addRetAttr(b.getAttribute(llvm::Attribute::Dereferenceable));
    fn->addRetAttr(llvm::Attribute::NonNull);
    fn->addRetAttr(llvm::Attribute::NoUndef);
    // First argument is number of field, which is then followed by n*2 Value-s
    // Pairs of Values for fields look like this: {Value(string), Value(val)}
    wrapFn(CREATE_FUNC(createHashMap, true, eslValTy, TYPE(Int32)));
    // Creates a function enclosed in a closure, args: function ptr, arity, name, num of freevars, followed by n freevars
    wrapFn(CREATE_FUNC(createClosure, true, eslValTy, PTR_TY(TYPE(Int8)), TYPE(Int8), PTR_TY(TYPE(Int8)), TYPE(Int32)));
    // ret: Value, args: ObjHashmap, ObjString that will be used as an index into the map
    wrapFn(CREATE_FUNC(hashmapGetV, false, eslValTy, types["ObjPtr"], types["ObjPtr"]));
    // ret: void, args: ObjHashmap, ObjString(index into the map), Value to be inserted
    wrapFn(CREATE_FUNC(hashmapSetV, false, TYPE(Void), types["ObjPtr"], types["ObjPtr"], eslValTy));

    // ret: void, args: wrapper function ptr, alloca ptr with args, argc
    wrapFn(CREATE_FUNC(createNewThread, false, TYPE(Void), builder.getPtrTy(), builder.getPtrTy(), builder.getInt64Ty()));
    // ret: void, args: frame address, malloc-ed args
    wrapFn(CREATE_FUNC(threadInit, false, TYPE(Void), builder.getPtrTy(), builder.getPtrTy()));
    // ret: void, args: threadData ptr
    wrapFn(CREATE_FUNC(threadDestruct, false, TYPE(Void), ));

    wrapFn(CREATE_FUNC(flush_wb, false, TYPE(Void)));
    wrapFn(CREATE_FUNC(endProgram, false, TYPE(Void)));

    module.getOrInsertGlobal("gcFlag", builder.getInt8Ty());
    llvm::GlobalVariable* gvar = module.getNamedGlobal("gcFlag");
    gvar->setLinkage(llvm::GlobalVariable::PrivateLinkage);
    gvar->setInitializer(builder.getInt8(0));
    gvar->setAlignment(llvm::Align::Of<uint64_t>());

    buildLLVMNativeFunctions(module, ctx, builder, types);
}

static llvm::Value* ESLValToI64(llvm::Value* val, llvm::IRBuilder<>& builder){
    return builder.CreatePtrToInt(val, builder.getInt64Ty());
}
static llvm::Constant* ESLConstToI64(llvm::Constant* constant){
    return llvm::ConstantExpr::getPtrToInt(constant, llvm::Type::getInt64Ty(constant->getContext()));
}


void buildLLVMNativeFunctions(llvm::Module& module, llvm::LLVMContext& ctx,
                              llvm::IRBuilder<>& builder, ankerl::unordered_dense::map<std::string, llvm::Type*>& types){
    auto createFunc = [&](const std::string& name, llvm::FunctionType *FT){
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::PrivateLinkage, name, module);
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(ctx, "entry", F);
        builder.SetInsertPoint(BB);
        return F;
    };
    llvm::Type* eslValTy = llvmHelpers::getESLValType(ctx);
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

        auto c1 = builder.CreateCall(module.getFunction("isBool"), arg);
        auto c2 = builder.CreateNot(builder.CreateCall(module.getFunction("decodeBool"), arg));
        auto c3 = builder.CreateCall(module.getFunction("isNull"), arg);
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

    [&]{
        llvm::Function *F = llvm::Function::Create(llvm::FunctionType::get(TYPE(Void), false), llvm::Function::ExternalLinkage, "safepoint_poll", module);
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(ctx, "entry", F);
        builder.SetInsertPoint(BB);
        llvm::BasicBlock* runGCBB = llvm::BasicBlock::Create(ctx, "runGC", F);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "merge");

        auto *R15MD   = llvm::MDNode::get(ctx, { llvm::MDString::get(ctx, "r15") });
        auto *MDAsVal = llvm::MetadataAsValue::get(ctx, R15MD);
        llvm::Value* tcb = builder.CreateIntrinsic(builder.getInt64Ty(), llvm::Intrinsic::read_register, { MDAsVal });
        tcb = builder.CreateIntToPtr(tcb, builder.getPtrTy());
        auto gep = builder.CreateConstGEP1_32(builder.getInt64Ty(), tcb, 2);
        auto load = builder.CreateLoad(builder.getInt64Ty(), gep);
        load->setAtomic(llvm::AtomicOrdering::Monotonic);
        load->setAlignment(llvm::Align(8));

        // When thread is in RUNNNING(=0) it can only go to HAS_PENDING(=1) involuntarily
        auto cond = builder.CreateIntCast(load, builder.getInt1Ty(), false);
        cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(false)});
        // Run gc if flag is true
        builder.CreateCondBr(cond, runGCBB, mergeBB);
        builder.SetInsertPoint(runGCBB);
        builder.CreateCall(module.getFunction("safepoint"));
        builder.CreateRetVoid();

        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }();

    [&]{
        llvm::Function *F = createFunc("decodeClosure",llvm::FunctionType::get(types["ObjClosurePtr"], eslValTy,false));
        llvm::Value* tmp = builder.CreateCall(module.getFunction("decodeObj"), F->getArg(0));
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

        auto cond1 = builder.CreateCall(module.getFunction("isObjType"),
                                        {inst, builder.getInt8(+rt_type::INSTANCE)});

        llvm::BasicBlock* notObjBB = llvm::BasicBlock::Create(ctx, "notObj");
        llvm::BasicBlock* checkTypeBB = llvm::BasicBlock::Create(ctx, "checkClassType", F);
        cond1 = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond1, builder.getInt1(true)});
        builder.CreateCondBr(cond1, checkTypeBB, notObjBB);

        builder.SetInsertPoint(checkTypeBB);
        inst = builder.CreateCall(module.getFunction("decodeObj"), {inst});
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
        llvm::Value* tmp = builder.CreateCall(module.getFunction("decodeObj"), F->getArg(0));
        tmp = builder.CreateBitCast(tmp, types["ObjArrayPtr"]);
        builder.CreateRet(tmp);
        llvm::verifyFunction(*F);
    }

    // Really funky since i dont want to define the tcb as a struct
    {
        llvm::Function *F = createFunc("gc_write_barrier",llvm::FunctionType::get(TYPE(Void), { eslValTy }, false));
        auto flag = builder.CreateLoad(builder.getInt8Ty(), module.getNamedGlobal("gcFlag"));
        flag->setAtomic(llvm::AtomicOrdering::Acquire);
        auto flag1 = builder.CreateICmpNE(flag, builder.getInt8(0));
        auto isObj = builder.CreateCall(module.getFunction("isObj"), F->getArg(0));
        llvm::BasicBlock* inactiveWB = llvm::BasicBlock::Create(ctx, "inactive", F);
        llvm::BasicBlock* activeWB = llvm::BasicBlock::Create(ctx, "active");

        auto cond = builder.CreateAnd(flag1, isObj);
        cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, { cond, builder.getInt1(false)});
        builder.CreateCondBr(cond, activeWB, inactiveWB);

        builder.SetInsertPoint(inactiveWB);
        builder.CreateRetVoid();
        F->insert(F->end(), activeWB);
        builder.SetInsertPoint(activeWB);

        auto *R15MD   = llvm::MDNode::get(ctx, { llvm::MDString::get(ctx, "r15") });
        auto *MDAsVal = llvm::MetadataAsValue::get(ctx, R15MD);
        llvm::Value* tcb = builder.CreateIntrinsic(builder.getInt64Ty(), llvm::Intrinsic::read_register, { MDAsVal });
        tcb = builder.CreateIntToPtr(tcb, builder.getPtrTy());
        auto mark_buf = builder.CreateConstGEP1_32(builder.getInt64Ty(), tcb, 3);
        mark_buf = builder.CreateLoad(builder.getPtrTy(), mark_buf);
        mark_buf = builder.CreateConstGEP1_32(builder.getInt64Ty(), mark_buf, 1); // Skip over next ptr of linked list

        llvm::Value* cnt = builder.CreateLoad(builder.getInt64Ty(), mark_buf);
        cnt = builder.CreateAdd(cnt, builder.getInt64(1));

        auto slot = builder.CreateInBoundsGEP(builder.getInt64Ty(), mark_buf, { cnt });
        auto obj = builder.CreateCall(module.getFunction("decodeObj"), { F->getArg(0) });

        builder.CreateStore(obj, slot);
        builder.CreateStore(cnt, mark_buf);

        cond = builder.CreateICmpEQ(cnt, builder.getInt64(128));
        cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, { cond, builder.getInt1(false)});

        llvm::BasicBlock* full_buf = llvm::BasicBlock::Create(ctx, "full_buf");
        llvm::BasicBlock* partial_buf = llvm::BasicBlock::Create(ctx, "partial_buf", F);
        builder.CreateCondBr(cond, full_buf, partial_buf);

        builder.SetInsertPoint(partial_buf);
        builder.CreateRetVoid();
        F->insert(F->end(), full_buf);
        builder.SetInsertPoint(full_buf);

        builder.CreateCall(module.getFunction("flush_wb"));

        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }

    {
        llvm::Function *F = createFunc("arrWriteBarrier",llvm::FunctionType::get(TYPE(Void), {types["ObjArrayPtr"], eslValTy}, false));
        llvm::Value* ptr = builder.CreateConstInBoundsGEP2_32(types["ObjArray"], F->getArg(0), 0, 1);
        llvm::Value* isObj = builder.CreateCall(module.getFunction("isObj"), F->getArg(1));

        auto is_obj = llvm::BasicBlock::Create(ctx, "is.obj");
        auto no_obj = llvm::BasicBlock::Create(ctx, "no.obj", F);
        builder.CreateCondBr(isObj, is_obj, no_obj);

        builder.SetInsertPoint(no_obj);
        builder.CreateRetVoid();
        F->insert(F->end(), is_obj);
        builder.SetInsertPoint(is_obj);

        auto st = builder.CreateStore(builder.getInt8(1), ptr);
        st->setAtomic(llvm::AtomicOrdering::Release);

        builder.CreateRetVoid();
        llvm::verifyFunction(*F);
    }
}
