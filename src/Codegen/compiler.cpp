#include "compiler.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "LLVMHelperFunctions.h"
#include "../Runtime/Values/valueHelpers.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"

#include <unordered_set>
#include <iostream>

using namespace compileCore;

Compiler::Compiler(vector<File*>& _srcFiles, fastMap<string, std::pair<int, int>>& _classHierarchy,
                   fastMap<string, types::tyPtr>& natives, const llvm::DataLayout& DL, errorHandler::ErrorHandler& errHandler) :
        errHandler(errHandler),
        ctx(std::make_unique<llvm::LLVMContext>()),
        curModule(std::make_unique<llvm::Module>("Module", *ctx)),
        builder(llvm::IRBuilder<>(*ctx)),
        _tyhelp(builder, *curModule, _classHierarchy),
        _ct(builder, errHandler, _tyhelp),
        _rt(builder, _tyhelp, _ct),
        _inst_builder(builder, _tyhelp, _ct, _rt, errHandler) {

    sourceFiles = _srcFiles;

    setupModule(DL);
    debugEmitter = DebugEmitter(*curModule, *sourceFiles.back(), true);
    generateNativeFuncs(natives);
}

llvm::orc::ThreadSafeModule Compiler::compile(std::shared_ptr<CFG::Function> _code, string mainFnName){
    createMainEntrypoint(mainFnName);
    try {
        for (auto stmt: _code->block.stmts) {
            stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
        }
    }catch(CompilerError err){
        std::cout<<fmt::format("Compiler exited because of error: '{}'.", err.reason);
    }
    // Get all string constants into gc
    pastAllocas([this](auto& tb)  {
        auto val = tb.CreateIntrinsic(tb.getPtrTy(), llvm::Intrinsic::frameaddress, {tb.getInt32(0)});
        tb.CreateCall(safeGetFunc("gcInit"), {curModule->getNamedGlobal("gcFlag")});
        tb.CreateCall(safeGetFunc("threadInit"), { val, llvm::ConstantPointerNull::get(builder.getPtrTy())});
        for(auto strObj : _ct.ESL_strings()) {
            tb.CreateCall(safeGetFunc("gcInternStr"), { strObj });
        }
    });
    // Ends the main function
    builder.CreateRetVoid();
    llvm::verifyFunction(*inProgressFuncs.top().fn);
    llvm::verifyModule(*curModule, &llvm::errs());
    llvm::errs()<<"--------------------Unoptimized module--------------------\n";
#ifdef COMPILER_DEBUG
    curModule->print(llvm::errs(), nullptr);
#endif
    debugEmitter.finalize();
    llvm::errs()<<"--------------------Optimized module--------------------\n";
    optimizeModule(*curModule);
    return std::move(llvm::orc::ThreadSafeModule(std::move(curModule), std::move(ctx)));
}


llvm::Value* Compiler::visitVarDecl(CFG::VarDecl* decl) {
    debugEmitter.emitNewLocation(builder, decl->dbgInfo.varName);
    switch(decl->varType){
        case CFG::VarType::LOCAL:
        case CFG::VarType::FREEVAR: {
            // Alloca at the beginning of the function to make use of mem2reg pass
            pastAllocas([this, decl](auto& tb) {
                auto tmp = tb.CreateAlloca(_tyhelp.getESLValType(), nullptr, decl->dbgInfo.varName.getLexeme());
                variables.insert_or_assign(decl->uuid, tmp);
                debugEmitter.addLocalVarDecl(builder, tmp, decl->dbgInfo.varName, false);
            });
            break;
        }
        case CFG::VarType::GLOBAL_FUNC:
        case CFG::VarType::GLOBAL:{
            string varName = decl->dbgInfo.varName.getLexeme() + std::to_string(decl->uuid);
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(*curModule, _tyhelp.getESLValType(), false,
                llvm::GlobalVariable::PrivateLinkage, _tyhelp.ConstCastToESLVal(builder.getInt64(mask_signature_null)),varName);
            gvar->setAlignment(llvm::Align::Of<Value>());
            // Globals aren't on the stack, so they need to be marked for GC collection separately
            if(decl->varType == CFG::VarType::GLOBAL) {
                builder.CreateCall(safeGetFunc("addGCRoot"), gvar);
            }
            variables.insert_or_assign(decl->uuid, gvar);
            debugEmitter.addGlobalVar(gvar, decl->dbgInfo.varName);
            break;
        }
        default: errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }

    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitVarRead(CFG::VarRead* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    return codegenVarRead(expr->varPtr);
}
llvm::Value* Compiler::visitVarStore(CFG::VarStore* expr) {
    llvm::Value* valToStore = expr->toStore->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    return codegenVarStore(expr->varPtr, valToStore);
}
llvm::Value* Compiler::visitVarReadNative(CFG::VarReadNative* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    // Since native variables are known at compile time reading them is noop
    return nativeFunctions[expr->nativeName];
}

static bool isFloatingPointOp(CFG::ArithmeticOp op){
    return op == CFG::ArithmeticOp::SUB || op == CFG::ArithmeticOp::MUL ||
           op == CFG::ArithmeticOp::DIV || op == CFG::ArithmeticOp::MOD;
}

static llvm::Value* compileArithmeticOp(CFG::ArithmeticOp op, llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs){
    using CFG::ArithmeticOp;
    llvm::Value* ilhs = nullptr;
    llvm::Value* irhs = nullptr;
    if(!isFloatingPointOp(op)){
        // TODO: this can break if val > 2^63
        ilhs = builder.CreateFPToSI(lhs, builder.getInt64Ty());
        irhs = builder.CreateFPToSI(rhs, builder.getInt64Ty());
    }

    llvm::Value* val = nullptr;
    switch(op){
        case ArithmeticOp::SUB: val = builder.CreateFSub(lhs, rhs, "fsub"); break;
        case ArithmeticOp::MUL: val = builder.CreateFMul(lhs, rhs, "fmul"); break;
        case ArithmeticOp::DIV: val = builder.CreateFDiv(lhs, rhs, "fdiv"); break;
        case ArithmeticOp::MOD: val = builder.CreateFRem(lhs, rhs, "frem"); break;
        case ArithmeticOp::AND: val = builder.CreateAnd(ilhs, irhs, "and"); break;
        case ArithmeticOp::OR: val = builder.CreateOr(ilhs, irhs, "or"); break;
        case ArithmeticOp::XOR: val =builder.CreateXor(ilhs, irhs, "xor"); break;
        case ArithmeticOp::BITSHIFT_L: val =builder.CreateShl(ilhs, irhs, "shl"); break;
        case ArithmeticOp::BITSHIFT_R: val =builder.CreateAShr(ilhs, irhs, "ashr"); break;
        case ArithmeticOp::IDIV: {
            auto tmp1 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, lhs);
            auto tmp2 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, rhs);
            val = builder.CreateFDiv(tmp1, tmp2, "floordiv.tmp");
            break;
        }
        // Add is handles separately because of string concatenating
        case ArithmeticOp::ADD:
        default: break;
    }
    if(!isFloatingPointOp(op)) val = builder.CreateSIToFP(val, builder.getDoubleTy());
    return val;
}

llvm::Value* Compiler::visitArithmeticExpr(CFG::ArithmeticExpr* expr) {
    using CFG::ArithmeticOp;
    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    auto castlhs = _tyhelp.ESLValTo(lhs, builder.getDoubleTy());
    auto castrhs = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
    if(expr->opType == ArithmeticOp::ADD) {
        // If types of both lhs and rhs are known unnecessary runtime checks are skipped
        // TODO: maybe split this so if variable type of one expression is known at compile time and the other isn't we only check for that type
        if(exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER)))
            return _tyhelp.CastToESLVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));

        if(exprIsType(expr->lhs, expr->lhs, types::getBasicType(types::TypeFlag::STRING)))
            return builder.CreateCall(safeGetFunc("strAdd"), {lhs, rhs});

        return codegenBinaryAdd(lhs, rhs, expr->dbgInfo.op);
    }
    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        // If either or both aren't numbers, go to error since all other ops work only on numbers
        string err = fmt::format("Operator '{}' expects numbers, got '{}' and '{}'.", expr->dbgInfo.op.getLexeme(), "{}", "{}");
        _rt.createTypeCheckBinary(err, lhs, rhs, TypeHelper::getNumberTypeMasks());
    }

    llvm::Value* val = compileArithmeticOp(expr->opType, builder, castlhs, castrhs);
    return _tyhelp.CastToESLVal(val);
}
llvm::Value* Compiler::visitComparisonExpr(CFG::ComparisonExpr* expr) {
    using CFG::ComparisonOp;
    // Special cases of comparison operators that don't use numbers
    if(expr->opType == ComparisonOp::OR || expr->opType == ComparisonOp::AND)
        return codegenLogicOps(expr->lhs, expr->rhs, expr->opType);

    if(expr->opType == ComparisonOp::EQUAL || expr->opType == ComparisonOp::NOT_EQUAL)
        return codegenCmp(expr->lhs, expr->rhs, expr->opType == ComparisonOp::NOT_EQUAL);

    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        string err = fmt::format("Operator '{}' expects numbers, got '{}' and '{}'.", expr->dbgInfo.op.getLexeme(), "{}", "{}");
        _rt.createTypeCheckBinary(err, lhs, rhs, TypeHelper::getNumberTypeMasks());
    }

    lhs = _tyhelp.ESLValTo(lhs, builder.getDoubleTy());
    rhs = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());
    llvm::Value* val;

    switch(expr->opType){
        case ComparisonOp::LESS: val = builder.CreateFCmpOLT(lhs, rhs, "olt.tmp"); break;
        case ComparisonOp::LESSEQ: val = builder.CreateFCmpOLE(lhs, rhs, "ole.tmp"); break;
        case ComparisonOp::GREAT: val = builder.CreateFCmpOGT(lhs, rhs, "ogt.tmp"); break;
        case ComparisonOp::GREATEQ: val = builder.CreateFCmpOGE(lhs, rhs, "oge.tmp"); break;
        default: errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    return builder.CreateCall(safeGetFunc("encodeBool"), val);
}
llvm::Value* Compiler::visitInstanceofExpr(CFG::InstanceofExpr* expr){
    llvm::Value* inst = expr->lhs->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);

    auto typeWidth = _tyhelp.class_hierarchy(expr->className);
    return builder.CreateCall(safeGetFunc("isInstAndClass"),
        { inst, builder.getInt32(typeWidth.first), builder.getInt32(typeWidth.second) });
}
llvm::Value* Compiler::visitUnaryExpr(CFG::UnaryExpr* expr) {
    using CFG::UnaryOp;
    if(expr->opType >= UnaryOp::NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);

        if (expr->opType >= UnaryOp::FNEG) return codegenNeg(rhs, expr->rhs->exprType, expr->opType, expr->dbgInfo.op);

        // If type is known to be a bool skip the runtime check and just execute the expr
        if(!exprIsType(expr->rhs, types::getBasicType(types::TypeFlag::BOOL)))
            _rt.createTypeCheckUnary("Operator '!' expects boolean value, got '{}'", rhs, TypeHelper::getBoolTypeMasks());

        return _tyhelp.CastToESLVal(builder.CreateXor(_tyhelp.ESLValTo(rhs, builder.getInt64Ty()), mask_type_true));
    }
    return codegenIncrement(expr->opType, expr->rhs, expr->dbgInfo.op);
}

llvm::Value* Compiler::visitLiteralExpr(CFG::LiteralExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.literal);
    switch(expr->val.index()){
        case 0:
            return _tyhelp.CastToESLVal(llvm::ConstantFP::get(*ctx, llvm::APFloat(get<double>(expr->val))));
        case 1:
            return _tyhelp.CastToESLVal(builder.getInt64(get<bool>(expr->val) ? mask_signature_true : mask_signature_false));
        case 2:
            return _tyhelp.CastToESLVal(builder.getInt64(mask_signature_null));
        case 3:
            return _ct.createESLString(get<string>(expr->val));
        default: errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    __builtin_unreachable();
}

llvm::Value* Compiler::visitHashmapExpr(CFG::HashmapExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.brace1);
    vector<llvm::Value*> args = {builder.getInt32(expr->fields.size())};
    // For each field, compile it and get the constant of the field name
    int i = 0;
    for (auto entry : expr->fields) {
        debugEmitter.emitNewLocation(builder, expr->dbgInfo.fields[i].str);
        args.push_back(_ct.createESLString(entry.first));
        args.push_back(entry.second->codegen(this));
        i++;
    }

    return builder.CreateCall(safeGetFunc("createHashMap"), args);
}
llvm::Value* Compiler::visitArrayExpr(CFG::ArrayExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.bracket1);
    vector<llvm::Value*> vals;
    for(auto mem : expr->fields){
        vals.push_back(mem->codegen(this));
    }
    auto arrNum = builder.getInt32(vals.size());
    auto arr = builder.CreateCall(safeGetFunc("createArr"), {arrNum}, "array");
    // I think this should be faster than passing everything to "createArr", but I could be wrong
    llvm::Value* arrPtr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(_tyhelp.internal_obj_ty("ObjArray"), arrPtr, 0, 3);
    storagePtr = builder.CreateLoad(_tyhelp.internal_obj_ty("ObjArrayStoragePtr"), storagePtr, "storage.ptr");
    storagePtr = builder.CreateConstInBoundsGEP1_32(_tyhelp.internal_obj_ty("ObjArrayStorage"), storagePtr, 1, "data.ptr");
    llvm::Value* containsObj = builder.getInt1(0);
    for(int i = 0; i < vals.size(); i++){
        builder.CreateStore(vals[i], builder.CreateConstInBoundsGEP1_32(_tyhelp.getESLValType(), storagePtr, i));
        containsObj = builder.CreateOr(containsObj, builder.CreateCall(safeGetFunc("isObj"), vals[i]));
    }
    arrPtr = builder.CreateConstInBoundsGEP2_32(_tyhelp.internal_obj_ty("ObjArray"), arrPtr, 0, 1, "arr.contains.obj");
    builder.CreateStore(builder.CreateZExt(containsObj, builder.getInt8Ty()), arrPtr);
    return arr;
}

// Returns whether collection is array(0b01) or hashmap(0b10), used in switch
static llvm::Value* collectionTypeCheck(llvm::IRBuilder<>& builder, llvm::Value* collection, llvm::Function* typeChecker){
    // Have to use i8 otherwise we won't know which function returned true
    llvm::Value* cond1 = builder.CreateCall(typeChecker,{collection, builder.getInt8(+object::ObjType::ARRAY)});
    cond1 = builder.CreateZExt(cond1, builder.getInt8Ty());

    llvm::Value* cond2 = builder.CreateCall(typeChecker,{collection, builder.getInt8(+object::ObjType::HASH_MAP)});
    cond2 = builder.CreateZExt(cond2,builder.getInt8Ty());
    cond2 = builder.CreateShl(cond2, 1, "shl", true, true);

    auto num = builder.CreateOr(cond1, cond2);
    return num;
}

llvm::Value* Compiler::visitCollectionGet(CFG::CollectionGet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.accessor);

    bool optArrIndex = exprIsType(expr->field, types::getBasicType(types::TypeFlag::NUMBER));
    bool optMapString = exprIsType(expr->field, types::getBasicType(types::TypeFlag::STRING));

    if(exprIsComplexType(expr->collection, types::TypeFlag::ARRAY))
        return getArrElement(collection, field, optArrIndex, expr->dbgInfo.accessor);

    if(exprIsComplexType(expr->collection, types::TypeFlag::HASHMAP))
        return getMapElement(collection, field, optMapString, expr->dbgInfo.accessor);

    // Uses switch instead of chained comparisons, this should be faster?
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "is.arr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "is.map");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto num = collectionTypeCheck(builder, collection, safeGetFunc("isObjType"));
    _rt.createWeightedSwitch(num, { { 0, errorBB, 0 }, { 1, isArray, 1<<31 }, { 2, isHashmap, 1 << 31 }});

    // Reuses getArrElement and getMapElement
    builder.SetInsertPoint(isArray);
    llvm::Value* arrVal = getArrElement(collection, field, optArrIndex, expr->dbgInfo.accessor);
    isArray = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), isHashmap);
    builder.SetInsertPoint(isHashmap);
    llvm::Value* mapVal = getMapElement(collection, field, optMapString, expr->dbgInfo.accessor);
    isHashmap = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);
    _rt.createTypeCheckFail("Expected an array or hashmap, got '{}'.", collection);

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(_tyhelp.getESLValType(), 2, "collection.get");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;

}

llvm::Value* Compiler::visitCollectionSet(CFG::CollectionSet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);
    llvm::Value* val = expr->toStore->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.accessor);

    bool optArrIndex = exprIsType(expr->field, types::getBasicType(types::TypeFlag::NUMBER));
    bool optMapString = exprIsType(expr->field, types::getBasicType(types::TypeFlag::STRING));
    bool optRhs = exprIsType(expr->toStore, types::getBasicType(types::TypeFlag::NUMBER));

    if(exprIsComplexType(expr->collection, types::TypeFlag::ARRAY))
        return setArrElement(collection, field, val, optArrIndex, optRhs, expr->operationType,expr->dbgInfo.op);

    if(exprIsComplexType(expr->collection, types::TypeFlag::HASHMAP))
        return setMapElement(collection, field, val, optMapString, optRhs, expr->operationType,expr->dbgInfo.op);

    // Uses switch instead of chained comparisons, this should be faster?
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "is.arr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "is.map");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto num = collectionTypeCheck(builder, collection, safeGetFunc("isObjType"));
    _rt.createWeightedSwitch(num, { { 0, errorBB, 0 }, { 1, isArray, 1<<31 }, { 2, isHashmap, 1 << 31 }});

    // Reuses setArrElement and setMapElelent
    builder.SetInsertPoint(isArray);
    llvm::Value* arrVal = setArrElement(collection, field, val, optArrIndex, optRhs, expr->operationType,
                                        expr->dbgInfo.op);
    isArray = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), isHashmap);
    builder.SetInsertPoint(isHashmap);
    llvm::Value* mapVal = setMapElement(collection, field, val, optMapString, optRhs, expr->operationType,
                                         expr->dbgInfo.op);
    isHashmap = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);
    _rt.createTypeCheckFail("Expected an array or hashmap, got '{}'.", collection);

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(_tyhelp.getESLValType(), 2, "collection.set");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;
}

llvm::Value* Compiler::visitConditionalExpr(CFG::ConditionalExpr* expr) {
    auto condtmp = expr->cond->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.questionmark);
    llvm::Value* cond = nullptr;
    // If condition is known to be a boolean the isNull check can be skipped
    if(exprIsType(expr->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(safeGetFunc("decodeBool"), condtmp);
    }
    else cond = builder.CreateCall(safeGetFunc("isTruthy"), condtmp);

    auto phi = llvm::PHINode::Create(_tyhelp.getESLValType(), 2);
    create_if(cond,
        [&]() {
            phi->addIncoming(expr->thenExpr->codegen(this), builder.GetInsertBlock());
        },
        [&]() {
            phi->addIncoming(expr->elseExpr->codegen(this), builder.GetInsertBlock());
        }
    );
    return builder.Insert(phi);
}

llvm::Value* Compiler::visitCallExpr(CFG::CallExpr* expr) {
    bool opt = exprIsComplexType(expr->callee, types::TypeFlag::FUNCTION);
    llvm::Value* closureVal = expr->callee->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.paren1);
    // First param of every function is reserved for closure ptr
    std::vector args = { closureVal };
    for(auto arg : expr->args) args.push_back(arg->codegen(this));

    if(!opt)
        return builder.CreateCall(_rt.createUnoptFunCall(closureVal, expr->args.size()), args);

    auto funcType = std::reinterpret_pointer_cast<types::FunctionType>(expr->callee->exprType);
    // TODO: this should be done in a separate pass
    if(funcType->argCount != expr->args.size()){
        errHandler.reportError(fmt::format("Function expects {} parameters, got {} arguments.", funcType->argCount, expr->args.size()),
                                      expr->dbgInfo.paren1);
        throw CompilerError("Incorrect number of arguments passed");
    }
    return builder.CreateCall(_tyhelp.ty_to_fn(funcType), args, "call.res");
}
llvm::Value* Compiler::visitInvokeExpr(CFG::InvokeExpr* expr) {
    auto inst = expr->inst->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.method);

    vector<llvm::Value*> args = {};
    for(auto arg : expr->args) args.push_back(arg->codegen(this));
    auto exec = [&](llvm::FunctionCallee fn, std::span<llvm::Value*> sp) {
        args.insert(args.begin(), sp.begin(), sp.end());
        auto res = builder.CreateCall(fn, args);
        args.erase(args.begin(), args.begin() + sp.size());
        return res;
    };

    if(exprIsComplexType(expr->inst, types::TypeFlag::INSTANCE)) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(expr->inst->exprType)->klass->name];

        return _inst_builder.optimizeInvoke({ inst, expr->field, *klass.ty }, klass.methodArrPtr, expr->args.size(), exec);
    }
    return _inst_builder.unoptimizedInvoke({ inst, expr->field }, expr->args.size(), exec);
}

llvm::Value* Compiler::visitNewExpr(CFG::NewExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.keyword);
    Class& klass = classes[expr->className];
    string name = expr->className.substr(expr->className.rfind(".")+1, expr->className.size()-1);
    // Instead of initializing instance in some runtime function, we request memory, copy the template and adjust pointers
    // benefit of this is the template already has all the fields nulled, so we don't have to do this at every instantiation
    size_t instSize = curModule->getDataLayout().getTypeAllocSize(klass.instTemplatePtr->getValueType());
    llvm::CallInst* memptr = builder.CreateCall(safeGetFunc("gcAlloc"), {builder.getInt64(instSize)});

    // All the gc info about an object is stored in the first 16 bits of the object
    auto objInfo = builder.CreateLoad(builder.getInt16Ty(), memptr);
    builder.CreateMemCpy(memptr, memptr->getRetAlign(), klass.instTemplatePtr, klass.instTemplatePtr->getAlign(), builder.getInt64(instSize));
    // Restore flag
    builder.CreateStore(objInfo, memptr);

    llvm::Value* inst = builder.CreateBitCast(memptr, _tyhelp.internal_obj_ty("ObjPtr"));
    inst = builder.CreateCall(safeGetFunc("encodeObj"), {inst, builder.getInt64(+object::ObjType::INSTANCE)});
    // If there is a constructor declared in this class, call it, else just return instance

    if(!klass.ty->methods.contains(name)) return inst;

    std::pair<types::tyPtr, uInt64> fnty = klass.ty->methods[name];
    auto fn  = _tyhelp.ty_to_fn(fnty.first);
    auto ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(_tyhelp.internal_obj_ty("ObjClosureAligned"), klass.methodArrPtr,
                                                             builder.getInt32(fnty.second));
    // Need to tag the method
    ptr = _ct.constObjToVal(ptr, +object::ObjType::CLOSURE);
    std::vector<llvm::Value*> args = { ptr, inst };
    for(const auto& arg : expr->args) args.push_back(arg->codegen(this));
    return builder.CreateCall(fn, args);
    // TODO: error if there are arguments passed when the constructor doesn't exist

}

//TODO: create a wrapper function that takes a single argument(param) in an array and then calls the function with them
llvm::Value* Compiler::visitSpawnStmt(CFG::SpawnStmt* stmt){
    // func being nonnull means we got optimized function
    if(stmt->call->type == CFG::NodeType::CALL){
        std::shared_ptr<CFG::CallExpr> expr = std::reinterpret_pointer_cast<CFG::CallExpr>(stmt->call);
        llvm::Value* closureVal = expr->callee->codegen(this);
        // Inserts tagged closure since that is what functions expect
        std::vector args = {closureVal};
        for(const auto& arg : expr->args) args.push_back(arg->codegen(this));
        debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);

        llvm::FunctionCallee callee;
        if(!exprIsComplexType(expr->callee, types::TypeFlag::FUNCTION))
            callee = _rt.createUnoptFunCall(closureVal, expr->args.size());
        else{
            // Safe cast because of opt
            auto funcType = std::reinterpret_pointer_cast<types::FunctionType>(expr->callee->exprType);
            // TODO: this should be done in a separate pass
            if(funcType->argCount != expr->args.size()){
                errHandler.reportError(fmt::format("Function expects {} parameters, got {} arguments.",
                    funcType->argCount, expr->args.size()),expr->dbgInfo.paren1);
                throw CompilerError("Incorrect number of arguments passed");
            }
            callee = _tyhelp.ty_to_fn(funcType);
        }
        setupThreadCreation(callee, args);
    } else { // Must be NodeType::INVOKE
        std::shared_ptr<CFG::InvokeExpr> expr = std::reinterpret_pointer_cast<CFG::InvokeExpr>(stmt->call);
        auto encodedInst = expr->inst->codegen(this);

        vector<llvm::Value*> args;
        for(const auto& arg : expr->args) args.push_back(arg->codegen(this));
        debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);
        auto exec = [&](llvm::FunctionCallee callee, std::span<llvm::Value*> sp) {
            args.insert(args.begin(), sp.begin(), sp.end());
            setupThreadCreation(callee, args);
            args.erase(args.begin(), args.begin() + sp.size());
            return builder.getInt64(0);
        };

        if(exprIsComplexType(expr->inst, types::TypeFlag::INSTANCE)) {
            auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(expr->inst->exprType)->klass->name];
            _inst_builder.optimizeInvoke({ encodedInst, expr->field, *klass.ty }, klass.methodArrPtr, expr->args.size(), exec);
            return nullptr;
        }
        return _inst_builder.unoptimizedInvoke({ encodedInst, expr->field }, expr->args.size(), exec);
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitCreateClosureExpr(CFG::CreateClosureExpr* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    inProgressFuncs.emplace(startFuncDef(expr->fn->name, expr->fn->fnTy, expr->dbgInfo.keyword));

    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    llvm::Value* cl = builder.CreateCall(safeGetFunc("decodeClosure"), inProgressFuncs.top().fn->getArg(0),
        "closure");
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        llvm::Value* freevarPtr = builder.CreateGEP(_tyhelp.internal_obj_ty("ObjClosure"), cl, builder.getInt32(1));
        freevarPtr = builder.CreateInBoundsGEP(_tyhelp.getESLValType(), freevarPtr, builder.getInt32(i));
        llvm::Value* tmp = builder.CreateLoad(_tyhelp.getESLValType(), freevarPtr);
        auto var = builder.CreateAlloca(_tyhelp.getESLValType(), builder.getInt32(1));
        builder.CreateStore(tmp, var);
        variables.insert_or_assign(freevar.second->uuid, var);
    }

    declareFuncArgs(expr->fn->args);

    for(auto stmt : expr->fn->block.stmts)
        stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto lambda = inProgressFuncs.top().fn;
    inProgressFuncs.pop();
    debugEmitter.popScope(builder, expr->dbgInfo.keyword);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());

    // Every function is converted to a closure(if even it has 0 freevars for ease of use when calling)
    // If expr->freevars.size() is 0 then no array for freevars is allocated
    vector<llvm::Value*> closureConstructorArgs = {
        llvm::ConstantExpr::getBitCast(lambda, builder.getPtrTy()), builder.getInt8(expr->fn->args.size()),
        _ct.createConstStr(expr->fn->name), builder.getInt32(expr->freevars.size())
    };

    // Freevars are gathered after switching to the enclosing function
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        closureConstructorArgs.push_back(builder.CreateLoad(_tyhelp.getESLValType(), variables.at(freevar.first->uuid)));
        // Removes the freevars uuid from the variable pool since compilation for this function is done and this won't be used again
        variables.erase(freevar.second->uuid);
    }
    // Create the closure and put the freevars in it, createClosure is a vararg function
    return builder.CreateCall(safeGetFunc("createClosure"), closureConstructorArgs);
}

llvm::Value* Compiler::visitFuncDecl(CFG::FuncDecl* stmt) {
    inProgressFuncs.emplace(startFuncDef(stmt->fn->name, stmt->fn->fnTy, stmt->dbgInfo.name));

    declareFuncArgs(stmt->fn->args);

    for(auto s : stmt->fn->block.stmts)
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = inProgressFuncs.top().fn;
    inProgressFuncs.pop();
    debugEmitter.popScope(builder, stmt->dbgInfo.keyword);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    // Every function is converted to a closure(even if it has 0 freevars) for ease of use when calling
    // Since this is a global function declaration number of freevars is always going to be 0
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(fn, builder.getPtrTy());
    auto arity = builder.getInt8(stmt->fn->args.size());
    auto name = _ct.createConstStr(stmt->fn->name);
    auto freeVarCnt = builder.getInt8(0);

    llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
        { _ct.createConstObjHeader(+object::ObjType::CLOSURE), arity, freeVarCnt, typeErasedFn, name });

    llvm::Constant* fnLoc = _ct.storeConstObj(fnC);
    auto gv = (llvm::dyn_cast<llvm::GlobalVariable>(variables.at(stmt->globalVarUuid)));
    gv->setInitializer(_ct.constObjToVal(fnLoc, +object::ObjType::CLOSURE));

    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitExprStmt(CFG::ExprStmt* stmt) {
    stmt->expr->codegen(this);
    return nullptr;
}

llvm::Value* Compiler::visitReturnStmt(CFG::ReturnStmt* stmt) {
    debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);
    builder.CreateRet(stmt->expr->codegen(this));
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitUncondJump(CFG::UncondJump* stmt) {
    debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);
    switch(stmt->jmpType){
        case CFG::JumpType::BREAK: builder.CreateBr(breakJumpDest.top()); break;
        case CFG::JumpType::CONTINUE: builder.CreateBr(continueJumpDest.top()); break;
        case CFG::JumpType::ADVANCE: builder.CreateBr(advanceJumpDest.top()); break;
        default: __builtin_unreachable();
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitIfStmt(CFG::IfStmt* stmt) {
    auto condtmp = stmt->cond->codegen(this);
    llvm::Value* cond;
    if(exprIsType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(safeGetFunc("decodeBool"), condtmp);
    }else cond = builder.CreateCall(safeGetFunc("isTruthy"), condtmp);

    create_if(cond,
        [&]() {
            codegenBlock(stmt->thenBlock);
        },
        [&]() {
            codegenBlock(stmt->elseBlock);
        }
    );
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitWhileStmt(CFG::WhileStmt* stmt) {
    bool canOptimize = stmt->cond ? exprIsType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL)) : true;
    auto decodeFn = canOptimize ? safeGetFunc("decodeBool") : safeGetFunc("isTruthy");

    llvm::Function* F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* headerBB = llvm::BasicBlock::Create(*ctx, "while.header", F);
    llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*ctx, "while.loop");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "while.merge");

    continueJumpDest.push(headerBB);
    breakJumpDest.push(mergeBB);
    builder.CreateBr(headerBB);
    builder.SetInsertPoint(headerBB);
    // stmt->cond might be null if the for statement that got transformed into while stmt didn't have a condition
    llvm::Value* cond = builder.getInt1(true);
    if(stmt->cond) cond = builder.CreateCall(decodeFn, stmt->cond->codegen(this));
    builder.CreateCondBr(cond, loopBB, mergeBB);

    // Loop body
    F->insert(F->end(), loopBB);
    builder.SetInsertPoint(loopBB);
    codegenBlock(stmt->loopBody);
    // As the name suggests, this is eval-ed after the main body of the loop is ran
    if(stmt->afterLoopExpr) stmt->afterLoopExpr->codegen(this);
    // Only jump to condition if the main body doesn't terminate already(eg. an unconditional break stmt at the end of loop)
    if(!stmt->loopBody.terminates){
        builder.CreateCall(safeGetFunc("safepoint_poll"));
        builder.CreateBr(headerBB);
    }

    F->insert(F->end(), mergeBB);
    // Sets the builder up to emit code after the while stmt
    builder.SetInsertPoint(mergeBB);
    // Pop destinations so that any break/continue for an outer loop works correctly
    continueJumpDest.pop();
    breakJumpDest.pop();

    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitSwitchStmt(CFG::SwitchStmt* stmt) {
    // Switch directly compares Int64 contents to determine equality, this should work for most ints represented as doubles(i hope)
    auto compVal = _tyhelp.ESLValTo(stmt->cond->codegen(this), builder.getInt64Ty());
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // Have to create the basic blocks before codegening because of advance stmt
    vector<llvm::BasicBlock *> blocks = createNCaseBlocks(stmt->cases.size());
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "switch.merge");
    llvm::BasicBlock* defaultDest = stmt->defaultCaseBlockNum == -1 ? mergeBB : blocks[stmt->defaultCaseBlockNum];

    // If this switch doesn't contain strings as case constants it can be optimized
    if(!stmt->containsStrings){
        auto inst = builder.CreateSwitch(compVal, defaultDest);
        for(auto constant : stmt->constants)
            inst->addCase(createSwitchConstantInt(constant.first), blocks[constant.second]);
    }
    else {
        llvm::Value *BBIdx = createSeqCmp(compVal, stmt->constants);
        auto inst = builder.CreateSwitch(BBIdx, defaultDest);
        for (int i = 0; i < blocks.size(); i++)
            inst->addCase(builder.getInt32(i), blocks[i]);
    }

    // Sets the destination blocks for break stmts
    breakJumpDest.push(mergeBB);

    for(int i = 0; i < stmt->cases.size(); i++){
        // First push the next block(or merge block) as the advance jump destination
        advanceJumpDest.push(i+1 < blocks.size() ? blocks[i+1] : mergeBB);

        F->insert(F->end(), blocks[i]);
        builder.SetInsertPoint(blocks[i]);

        debugEmitter.emitNewLocation(builder, stmt->dbgInfo.cases[i]);
        codegenBlock(stmt->cases[i]);
        if(!stmt->cases[i].terminates)
            builder.CreateBr(mergeBB);
        advanceJumpDest.pop();
    }
    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    breakJumpDest.pop();
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitClassDecl(CFG::ClassDecl* stmt) {
    auto name = _ct.createConstStr(stmt->fullName);
    auto fieldsLen = builder.getInt16(stmt->classType->fields.size());
    auto methodsLen = builder.getInt16(stmt->classType->methods.size());
    // Generates functions to call when type of instance is not known at compile time to get the index into the field/method array
    auto fieldsFunc = createStrToIdxFunc(stmt->classType, false);
    auto methodsFunc = createStrToIdxFunc(stmt->classType, true);
    // Result of a dfs done on the class graph
    auto subClassIdxStart = builder.getInt32(_tyhelp.class_hierarchy(stmt->fullName).first);
    auto subClassIdxEnd = builder.getInt32(_tyhelp.class_hierarchy(stmt->fullName).second);

    vector<llvm::Constant*> methods(stmt->classType->methods.size());
    // Copy all methods of parent class, these can then be (possibly) overriden
    if(classes.contains(stmt->parentClassName)){
        int i = 0;
        for(llvm::Constant* parentMethod : classes[stmt->parentClassName].methodArr){
            methods[i++] = parentMethod;
        }
    }
    for(auto& [mName, method] : stmt->methods){
        llvm::Function* methodFn = declareFunction(method.first.code->fnTy);
        methodFn->setName(stmt->fullName + mName);
        // Creates an ObjClosure associated with this method
        methods[method.second] = _ct.createMethodObj(method.first.code->name, method.first.code->args.size(), methodFn);
    }
    llvm::Constant* methodArr = llvm::ConstantArray::get(llvm::ArrayType::get(_tyhelp.internal_obj_ty("ObjClosureAligned"),
                                                                   methods.size()), methods);

    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClass"), {
            _ct.createConstObjHeader(+object::ObjType::CLASS), methodsLen, fieldsLen, subClassIdxStart, subClassIdxEnd, name, methodsFunc,
            fieldsFunc, builder.getInt64(0)});
    obj = llvm::ConstantStruct::getAnon({obj, methodArr});
    llvm::GlobalVariable* klass = new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                                           llvm::GlobalVariable::PrivateLinkage, obj);
    klass->setAlignment(llvm::Align(16));
    // ArrayRef can't be taken out into its own thing because it causes a segfault for some reason
    llvm::Constant* methodArrPtr = llvm::ConstantExpr::getInBoundsGetElementPtr(obj->getType(), klass,
        llvm::ArrayRef<llvm::Constant*>({builder.getInt32(0), builder.getInt32(1)}));
    // Associates a full class name with the class object and instance template
    classes[stmt->fullName] = Class(klass, _inst_builder.createInstanceTemplate(klass, stmt->fields.size()),
        stmt->classType, methods, methodArrPtr);

    for(auto& [mName, method] : stmt->methods)
        codegenMethod(stmt->fullName, method.first, subClassIdxStart, subClassIdxEnd);
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitInstGet(CFG::InstGet* expr) {
    auto inst = expr->instance->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.field);
    if(exprIsComplexType(expr->instance, types::TypeFlag::INSTANCE)) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(expr->instance->exprType)->klass->name];
        return _inst_builder.optimizeInstGet({ inst, expr->field, *klass.ty }, klass.methodArrPtr);
    }
    return _inst_builder.instGetUnoptimized({ inst, expr->field });
}
llvm::Value* Compiler::visitInstSet(CFG::InstSet* expr) {
    auto inst = expr->instance->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.field);
    llvm::Value* fieldPtr = nullptr;
    if(exprIsComplexType(expr->instance, types::TypeFlag::INSTANCE)) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(expr->instance->exprType)->klass->name];
        fieldPtr = _inst_builder.getOptInstFieldPtr({ inst, expr->field, *klass.ty });
    }else
        fieldPtr = _inst_builder.getUnoptInstFieldPtr({ inst, expr->field });
    auto val = expr->toStore->codegen(this);

    if(expr->operationType == CFG::SetType::SET){
        builder.CreateStore(val, fieldPtr);
        return val;
    }
    if(expr->operationType == CFG::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateLoad(_tyhelp.getESLValType(), fieldPtr);
        val = codegenBinaryAdd(storedVal, val, expr->dbgInfo.op);
        builder.CreateStore(val, fieldPtr);
        return val;
    }
    auto storedField = builder.CreateLoad(_tyhelp.getESLValType(), fieldPtr);
    if(!exprIsType(expr->toStore, types::getBasicType(types::TypeFlag::NUMBER))) {
        string err = fmt::format("Operator '{}' expects numbers, field '{}' is '{}', rhs is '{}'.",
                                 expr->dbgInfo.op.getLexeme(), expr->field, "{}", "{}");
        _rt.createTypeCheckBinary(err, storedField, val, TypeHelper::getNumberTypeMasks());
    }else{
        string err = fmt::format("Operator '{}' expects numbers, field '{}' is '{}'.", expr->dbgInfo.op.getLexeme(), expr->field, "{}");
        _rt.createTypeCheckUnary(err, storedField, TypeHelper::getNumberTypeMasks());
    }

    val = _tyhelp.CastToESLVal(decoupleSetOperation(storedField, val, expr->operationType, expr->dbgInfo.op));
    builder.CreateStore(val, fieldPtr);
    return val;
}

llvm::Value* Compiler::visitScopeBlock(CFG::ScopeEdge* stmt) {
    // Erases local variables which will no longer be used, done to keep memory usage at least somewhat reasonable
    for(auto uuid : stmt->toPop)
        variables.erase(uuid);
    // TODO: very hacky, but macroExpander creates synthetic scopes that shouldn't be a part of debug info
    if(stmt->location.isSynthetic) return nullptr;
    if(stmt->edgeType == CFG::ScopeEdgeType::START) debugEmitter.addScope(builder, stmt->location);
    else debugEmitter.popScope(builder, stmt->location);
    return nullptr; // Stmts return nullptr on codegen
}

void Compiler::setupModule(const llvm::DataLayout& DL){
    curModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                             llvm::DEBUG_METADATA_VERSION);
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    curModule->setDataLayout(DL);
    curModule->setTargetTriple(targetTriple);
}

void Compiler::optimizeModule(llvm::Module& module){
    // Create the analysis managers.
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    // Create the pass manager.
    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    MPM.run(module, MAM);
    curModule->print(llvm::errs(), nullptr);
}

llvm::Function* Compiler::declareFunction(const std::shared_ptr<types::FunctionType> fnType){
    // First argument is always the thread data ptr
    vector<llvm::Type*> params {};
    // Second argument is always the closure structure
    for(int i = 0; i < fnType->argCount + 1; i++) params.push_back(_tyhelp.getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(_tyhelp.getESLValType(), params, false);
    auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, "thunk", curModule.get());
    _tyhelp.set_fn_attrs(tmp);
    tmp->setGC("statepoint-example");
    // Creates a connection between function types and functions
    _tyhelp.add_fn_mapping(fnType, tmp);
    return tmp;
}

void Compiler::createMainEntrypoint(string entrypointName){
    // Create internal entrypoint function, takes in the thread data ptr
    llvm::FunctionType* entryFT = llvm::FunctionType::get(builder.getVoidTy(),false);
    auto entryFn = llvm::Function::Create(entryFT, llvm::Function::PrivateLinkage, "entrypoint", curModule.get());
    _tyhelp.set_fn_attrs(entryFn);
    entryFn->setGC("statepoint-example");
    entryFn->addFnAttr(llvm::Attribute::AttrKind::NoInline);
    debugEmitter.addMainFunc(entryFn);
    // Create the runtime entrypoint that calls the internal entrypoint
    llvm::FunctionType* FT = llvm::FunctionType::get(builder.getInt32Ty(),{builder.getInt32Ty(), builder.getPtrTy()}, false);
    auto tmpfn = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, entrypointName, curModule.get());
    _tyhelp.set_fn_attrs(tmpfn);
    tmpfn->setGC("statepoint-example");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmpfn);
    builder.SetInsertPoint(BB);

    builder.CreateCall(entryFn);
    builder.CreateCall(safeGetFunc("exit"), builder.getInt32(0));
    builder.CreateRet(builder.getInt32(0));
    llvm::verifyFunction(*tmpfn);

    // Setup to start writing to internal entrypoint
    entryFn->setGC("statepoint-example");
    BB = llvm::BasicBlock::Create(*ctx, "entry", entryFn);
    builder.SetInsertPoint(BB);
    inProgressFuncs.emplace(entryFn);
}

void Compiler::pastAllocas(std::function<void(llvm::IRBuilder<>&)> func) {
    llvm::IRBuilder<> tempBuilder(*ctx);
    tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
    func(tempBuilder);
}

#pragma region helpers

// Compile time type checking
bool Compiler::exprIsType(const typedExprPtr expr, const types::tyPtr ty){
    return types::types_equal(expr->exprType, ty);
}
bool Compiler::exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty) {
    return exprIsType(expr1, ty) && exprIsType(expr2, ty);
}
bool Compiler::exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag){
    return expr->exprType->type == flag;
}


// Runtime type checking


void Compiler::create_if(llvm::Value* cond, std::function<void()> then, std::function<void()> _else) {
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx, "then", F);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx, "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");
    builder.CreateCondBr(cond, thenBB, elseBB);
    builder.SetInsertPoint(thenBB);
    then();
    if (!builder.GetInsertBlock()->back().isTerminator()) builder.CreateBr(mergeBB);

    F->insert(F->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    _else();
    if (builder.GetInsertBlock()->empty() || !builder.GetInsertBlock()->back().isTerminator()) builder.CreateBr(mergeBB);
    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
}

// Codegen functions
llvm::Value* Compiler::codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, Token op){
    debugEmitter.emitNewLocation(builder, op);
    // If both are a number go to addNum, if not try adding as string
    auto phi = llvm::PHINode::Create(_tyhelp.getESLValType(), 2);

    // Call isNum on both values and && the results
    auto isnum = safeGetFunc("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);
    create_if(builder.CreateAnd(c1, c2),
        [&]() {
            auto castlhs = _tyhelp.ESLValTo(lhs, builder.getDoubleTy());
            auto castrhs = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());
            auto numAddRes = _tyhelp.CastToESLVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
            phi->addIncoming(numAddRes, builder.GetInsertBlock());
        },
        [&]() {
            _rt.createTypeCheckBinary("Addition expects numbers or strings, got '{}' and '{}'.", lhs, rhs,
                          TypeHelper::getObjectTypeMasks(object::ObjType::STRING));
            auto stringAddRes = builder.CreateCall(safeGetFunc("strAdd"), {lhs, rhs,});
            phi->addIncoming(stringAddRes, builder.GetInsertBlock());
        }
    );
    return builder.Insert(phi);
}
llvm::Value* Compiler::codegenLogicOps(const typedExprPtr expr1, const typedExprPtr expr2, const CFG::ComparisonOp op){
    bool canOptimize = exprIsType(expr1, expr2, types::getBasicType(types::TypeFlag::BOOL));
    auto castToBool = canOptimize ? safeGetFunc("decodeBool") : safeGetFunc("isTruthy");
    auto phi = llvm::PHINode::Create(_tyhelp.getESLValType(), 2);

    llvm::Value* lhs = builder.CreateCall(castToBool, expr1->codegen(this));
    create_if(op == CFG::ComparisonOp::OR ? builder.CreateNot(lhs) : lhs,
        [&]() {
            llvm::Value* rhs = builder.CreateCall(castToBool, expr2->codegen(this));
            // For both operators, if control flow is coming from evalRhsBB rhs becomes the value of the entire expression
            phi->addIncoming(rhs, builder.GetInsertBlock());
        },
        [&]() {
            phi->addIncoming(builder.getInt1(op == CFG::ComparisonOp::OR ? true : false), builder.GetInsertBlock());
        }
    );

    // If we're coming from the originalBB and the operator is 'or' it means that lhs is true, and thus the entire expression is true
    // For 'and' it's the opposite, if lhs is false, then the entire expression is false
    builder.Insert(phi);
    return builder.CreateCall(safeGetFunc("encodeBool"), phi);
}
llvm::Value* Compiler::codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg){
    llvm::Value* lhs = expr1->codegen(this);
    llvm::Value* rhs = expr2->codegen(this);
    // Numbers have to be compared using fcmp for rounding reasons,
    // other value types are compared as 64 bit ints since every object is unique(strings are interned)

    // Optimizations if types are known, no need to do runtime checks
    const auto numTy = types::getBasicType(types::TypeFlag::NUMBER);
    const auto stringTy = types::getBasicType(types::TypeFlag::STRING);
    const auto anyTy = types::getBasicType(types::TypeFlag::ANY);
    if(exprIsType(expr1, expr2, numTy)){
        // fcmp when both values are numbers
        lhs = _tyhelp.ESLValTo(lhs, builder.getDoubleTy());
        rhs = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());

        auto val = neg ? builder.CreateFCmpONE(lhs, rhs, "fcmp.one") : builder.CreateFCmpOEQ(lhs, rhs, "fcmp.oeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }
    if(exprIsType(expr1, expr2, stringTy))
        return builder.CreateCall(safeGetFunc("strCmp"), {lhs, rhs});

    if(!exprIsType(expr1, numTy) && !exprIsType(expr2, numTy) &&
            !exprIsType(expr1, anyTy) && !exprIsType(expr2, anyTy) &&
            !exprIsType(expr1, stringTy) && !exprIsType(expr2, stringTy)){
        auto val = neg ? builder.CreateICmpNE(lhs, rhs, "icmpne") : builder.CreateICmpEQ(lhs, rhs, "icmp.eq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }

    // If both values are numbers, use the floating comparison, if there is type mismatch/values are of some other type use icmp
    auto isnum = safeGetFunc("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);

    llvm::Value* icmptmp = builder.CreateICmpEQ(lhs, rhs, "icmp.tmp");
    llvm::Value* fcmptmp = builder.CreateFCmpOEQ(_tyhelp.ESLValTo(lhs, builder.getDoubleTy()),
        _tyhelp.ESLValTo(rhs, builder.getDoubleTy()), "fcmp.tmp");
    if(neg){
        icmptmp = builder.CreateNot(icmptmp, "icmp.neg.tmp");
        fcmptmp = builder.CreateNot(fcmptmp, "fcmp.neg.tmp");
    }
    // To reduce branching on a common operation, select instruction is used
    auto sel = builder.CreateSelect(builder.CreateAnd(c1, c2), fcmptmp, icmptmp);
    llvm::Value* cmpRes = builder.CreateCall(safeGetFunc("encodeBool"), sel);

    // Strings need to be compared using strcmp because some of them are not interned
    auto phi = llvm::PHINode::Create(_tyhelp.getESLValType(), 2);
    auto sc1 = builder.CreateCall(safeGetFunc("isObjType"), {lhs, builder.getInt8(+object::ObjType::STRING)});
    auto sc2 = builder.CreateCall(safeGetFunc("isObjType"), {rhs, builder.getInt8(+object::ObjType::STRING)});

    create_if(builder.CreateAnd(sc1, sc2),
        [&]() {
            llvm::Value* strCmpRes = builder.CreateCall(safeGetFunc("strCmp"), {lhs, rhs});
            phi->addIncoming(strCmpRes, builder.GetInsertBlock());
        },
        [&]() {
            phi->addIncoming(cmpRes, builder.GetInsertBlock());
        }
    );
    return builder.Insert(phi);
}
llvm::Value* Compiler::codegenNeg(llvm::Value* rhs, const types::tyPtr type, CFG::UnaryOp op, Token dbg){
    // If rhs is known to be a number, no need for the type check
    if(type != types::getBasicType(types::TypeFlag::NUMBER)){
        string err = fmt::format("Operator '{}' expects a number, got '{}'.", dbg.getLexeme(), "{}");
        _rt.createTypeCheckUnary(err, rhs, TypeHelper::getNumberTypeMasks());
    }
    // For binary negation, the casting is as follows Value -> double -> int64 -> double -> Value
    if(op == CFG::UnaryOp::BIN_NEG){
        // Cast value to double, then convert to signed 64bit integer and negate
        auto tmp = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());
        auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"bin.neg.tmp");
        // Cast back to double and then to 64bit int
        auto castToDouble = builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx));
        return _tyhelp.CastToESLVal(castToDouble);
    }
    auto tmp = _tyhelp.ESLValTo(rhs, builder.getDoubleTy());
    return _tyhelp.CastToESLVal(builder.CreateFNeg(tmp, "fneg.tmp"));
}
void Compiler::codegenBlock(const CFG::Block& block){
    for(auto stmt : block.stmts){
        stmt->codegen(this);
    }
}
llvm::Value * Compiler::codegenIncrement(const CFG::UnaryOp op, const typedExprPtr expr, const Token dbg) {
    // No array/hashmap field access because it's too complicated
    if(expr->type == CFG::NodeType::VAR_READ)
        return codegenVarIncrement(op, std::reinterpret_pointer_cast<CFG::VarRead>(expr), dbg);

    if(expr->type == CFG::NodeType::INST_GET)
        return codegenInstIncrement(op, std::reinterpret_pointer_cast<CFG::InstGet>(expr), dbg);

    // TODO: error
    errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    return nullptr;
}
// Reuses var read and var store
llvm::Value * Compiler::codegenVarIncrement(const CFG::UnaryOp op, const std::shared_ptr<CFG::VarRead> expr, Token dbg) {
    llvm::Value* val = codegenVarRead(expr->varPtr);
    debugEmitter.emitNewLocation(builder, dbg);
    // Right now we can only increment numbers, maybe change this when adding iterators?
    if(!exprIsType(expr, types::getBasicType(types::TypeFlag::NUMBER))){
        string err = fmt::format("Operator '{}' expects a number, but got '{}'.", dbg.getLexeme(), "{}");
        _rt.createTypeCheckUnary(err, val, TypeHelper::getNumberTypeMasks());
    }
    llvm::Value* res = _tyhelp.ESLValTo(val, builder.getDoubleTy());

    if(op == CFG::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    res = _tyhelp.CastToESLVal(res);
    codegenVarStore(expr->varPtr, res);
    return val;
}
llvm::Value * Compiler::codegenInstIncrement(const CFG::UnaryOp op, const std::shared_ptr<CFG::InstGet> expr, Token dbg) {
    auto inst = expr->instance->codegen(this);

    llvm::Value* fieldPtr = nullptr;
    // If type of instance if known optimize getting pointer to field
    if(exprIsComplexType(expr->instance, types::TypeFlag::INSTANCE)) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(expr->instance->exprType)->klass->name];
        fieldPtr = _inst_builder.getOptInstFieldPtr({ inst, expr->field, *klass.ty});
    }else{
        fieldPtr = _inst_builder.getUnoptInstFieldPtr({ inst, expr->field });
    }
    llvm::Value* storedField = builder.CreateLoad(_tyhelp.getESLValType(), fieldPtr);
    // Set debug to operator after getting field for more correct error messages
    debugEmitter.emitNewLocation(builder, dbg);
    string err = fmt::format("Operator '{}' expects a number, field '{}' is '{}'.", dbg.getLexeme(), expr->field, "{}");
    _rt.createTypeCheckUnary(err, storedField, TypeHelper::getNumberTypeMasks());

    llvm::Value* res = _tyhelp.ESLValTo(storedField, builder.getDoubleTy());
    if(op == CFG::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));

    res = _tyhelp.CastToESLVal(res);
    builder.CreateStore(res, fieldPtr);
    return storedField;
}

// Function codegen helpers
llvm::Function* Compiler::startFuncDef(const string &name, const std::shared_ptr<types::FunctionType> fnTy, Token& loc){
    auto fn = declareFunction(fnTy);
    fn->setName(name);
    debugEmitter.addNewFunc(builder, fn, *fnTy, loc);

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    builder.CreateCall(safeGetFunc("safepoint_poll"));
    return fn;
}
void Compiler::declareFuncArgs(const vector<std::shared_ptr<CFG::VarDecl>>& args){
    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    // First argument is ALWAYS the thread data ptr, and second is obj closure ptr
    int argIndex = 1;
    for (auto var : args) {
        llvm::Value* varPtr;
        // Don't need to use temp builder when creating alloca since this happens in the first basicblock of the function
        // Since closures capture by value we can make everything a stack variable
        varPtr = builder.CreateAlloca(_tyhelp.getESLValType(), nullptr, var->dbgInfo.varName.getLexeme());
        builder.CreateStore(inProgressFuncs.top().fn->getArg(argIndex++), varPtr);
        debugEmitter.addLocalVarDecl(builder, varPtr, var->dbgInfo.varName, true, argIndex);
        // Insert the argument into the pool of variables
        variables.insert_or_assign(var->uuid, varPtr);
    }
}

llvm::Value* Compiler::decoupleSetOperation(llvm::Value* storedVal, llvm::Value* newVal, CFG::SetType opTy, Token dbg){
    auto num1 = _tyhelp.ESLValTo(storedVal, builder.getDoubleTy());
    auto num2 = _tyhelp.ESLValTo(newVal, builder.getDoubleTy());
    switch(opTy){
        case CFG::SetType::ADD_SET:
            return builder.CreateFAdd(num1, num2);
        case CFG::SetType::SUB_SET:
            return builder.CreateFSub(num1, num2);
        case CFG::SetType::MUL_SET:
            return builder.CreateFMul(num1, num2);
        case CFG::SetType::DIV_SET:
            return builder.CreateFDiv(num1, num2);
        case CFG::SetType::REM_SET:
            return builder.CreateFRem(num1, num2);
        case CFG::SetType::AND_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateAnd(num1, num2), builder.getDoubleTy());
        case CFG::SetType::OR_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateOr(num1, num2), builder.getDoubleTy());
        case CFG::SetType::XOR_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateXor(num1, num2), builder.getDoubleTy());
        default: __builtin_unreachable();
    }
    // This will never be hit
}

llvm::Value* Compiler::getArrElement(llvm::Value* arr, llvm::Value* field, bool opt, Token dbg){
    if(!opt) _rt.createTypeCheckUnary("Array accessor must be a number, got '{}'.", field, TypeHelper::getNumberTypeMasks());
    // Check the index first because we need the untagged version of the index for error reporting
    _rt.createArrBoundsCheck("Index {} outside of array range, array size is {}.", arr, field);
    field = _tyhelp.ESLValTo(field, builder.getDoubleTy());
    field = builder.CreateFPToSI(field, builder.getInt64Ty());


    arr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(_tyhelp.internal_obj_ty("ObjArray"), arr, 0, 3);
    arr = builder.CreateLoad(_tyhelp.internal_obj_ty("ObjArrayStoragePtr"), storagePtr, "storage.ptr");
    arr = builder.CreateConstInBoundsGEP1_32(_tyhelp.internal_obj_ty("ObjArrayStorage"), arr, 1, "data.ptr");
    auto ptr = builder.CreateGEP(_tyhelp.getESLValType(), arr, field, "item.ptr");
    return builder.CreateLoad(_tyhelp.getESLValType(), ptr, "arr.elem");
}

llvm::Value* Compiler::getMapElement(llvm::Value* map, llvm::Value* field, bool opt, Token dbg){
    if(!opt) _rt.createTypeCheckUnary("Map accessor must be a string, got '{}'.", field,
                                  TypeHelper::getObjectTypeMasks(object::ObjType::STRING));

    map = builder.CreateCall(safeGetFunc("decodeObj"), map);
    field = builder.CreateCall(safeGetFunc("decodeObj"), field);
    return builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field}, "map.elem");
}

llvm::Value* Compiler::setArrElement(llvm::Value* arr, llvm::Value* index, llvm::Value* val, bool optIdx, bool optVal,
                                     CFG::SetType opTy, Token dbg){
    if(!optIdx) _rt.createTypeCheckUnary("Array accessor must be a number, got '{}'.", index, TypeHelper::getNumberTypeMasks());

    _rt.createArrBoundsCheck("Index {} outside of array range, array size is {}.", arr, index);
    index = _tyhelp.ESLValTo(index, builder.getDoubleTy());
    index = builder.CreateFPToUI(index, builder.getInt64Ty());
    arr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    builder.CreateCall(safeGetFunc("arrWriteBarrier"), {arr, val});
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(_tyhelp.internal_obj_ty("ObjArray"), arr, 0, 3);
    arr = builder.CreateLoad(_tyhelp.internal_obj_ty("ObjArrayStoragePtr"), storagePtr, "storage.ptr");
    arr = builder.CreateConstInBoundsGEP1_32(_tyhelp.internal_obj_ty("ObjArrayStorage"), arr, 1, "data.ptr");
    llvm::Value* ptr = builder.CreateGEP(_tyhelp.getESLValType(), arr, index);

    if(opTy == CFG::SetType::SET){
        builder.CreateStore(val, ptr);
        return val;
    }
    if(opTy == CFG::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateLoad(_tyhelp.getESLValType(), ptr);
        val = codegenBinaryAdd(storedVal, val, dbg);
        builder.CreateStore(val, ptr);
        return val;
    }
    auto storedVal = builder.CreateLoad(_tyhelp.getESLValType(), ptr);
    if(!optVal) {
        _rt.createTypeCheckBinary("Operator expects numbers, array element is '{}', rhs is '{}'.", storedVal, val, TypeHelper::getNumberTypeMasks());
    }else{
        _rt.createTypeCheckUnary("Operator expects numbers, array element is '{}'.", storedVal, TypeHelper::getNumberTypeMasks());
    }

    val = _tyhelp.CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
    builder.CreateStore(val, ptr);
    return val;
}
llvm::Value* Compiler::setMapElement(llvm::Value* map, llvm::Value* field, llvm::Value* val, bool optIdx, bool optVal,
                                     CFG::SetType opTy, Token dbg){
    if(!optIdx) _rt.createTypeCheckUnary("Map accessor must be a string, got '{}'.", field,
                                  TypeHelper::getObjectTypeMasks(object::ObjType::STRING));

    map = builder.CreateCall(safeGetFunc("decodeObj"), map);
    field = builder.CreateCall(safeGetFunc("decodeObj"), field);

    if(opTy == CFG::SetType::SET){
        builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
        return val;
    }
    if(opTy == CFG::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
        val = codegenBinaryAdd(storedVal, val, dbg);
        builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
        return val;
    }
    auto storedVal = builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
    if(!optVal)
        _rt.createTypeCheckBinary("Operator expects numbers, array element is '{}', rhs is '{}'.", storedVal, val, TypeHelper::getNumberTypeMasks());
    else
        _rt.createTypeCheckUnary("Operator expects numbers, array element is '{}'.", storedVal, TypeHelper::getNumberTypeMasks());

    val = _tyhelp.CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
    builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
    return val;
}

// Switch stmt stuff
// For everything except strings
llvm::ConstantInt* Compiler::createSwitchConstantInt(std::variant<double, bool, void*, string>& constant){
    switch(constant.index()){
        case 0: return builder.getInt64(*reinterpret_cast<uInt64*>(&get<double>(constant)));
        case 1: return builder.getInt64(get<bool>(constant) ? mask_signature_true : mask_signature_false);
        case 2: return builder.getInt64(mask_signature_null);
        default: errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");

    }
    __builtin_unreachable();
}

vector<llvm::BasicBlock*> Compiler::createNCaseBlocks(int n){
    vector<llvm::BasicBlock*> blocks;
    for(int i = 0; i < n; i++){
        auto caseBB = llvm::BasicBlock::Create(*ctx, fmt::format("case.{}", i));
        blocks.emplace_back(caseBB);
    }
    return blocks;
}

llvm::Value* Compiler::createSeqCmp(llvm::Value* compVal, vector<std::pair<std::variant<double, bool, void*, string>, int>>& constants){
    // Starting index is outside the range of blocks so that if switch executes with it control flow goes to default dest
    llvm::Value* BBIdx = builder.getInt32(-1);
    for(auto c : constants){
        llvm::Value* val = _ct.createConstant(c.first);
        // All constants(constant strings are interned) have a unique representation as I64, so ICmpEQ is sufficient
        // compVal is i64
        llvm::Value* cmp = builder.CreateICmpEQ(compVal, val);
        // If comparison is successful BBIdx becomes the index of the block that the switch needs to jump to
        BBIdx = builder.CreateSelect(cmp, builder.getInt32(c.second), BBIdx);
    }
    return BBIdx;
}

// Class helpers
llvm::Function* Compiler::createStrToIdxFunc(std::shared_ptr<types::ClassType> classType, bool isMethod){
    string fnName = classType->name + (isMethod ? ":methodChoose" : "fieldChoose");
    auto& collection = isMethod ? classType->methods : classType->fields;
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), { _tyhelp.getESLValType() }, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, fnName, curModule.get());

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    llvm::Value* idx = builder.getInt32(-1);
    for(auto& p : collection){
        auto toCmp = _ct.createESLString(p.first);
        auto cmp = builder.CreateICmpEQ(fn->getArg(0), toCmp);
        idx = builder.CreateSelect(cmp, builder.getInt32(p.second.second), idx);
    }
    builder.CreateRet(idx);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}
void Compiler::codegenMethod(string classname, CFG::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd){
    llvm::Function* methodFn = _tyhelp.ty_to_fn(method.code->fnTy);
    inProgressFuncs.emplace(methodFn);
    debugEmitter.addNewFunc(builder, methodFn, *method.code->fnTy, method.dbg.name);
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", inProgressFuncs.top().fn);
    builder.SetInsertPoint(BB);
    declareFuncArgs(method.code->args);
    _rt.createInstClassCheck(fmt::format("Expected instance of class '{}', got '{}'.", classname, "{}"),
                         inProgressFuncs.top().fn->getArg(1), subClassIdxStart, subClassIdxEnd);

    for(auto s : method.code->block.stmts)
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it

    // Enclosing function become the active one
    inProgressFuncs.pop();
    debugEmitter.popScope(builder, method.dbg.keyword);

    // Set insertion point to the end of the enclosing method
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
}

// Multithreading
llvm::Function* Compiler::createThreadWrapper(llvm::FunctionType* funcType, int numArgs){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getPtrTy(), { builder.getPtrTy() }, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, "threadWrapper", curModule.get());
    fn->addFnAttr("uwtable", "sync");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    // Loads arguments from memory passed to wrapper, must load all args before threadInit is called since that function frees the memory
    vector<llvm::Value*>args;
    for(int i = 0; i < numArgs; i++){
        llvm::Value* gep = builder.CreateConstInBoundsGEP1_32(_tyhelp.getESLValType(), fn->getArg(0), i + 1);
        args.push_back(builder.CreateLoad(_tyhelp.getESLValType(), gep));
    }
    llvm::Value* funcPtr = builder.CreateLoad(builder.getPtrTy(), fn->getArg(0));

    llvm::Value* frameAddr = builder.CreateIntrinsic(builder.getPtrTy(), llvm::Intrinsic::frameaddress, {builder.getInt32(0)});
    builder.CreateCall(safeGetFunc("threadInit"), { frameAddr, fn->getArg(0) });

    builder.CreateCall(funcType, funcPtr, args);

    builder.CreateCall(safeGetFunc("threadDestruct"));
    builder.CreateRet(llvm::ConstantPointerNull::get(builder.getPtrTy()));

    llvm::verifyFunction(*fn);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}

void Compiler::setupThreadCreation(llvm::FunctionCallee callee, vector<llvm::Value*>& args){
    // alloca content: func ptr + args
    llvm::AllocaInst* alloca = builder.CreateAlloca(builder.getInt64Ty(), builder.getInt32(args.size()+1), "args");
    for(int i = 0; i < args.size(); i++){
        llvm::Value* ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt64Ty(), alloca, i+1, fmt::format("{}.arg", i));
        builder.CreateStore(args[i], ptr);
    }
    // Store pointer to func in first slot of alloca
    builder.CreateStore(callee.getCallee(), alloca);
    llvm::Function* wrapper = createThreadWrapper(callee.getFunctionType(), args.size());
    // C++ function that actually calls pthread_create and also does cleanup when a thread dies
    builder.CreateCall(safeGetFunc("createNewThread"), { wrapper, alloca, builder.getInt64(args.size()+1)});
}

// Misc

llvm::Function* Compiler::safeGetFunc(const string& name){
    auto* fn = curModule->getFunction(name);
    if(!fn){
        std::cerr<<fmt::format("Function {} hasn't been created yet.\n", name);
        exit(64);
    }
    return fn;
}

llvm::Value* Compiler::codegenVarRead(std::shared_ptr<CFG::VarDecl> varPtr){
    switch(varPtr->varType){
        case CFG::VarType::LOCAL:
        case CFG::VarType::FREEVAR: {
            return builder.CreateLoad(_tyhelp.getESLValType(), variables.at(varPtr->uuid), "load.local");
        }
        case CFG::VarType::GLOBAL:
        case CFG::VarType::GLOBAL_FUNC:{
            return builder.CreateLoad(_tyhelp.getESLValType(), variables.at(varPtr->uuid), "load.gvar");
        }
    }
    errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* Compiler::codegenVarStore(std::shared_ptr<CFG::VarDecl> varPtr, llvm::Value* toStore){
    builder.CreateStore(toStore, variables.at(varPtr->uuid));
    return toStore;
}

void Compiler::generateNativeFuncs(fastMap<string, types::tyPtr>& natives){
    auto addNativeFn = [&](string name, int argc, std::shared_ptr<types::FunctionType> type){
        // Every function is declared in generateNativeFuncs, natives need to fix up the linkage
        llvm::Function* func = declareFunction(type);
        func->setLinkage(llvm::Function::ExternalLinkage);
        func->setName(name);

        auto typeErasedFn = llvm::ConstantExpr::getBitCast(func, builder.getPtrTy());
        auto arity = builder.getInt8(argc);
        auto cname = _ct.createConstStr(name);
        auto freeVarCnt = builder.getInt8(0);

        // Create function constant
        llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
            { _ct.createConstObjHeader(+object::ObjType::CLOSURE), arity, freeVarCnt, typeErasedFn, cname});
        // Creates a place in memory for the function and stores it there
        llvm::Constant* fnLoc = _ct.storeConstObj(fnC);
        return _ct.constObjToVal(fnLoc, +object::ObjType::CLOSURE);
    };
    for(auto& [name, type] : natives){
        if (type->type == types::TypeFlag::FUNCTION) {
            auto fnTy = std::reinterpret_pointer_cast<types::FunctionType>(type);
            nativeFunctions[name] = addNativeFn(name, fnTy->argCount, fnTy);
        }

    }
}
#pragma endregion
