#include "compiler.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "LLVMHelperFunctions.h"
#include "../Runtime/Values/valueHelpers.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Transforms/Scalar/PlaceSafepoints.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"

#include <unordered_set>
#include <iostream>

enum class runtimeErrorType : uint8_t{
    WRONG_TYPE,
    WRONG_TYPE_BINARY,
    ARG_CNT,
    INST_FIELD,
    OUT_OF_BOUNDS
};
inline constexpr unsigned operator+ (runtimeErrorType const val) { return static_cast<byte>(val); }

// first el: expected masked value, second el: mask, third val: whether to use NEQ or not, needed to support number type checking
static inline std::tuple<uint64_t, uint64_t, bool> getObjectTypeMasks(object::ObjType type){
    return {mask_signature_obj | +type, mask_signature_obj | mask_payload_type, false};
}
static inline std::tuple<uint64_t, uint64_t, bool> getNumberTypeMasks(){
    return {mask_qnan, mask_qnan, true};
}
static inline std::tuple<uint64_t, uint64_t, bool> getBoolTypeMasks(){
    return {mask_signature_bool, mask_signature_bool, false};
}
static inline std::tuple<uint64_t, uint64_t, bool> getNullTypeMasks(){
    return {mask_signature_null, mask_signature_null, false};
}

using namespace compileCore;

Compiler::Compiler(vector<File*>& _srcFiles, vector<types::tyPtr>& _tyEnv, fastMap<string, std::pair<int, int>>& _classHierarchy,
                   fastMap<string, types::tyVarIdx>& natives, const llvm::DataLayout& DL, errorHandler::ErrorHandler& errHandler)
    : ctx(std::make_unique<llvm::LLVMContext>()), builder(llvm::IRBuilder<>(*ctx)), errHandler(errHandler) {
    sourceFiles = _srcFiles;
    typeEnv = _tyEnv;
    classHierarchy = _classHierarchy;

    setupModule(DL);
    debugEmitter = DebugEmitter(*curModule, *sourceFiles.back(), true);
    llvmHelpers::addHelperFunctionsToModule(curModule, ctx, builder, namedTypes);
    declareFunctions();
    generateNativeFuncs(natives);
}

llvm::orc::ThreadSafeModule Compiler::compile(std::shared_ptr<typedAST::Function> _code, string mainFnName){
    createMainEntrypoint(mainFnName);
    try {
        for (auto stmt: _code->block.stmts) {
            stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
        }
    }catch(CompilerError err){
        std::cout<<fmt::format("Compiler exited because of error: '{}'.", err.reason);
    }
    // Get all string constants into gc
    llvm::IRBuilder<> tempBuilder(*ctx);
    tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
    auto val = tempBuilder.CreateIntrinsic(tempBuilder.getPtrTy(), llvm::Intrinsic::frameaddress, {tempBuilder.getInt32(0)});
    tempBuilder.CreateCall(safeGetFunc("gcInit"), {curModule->getNamedGlobal("gcFlag")});
    tempBuilder.CreateCall(safeGetFunc("threadInit"), {val});
    for(auto strObj : ESLStrings){
        tempBuilder.CreateCall(safeGetFunc("gcInternStr"), {strObj.second});
    }
    // Ends the main function
    builder.CreateRetVoid();
    llvm::verifyFunction(*inProgressFuncs.top().fn);
    llvm::verifyModule(*curModule, &llvm::errs());
    llvm::errs()<<"--------------------Unoptimized module--------------------\n";
#ifdef COMPILER_DEBUG
    //curModule->print(llvm::errs(), nullptr);
#endif
    debugEmitter.finalize();
    optimizeModule(*curModule);
    return std::move(llvm::orc::ThreadSafeModule(std::move(curModule), std::move(ctx)));
}


llvm::Value* Compiler::visitVarDecl(typedAST::VarDecl* decl) {
    debugEmitter.emitNewLocation(builder, decl->dbgInfo.varName);
    switch(decl->varType){
        case typedAST::VarType::LOCAL:{
            // Alloca at the beginning of the function to make use of mem2reg pass
            llvm::IRBuilder<> tempBuilder(*ctx);
            tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
            auto tmp = tempBuilder.CreateAlloca(getESLValType(), nullptr, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);
            debugEmitter.addLocalVarDecl(builder, tmp, decl->dbgInfo.varName, false);
            break;
        }
        case typedAST::VarType::FREEVAR:{
            llvm::IRBuilder<> tempBuilder(*ctx);
            tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
            // Creates a heap allocated free variable
            size_t freevarSize = curModule->getDataLayout().getTypeAllocSize(namedTypes["ObjFreevar"]);
            llvm::Value* var = builder.CreateCall(safeGetFunc("gcAlloc"), { builder.getInt64(freevarSize)}, decl->dbgInfo.varName.getLexeme());
            // Get pointers to obj type field and payload field
            llvm::Value* objTypePtr = builder.CreateInBoundsGEP(namedTypes["Obj"], var, {builder.getInt32(0), builder.getInt32(1)});
            llvm::Value* storedValPtr = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjFreevar"], var, 0, 1);
            // Store type tag and null as default value
            builder.CreateStore(builder.getInt8(+object::ObjType::FREEVAR), objTypePtr);
            builder.CreateStore(builder.getInt64(mask_signature_null), storedValPtr);
            variables.insert_or_assign(decl->uuid, var);
            debugEmitter.addLocalVarDecl(builder, var, decl->dbgInfo.varName, false);
            break;
        }
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL:{
            string varName = decl->dbgInfo.varName.getLexeme() + std::to_string(decl->uuid);
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(*curModule, getESLValType(), false,
                                                                  llvm::GlobalVariable::PrivateLinkage,
                                                                  ConstCastToESLVal(builder.getInt64(mask_signature_null)),varName);
            gvar->setAlignment(llvm::Align::Of<Value>());
            // Globals aren't on the stack, so they need to be marked for GC collection separately
            if(decl->varType == typedAST::VarType::GLOBAL) {
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

llvm::Value* Compiler::visitVarRead(typedAST::VarRead* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    return codegenVarRead(expr->varPtr);
}
llvm::Value* Compiler::visitVarStore(typedAST::VarStore* expr) {
    llvm::Value* valToStore = expr->toStore->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    return codegenVarStore(expr->varPtr, valToStore);
}
llvm::Value* Compiler::visitVarReadNative(typedAST::VarReadNative* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.varName);
    // Since native variables are known at compile time reading them is noop
    return nativeFunctions[expr->nativeName];
}

static bool isFloatingPointOp(typedAST::ArithmeticOp op){
    return op == typedAST::ArithmeticOp::SUB || op == typedAST::ArithmeticOp::MUL ||
           op == typedAST::ArithmeticOp::DIV || op == typedAST::ArithmeticOp::MOD;
}

static llvm::Value* compileArithmeticOp(typedAST::ArithmeticOp op, llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs){
    using typedAST::ArithmeticOp;
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

llvm::Value* Compiler::visitArithmeticExpr(typedAST::ArithmeticExpr* expr) {
    using typedAST::ArithmeticOp;
    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
    if(expr->opType == ArithmeticOp::ADD) {
        // If types of both lhs and rhs are known unnecessary runtime checks are skipped
        // TODO: maybe split this so if variable type of one expression is known at compile time and the other isn't we only check for that type
        if(exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))){
            auto castlhs = ESLValTo(lhs, builder.getDoubleTy());
            auto castrhs = ESLValTo(rhs, builder.getDoubleTy());
            return CastToESLVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
        }else if(exprIsType(expr->lhs, expr->lhs, types::getBasicType(types::TypeFlag::STRING))){
            return builder.CreateCall(safeGetFunc("strAdd"), {lhs, rhs});
        }else return codegenBinaryAdd(lhs, rhs, expr->dbgInfo.op);
    }
    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        // If either or both aren't numbers, go to error since all other ops work only on numbers
        string err = fmt::format("Operator '{}' expects numbers, got '{}' and '{}'.", expr->dbgInfo.op.getLexeme(), "{}", "{}");
        createTypeCheckBinary(err, lhs, rhs, getNumberTypeMasks());
    }

    // Transforms the operands into the required type
    lhs = ESLValTo(lhs, builder.getDoubleTy());
    rhs = ESLValTo(rhs, builder.getDoubleTy());

    llvm::Value* val = compileArithmeticOp(expr->opType, builder, lhs, rhs);
    return CastToESLVal(val);
}
llvm::Value* Compiler::visitComparisonExpr(typedAST::ComparisonExpr* expr) {
    using typedAST::ComparisonOp;
    // Special cases of comparison operators that don't use numbers
    if(expr->opType == ComparisonOp::OR || expr->opType == ComparisonOp::AND){
        return codegenLogicOps(expr->lhs, expr->rhs, expr->opType);
    }
    else if(expr->opType == ComparisonOp::EQUAL || expr->opType == ComparisonOp::NOT_EQUAL){
        return codegenCmp(expr->lhs, expr->rhs, expr->opType == ComparisonOp::NOT_EQUAL);
    }

    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        string err = fmt::format("Operator '{}' expects numbers, got '{}' and '{}'.", expr->dbgInfo.op.getLexeme(), "{}", "{}");
        createTypeCheckBinary(err, lhs, rhs, getNumberTypeMasks());
    }

    lhs = ESLValTo(lhs, builder.getDoubleTy());
    rhs = ESLValTo(rhs, builder.getDoubleTy());
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
llvm::Value* Compiler::visitInstanceofExpr(typedAST::InstanceofExpr* expr){
    llvm::Value* inst = expr->lhs->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);

    auto subclassesInterval = classHierarchy[expr->className];
    return builder.CreateCall(safeGetFunc("isInstAndClass"),
                              {inst, builder.getInt32(subclassesInterval.first), builder.getInt32(subclassesInterval.second)});
}
llvm::Value* Compiler::visitUnaryExpr(typedAST::UnaryExpr* expr) {
    using typedAST::UnaryOp;
    if(expr->opType == UnaryOp::NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
        // If type is known to be a bool skip the runtime check and just execute the expr
        if(!exprIsType(expr->rhs, types::getBasicType(types::TypeFlag::BOOL))) {
            createTypeCheckUnary("Operator '!' expects boolean value, got '{}'", rhs, getBoolTypeMasks());
        }

        return CastToESLVal(builder.CreateXor(ESLValTo(rhs, builder.getInt64Ty()), mask_type_true));
    }else if(expr->opType == UnaryOp::FNEG || expr->opType == UnaryOp::BIN_NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        debugEmitter.emitNewLocation(builder, expr->dbgInfo.op);
        return codegenNeg(rhs, expr->rhs->exprType, expr->opType, expr->dbgInfo.op);
    }else{
        return codegenIncrement(expr->opType, expr->rhs, expr->dbgInfo.op);
    }
}

llvm::Value* Compiler::visitLiteralExpr(typedAST::LiteralExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.literal);
    switch(expr->val.index()){
        case 0: {
            auto tmp = llvm::ConstantFP::get(*ctx, llvm::APFloat(get<double>(expr->val)));
            return CastToESLVal(tmp);
        }
        case 1: {
            uInt64 val = get<bool>(expr->val) ? mask_signature_true : mask_signature_false;
            return CastToESLVal(builder.getInt64(val));
        }
        case 2: return CastToESLVal(builder.getInt64(mask_signature_null));
        case 3: {
            return createESLString(get<string>(expr->val));
        }
        default: errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    __builtin_unreachable();
}

llvm::Value* Compiler::visitHashmapExpr(typedAST::HashmapExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.brace1);
    vector<llvm::Value*> args = {builder.getInt32(expr->fields.size())};
    // For each field, compile it and get the constant of the field name
    int i = 0;
    for (auto entry : expr->fields) {
        debugEmitter.emitNewLocation(builder, expr->dbgInfo.fields[i].str);
        // This gets rid of quotes, ""Hello world""->"Hello world"
        args.push_back(createESLString(entry.first));
        args.push_back(entry.second->codegen(this));
        i++;
    }

    return builder.CreateCall(safeGetFunc("createHashMap"), args);
}
llvm::Value* Compiler::visitArrayExpr(typedAST::ArrayExpr* expr) {
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.bracket1);
    vector<llvm::Value*> vals;
    for(auto mem : expr->fields){
        vals.push_back(mem->codegen(this));
    }
    auto arrNum = builder.getInt32(vals.size());
    auto arr = builder.CreateCall(safeGetFunc("createArr"), {arrNum}, "array");
    // I think this should be faster than passing everything to "createArr", but I could be wrong
    llvm::Value* arrPtr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjArray"], arrPtr, 0, 3);
    storagePtr = builder.CreateLoad(namedTypes["ObjArrayStoragePtr"], storagePtr, "storage.ptr");
    storagePtr = builder.CreateConstInBoundsGEP1_32(namedTypes["ObjArrayStorage"], storagePtr, 1, "data.ptr");
    llvm::Value* containsObj = builder.getInt8(0);
    for(int i = 0; i < vals.size(); i++){
        auto gep = builder.CreateConstInBoundsGEP1_32(getESLValType(), storagePtr, i);
        builder.CreateStore(vals[i], gep);
        llvm::Value* isObj = builder.CreateZExt(builder.CreateCall(safeGetFunc("isObj"), vals[i]), builder.getInt8Ty());
        containsObj = builder.CreateOr(containsObj, isObj);
    }
    arrPtr = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjArray"], arrPtr, 0, 1, "arr.contains.obj");
    builder.CreateStore(containsObj, arrPtr);
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

llvm::Value* Compiler::visitCollectionGet(typedAST::CollectionGet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.accessor);

    bool optArrIndex = exprIsType(expr->field, types::getBasicType(types::TypeFlag::NUMBER));
    bool optMapString = exprIsType(expr->field, types::getBasicType(types::TypeFlag::STRING));

    if(exprIsComplexType(expr->collection, types::TypeFlag::ARRAY)){
        return getArrElement(collection, field, optArrIndex, expr->dbgInfo.accessor);

    }else if(exprIsComplexType(expr->collection, types::TypeFlag::HASHMAP)){
        return getMapElement(collection, field, optMapString, expr->dbgInfo.accessor);
    }
    // Uses switch instead of chained comparisons, this should be faster?
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "is.arr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "is.map");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto num = collectionTypeCheck(builder, collection, safeGetFunc("isObjType"));
    createWeightedSwitch(num, {{1, isArray}, {2, isHashmap}}, errorBB, {0, 1<<31, 1<<31});

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
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr("Expected an array or hashmap, got '{}'.");
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::WRONG_TYPE),
                                                    collection, intMax, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(getESLValType(), 2, "collection.get");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;

}

llvm::Value* Compiler::visitCollectionSet(typedAST::CollectionSet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);
    llvm::Value* val = expr->toStore->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.accessor);

    bool optArrIndex = exprIsType(expr->field, types::getBasicType(types::TypeFlag::NUMBER));
    bool optMapString = exprIsType(expr->field, types::getBasicType(types::TypeFlag::STRING));
    bool optRhs = exprIsType(expr->toStore, types::getBasicType(types::TypeFlag::NUMBER));

    if(exprIsComplexType(expr->collection, types::TypeFlag::ARRAY)){
        return setArrElement(collection, field, val, optArrIndex, optRhs, expr->operationType,
                             expr->dbgInfo.op);
    }else if(exprIsComplexType(expr->collection, types::TypeFlag::HASHMAP)){
        return setMapElement(collection, field, val, optMapString, optRhs, expr->operationType,
                             expr->dbgInfo.op);
    }
    // Uses switch instead of chained comparisons, this should be faster?
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "is.arr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "is.map");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto num = collectionTypeCheck(builder, collection, safeGetFunc("isObjType"));
    createWeightedSwitch(num, {{1, isArray}, {2, isHashmap}}, errorBB, {0, 1<<31, 1<<31});

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
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr("Expected an array or hashmap, got '{}'.");
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::WRONG_TYPE),
                                                    collection, intMax, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(getESLValType(), 2, "collection.set");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;
}

llvm::Value* Compiler::visitConditionalExpr(typedAST::ConditionalExpr* expr) {
    auto condtmp = expr->cond->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.questionmark);
    llvm::Value* cond = nullptr;
    // If condition is known to be a boolean the isNull check can be skipped
    if(exprIsType(expr->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(safeGetFunc("decodeBool"), condtmp);
    }
    else cond = builder.CreateCall(safeGetFunc("isTruthy"), condtmp);

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*ctx, "condexpr.then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*ctx, "condexpr.else");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "condexpr.merge");

    builder.CreateCondBr(cond, thenBB, elseBB);
    //Emits code to conditionally execute mhs(thenBB) and rhs(elseBB)
    builder.SetInsertPoint(thenBB);
    llvm::Value* thentmp = expr->thenExpr->codegen(this);
    builder.CreateBr(mergeBB);
    // PHI nodes need up-to-date block info to know which block control is coming from
    thenBB = builder.GetInsertBlock();

    func->insert(func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    llvm::Value* elsetmp = expr->elseExpr->codegen(this);
    // PHI nodes need up-to-date block info to know which block control is coming from
    elseBB = builder.GetInsertBlock();

    // Get the results
    builder.CreateBr(mergeBB);
    func->insert(func->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    llvm::PHINode *PN = builder.CreatePHI(getESLValType(), 2, "condexpr.res");
    PN->addIncoming(thentmp, thenBB);
    PN->addIncoming(elsetmp, elseBB);

    return PN;
}

llvm::Value* Compiler::visitCallExpr(typedAST::CallExpr* expr) {
    bool opt = exprIsComplexType(expr->callee, types::TypeFlag::FUNCTION);
    llvm::Value* closureVal = expr->callee->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.paren1);
    // First param of every function is reserved for closure ptr
    vector<llvm::Value*> args = {closureVal};
    for(auto arg : expr->args) args.push_back(arg->codegen(this));

    if(!opt) {
        // -1 because arg checking doesn't take closure ptr into account
        llvm::FunctionCallee indirectFn = setupUnoptCall(closureVal, args.size(), expr->dbgInfo.paren1);
        return builder.CreateCall(indirectFn, args);
    }

    auto funcType = std::reinterpret_pointer_cast<types::FunctionType>(typeEnv[expr->callee->exprType]);
    // TODO: this should be done in a separate pass
    if(funcType->argCount != expr->args.size()){
        errHandler.reportError(fmt::format("Function expects {} parameters, got {} arguments.", funcType->argCount, expr->args.size()),
                                      expr->dbgInfo.paren1);
        throw CompilerError("Incorrect number of arguments passed");
    }
    return builder.CreateCall(functions[funcType], args, "call.res");
}
llvm::Value* Compiler::visitInvokeExpr(typedAST::InvokeExpr* expr) {
    auto inst = expr->inst->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.method);

    vector<llvm::Value*> args = {};
    for(auto arg : expr->args) args.push_back(arg->codegen(this));

    if(typeEnv[expr->inst->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->inst->exprType])->klass->name];
        // args get modified to include closure and instance(if needed)
        llvm::FunctionCallee func = optimizeInvoke(inst, expr->field, klass, args, expr->dbgInfo.method);
        return builder.CreateCall(func, args);
    }

    // Creates the switch which returns either method or field
    return unoptimizedInvoke(inst, expr->field, args, expr->dbgInfo.method);
}

llvm::Value* Compiler::visitNewExpr(typedAST::NewExpr* expr) {
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

    llvm::Value* inst = builder.CreateBitCast(memptr, namedTypes["ObjPtr"]);
    inst = builder.CreateCall(safeGetFunc("encodeObj"), {inst, builder.getInt64(+object::ObjType::INSTANCE)});
    // If there is a constructor declared in this class, call it
    if(klass.ty->methods.contains(name)){
        std::pair<types::tyVarIdx, uInt64> fnty = klass.ty->methods[name];
        auto fn  = functions[typeEnv[fnty.first]];
        auto ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosureAligned"], klass.methodArrPtr,
                                                                 builder.getInt32(fnty.second));
        // Need to tag the method
        ptr = constObjToVal(ptr, +object::ObjType::CLOSURE);
        vector<llvm::Value*> args = {ptr, inst};
        for(auto arg : expr->args) args.push_back(arg->codegen(this));
        return builder.CreateCall(fn, args);
    }else{
        // TODO: error if there are arguments passed when the constructor doesn't exist
        return inst;
    }
}

//TODO: create a wrapper function that takes a single argument(param) in an array and then calls the function with them
llvm::Value* Compiler::visitSpawnStmt(typedAST::SpawnStmt* stmt){
    // func being nonnull means we got optimized function
    if(stmt->call->type == typedAST::NodeType::CALL){
        std::shared_ptr<typedAST::CallExpr> expr = std::reinterpret_pointer_cast<typedAST::CallExpr>(stmt->call);
        bool opt = exprIsComplexType(expr->callee, types::TypeFlag::FUNCTION);
        llvm::FunctionCallee callee;
        llvm::Value* closureVal = expr->callee->codegen(this);
        // Inserts tagged closure since that is what functions expect
        vector<llvm::Value*> args = {closureVal};
        for(auto arg : expr->args) args.push_back(arg->codegen(this));
        debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);

        if(!opt) {
            // -1 because arg checking doesn't take closure ptr into account
            callee = setupUnoptCall(closureVal, args.size(), expr->dbgInfo.paren1);
        }else{
            auto funcType = std::reinterpret_pointer_cast<types::FunctionType>(typeEnv[expr->callee->exprType]);
            // TODO: this should be done in a separate pass
            if(funcType->argCount != expr->args.size()){
                errHandler.reportError(fmt::format("Function expects {} parameters, got {} arguments.", funcType->argCount, expr->args.size()),
                                              expr->dbgInfo.paren1);
                throw CompilerError("Incorrect number of arguments passed");
            }
            callee = functions[funcType];
        }
        setupThreadCreation(callee, args);
    }else{ // Must be NodeType::INVOKE
        std::shared_ptr<typedAST::InvokeExpr> expr = std::reinterpret_pointer_cast<typedAST::InvokeExpr>(stmt->call);
        auto encodedInst = expr->inst->codegen(this);

        vector<llvm::Value*> args;
        for(auto arg : expr->args) args.push_back(arg->codegen(this));
        debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);

        if(typeEnv[expr->inst->exprType]->type == types::TypeFlag::INSTANCE) {
            auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->inst->exprType])->klass->name];
            // args get modified to include closure and instance(if needed)
            llvm::FunctionCallee func = optimizeInvoke(encodedInst, expr->field, klass, args, expr->dbgInfo.method);
            setupThreadCreation(func, args);
            return nullptr;
        }
        auto [inst, klass] = instGetClassPtr(encodedInst, expr->dbgInfo.accessor);
        auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, expr->field);

        llvm::Function *F = builder.GetInsertBlock()->getParent();

        llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
        llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
        llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(*ctx, "methods");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

        createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx), {{1, fieldBB}, {2, methodBB}},
                             errorBB, {0, 1<<31, 1<<31});

        F->insert(F->end(), fieldBB);
        builder.SetInsertPoint(fieldBB);
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(1)});
        auto fieldPtr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);
        auto field =  builder.CreateLoad(getESLValType(), fieldPtr);
        // Erases closure from args list because they are needed for method calling below
        args.insert(args.begin(), field);
        setupThreadCreation(setupUnoptCall(field, args.size(), expr->dbgInfo.method),  args);
        args.erase(args.begin());
        builder.CreateBr(mergeBB);

        F->insert(F->end(), methodBB);
        builder.SetInsertPoint(methodBB);
        // First load the ObjClosurePtr(we treat the offset into the ObjClosure array that the class has as a standalone pointer to that closure)
        ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(1)});
        llvm::Value* val = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], ptr, methodIdx);
        // Since this is a raw ObjClosure pointer we tag it first
        auto method = builder.CreateCall(safeGetFunc("encodeObj"), {val, builder.getInt64(+object::ObjType::CLOSURE)});
        args.insert(args.begin(), {method, encodedInst});
        // -1 because closure ptr is not taken into account by arg checking
        setupThreadCreation(setupUnoptCall(method, args.size(), expr->dbgInfo.method),  args);
        builder.CreateBr(mergeBB);

        F->insert(F->end(), errorBB);
        builder.SetInsertPoint(errorBB);
        createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", expr->field, encodedInst);
        builder.CreateUnreachable();

        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
    }
    return nullptr; // Stmts return nullptr on codegen
}


llvm::Value* Compiler::visitCreateClosureExpr(typedAST::CreateClosureExpr* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    inProgressFuncs.emplace(startFuncDef(expr->fn->name, expr->fn->fnTy, expr->dbgInfo.keyword));

    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    llvm::Value* cl = builder.CreateCall(safeGetFunc("decodeClosure"), inProgressFuncs.top().fn->getArg(0), "closure");
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        llvm::Value* freevarPtr = builder.CreateGEP(namedTypes["ObjClosure"], cl, builder.getInt32(1));
        freevarPtr = builder.CreateInBoundsGEP(namedTypes["ObjFreevarPtr"], freevarPtr, builder.getInt32(i));
        llvm::Value* tmp = builder.CreateLoad(namedTypes["ObjFreevarPtr"], freevarPtr);
        variables.insert_or_assign(freevar.second->uuid, tmp);
    }

    declareFuncArgs(expr->fn->args);

    for(auto stmt : expr->fn->block.stmts){
        stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto lambda = inProgressFuncs.top().fn;
    inProgressFuncs.pop();
    debugEmitter.popScope(builder, expr->dbgInfo.keyword);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(lambda, builder.getPtrTy());
    auto arity = builder.getInt8(expr->fn->args.size());
    auto name = createConstStr(expr->fn->name);

    // Every function is converted to a closure(if even it has 0 freevars for ease of use when calling)
    // If expr->freevars.size() is 0 then no array for freevars is allocated
    vector<llvm::Value*> closureConstructorArgs = {typeErasedFn, arity, name, builder.getInt32(expr->freevars.size())};

    // Freevars are gathered after switching to the enclosing function
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        // Pushes the local var/freevars that is enclosed by lambda to the arg list to be added to the closure
        closureConstructorArgs.push_back(variables.at(freevar.first->uuid));
        // Removes the freevars uuid from the variable pool since compilation for this function is done and this won't be used again
        variables.erase(freevar.second->uuid);
    }
    // Create the closure and put the freevars in it, createClosure is a vararg function
    return builder.CreateCall(safeGetFunc("createClosure"), closureConstructorArgs);
}

llvm::Value* Compiler::visitFuncDecl(typedAST::FuncDecl* stmt) {
    inProgressFuncs.emplace(startFuncDef(stmt->fn->name, stmt->fn->fnTy, stmt->dbgInfo.name));

    declareFuncArgs(stmt->fn->args);

    for(auto s : stmt->fn->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

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
    auto name = createConstStr(stmt->fn->name);
    auto freeVarCnt = builder.getInt8(0);
    auto ty = llvm::PointerType::getUnqual(namedTypes["ObjFreevarPtr"]);

    // Create function constant
    llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                         {createConstObjHeader(+object::ObjType::CLOSURE),
                                          arity, freeVarCnt, typeErasedFn, name});
    // Creates a place in memory for the function and stores it there
    llvm::Constant* fnLoc = storeConstObj(fnC);
    auto gv = (llvm::dyn_cast<llvm::GlobalVariable>(variables.at(stmt->globalVarUuid)));
    gv->setInitializer(constObjToVal(fnLoc, +object::ObjType::CLOSURE));
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitReturnStmt(typedAST::ReturnStmt* stmt) {
    debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);
    builder.CreateRet(stmt->expr->codegen(this));
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitUncondJump(typedAST::UncondJump* stmt) {
    debugEmitter.emitNewLocation(builder, stmt->dbgInfo.keyword);
    switch(stmt->jmpType){
        case typedAST::JumpType::BREAK: builder.CreateBr(breakJumpDest.top()); break;
        case typedAST::JumpType::CONTINUE: builder.CreateBr(continueJumpDest.top()); break;
        case typedAST::JumpType::ADVANCE: builder.CreateBr(advanceJumpDest.top()); break;
        default: __builtin_unreachable();
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitIfStmt(typedAST::IfStmt* stmt) {
    auto condtmp = stmt->cond->codegen(this);
    llvm::Value* cond;
    if(exprIsType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(safeGetFunc("decodeBool"), condtmp);
    }else cond = builder.CreateCall(safeGetFunc("isTruthy"), condtmp);

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*ctx, "if.then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*ctx, "if.else");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "if.merge");

    builder.CreateCondBr(cond, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);
    codegenBlock(stmt->thenBlock);
    if(!stmt->thenBlock.terminates) builder.CreateBr(mergeBB);

    func->insert(func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    codegenBlock(stmt->elseBlock);
    if(!stmt->elseBlock.terminates) builder.CreateBr(mergeBB);

    func->insert(func->end(), mergeBB);
    // Sets the builder up to emit code after the if stmt
    builder.SetInsertPoint(mergeBB);
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitWhileStmt(typedAST::WhileStmt* stmt) {
    bool canOptimize = stmt->cond ? exprIsType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL)) : true;
    auto decodeFn = canOptimize ? safeGetFunc("decodeBool") : safeGetFunc("isTruthy");

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* headerBB = llvm::BasicBlock::Create(*ctx, "while.header", func);
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
    func->insert(func->end(), loopBB);
    builder.SetInsertPoint(loopBB);
    codegenBlock(stmt->loopBody);
    // As the name suggests, this is eval-ed after the main body of the loop is ran
    if(stmt->afterLoopExpr) stmt->afterLoopExpr->codegen(this);
    // Only jump to condition if the main body doesn't terminate already(eg. an unconditional break stmt at the end of loop)
    if(!stmt->loopBody.terminates){
        builder.CreateCall(safeGetFunc("safepoint_poll"));
        builder.CreateBr(headerBB);
    }

    func->insert(func->end(), mergeBB);
    // Sets the builder up to emit code after the while stmt
    builder.SetInsertPoint(mergeBB);
    // Pop destinations so that any break/continue for an outer loop works correctly
    continueJumpDest.pop();
    breakJumpDest.pop();

    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitSwitchStmt(typedAST::SwitchStmt* stmt) {
    auto compVal = stmt->cond->codegen(this);
    // Switch directly compares Int64 contents to determine equality, this should work for most ints represented as doubles(i hope)
    compVal = ESLValTo(compVal, builder.getInt64Ty());
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // Have to create the basic blocks before codegening because of advance stmt
    vector<llvm::BasicBlock *> blocks = createNCaseBlocks(stmt->cases.size());
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "switch.merge");
    llvm::BasicBlock* defaultDest = stmt->defaultCaseBlockNum == -1 ? mergeBB : blocks[stmt->defaultCaseBlockNum];

    // If this switch doesn't contain strings as case constants it can be optimized
    if(!stmt->containsStrings){
        auto inst = builder.CreateSwitch(compVal, defaultDest);
        for(auto constant : stmt->constants){
            inst->addCase(createSwitchConstantInt(constant.first), blocks[constant.second]);
        }
    }
    else {
        llvm::Value *BBIdx = createSeqCmp(compVal, stmt->constants);
        auto inst = builder.CreateSwitch(BBIdx, defaultDest);
        for (int i = 0; i < blocks.size(); i++) {
            inst->addCase(builder.getInt32(i), blocks[i]);
        }
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
        if(!stmt->cases[i].terminates){
            builder.CreateBr(mergeBB);
        }
        advanceJumpDest.pop();
    }
    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    breakJumpDest.pop();
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitClassDecl(typedAST::ClassDecl* stmt) {
    auto name = createConstStr(stmt->fullName);
    auto fieldsLen = builder.getInt16(stmt->classType->fields.size());
    auto methodsLen = builder.getInt16(stmt->classType->methods.size());
    // Generates functions to call when type of instance is not known at compile time to get the index into the field/method array
    auto fieldsFunc = createStrToIdxFunc(stmt->classType, false);
    auto methodsFunc = createStrToIdxFunc(stmt->classType, true);
    // Result of a dfs done on the class graph
    auto subClassIdxStart = builder.getInt32(classHierarchy[stmt->fullName].first);
    auto subClassIdxEnd = builder.getInt32(classHierarchy[stmt->fullName].second);

    vector<llvm::Constant*> methods(stmt->classType->methods.size());
    // Copy all methods of parent class, these can then be (possibly) overriden
    if(classes.contains(stmt->parentClassName)){
        int i = 0;
        for(llvm::Constant* parentMethod : classes[stmt->parentClassName].methodArr){
            methods[i++] = parentMethod;
        }
    }
    for(auto [mName, method] : stmt->methods){
        llvm::Function* methodFn = functions[method.first.code->fnTy];
        methodFn->setName(stmt->fullName + mName);
        // Creates an ObjClosure associated with this method
        methods[method.second] = createMethodObj(method.first, methodFn);
    }
    llvm::Constant* methodArr = llvm::ConstantArray::get(llvm::ArrayType::get(namedTypes["ObjClosureAligned"],
                                                                   methods.size()), methods);

    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClass"), {
            createConstObjHeader(+object::ObjType::CLASS), methodsLen, fieldsLen, subClassIdxStart, subClassIdxEnd, name, methodsFunc,
            fieldsFunc, builder.getInt64(0)});
    obj = llvm::ConstantStruct::getAnon({obj, methodArr});
    llvm::GlobalVariable* klass = new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                                           llvm::GlobalVariable::PrivateLinkage, obj);
    klass->setAlignment(llvm::Align(16));
    llvm::Type* type = obj->getType();
    // ArrayRef can't be taken out into its own thing because it causes a segfault for some reason
    llvm::Constant* methodArrPtr = llvm::ConstantExpr::getInBoundsGetElementPtr(type, klass,
                                                                                llvm::ArrayRef<llvm::Constant*>({builder.getInt32(0), builder.getInt32(1)}));
    // Associates a full class name with the class object and instance template
    classes[stmt->fullName] = Class(klass, createInstanceTemplate(klass, stmt->fields.size()),
                                    stmt->classType, methods, methodArrPtr);

    for(auto [mName, method] : stmt->methods){
        codegenMethod(stmt->fullName, method.first, subClassIdxStart, subClassIdxEnd);
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitInstGet(typedAST::InstGet* expr) {
    auto inst = expr->instance->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.field);
    if(typeEnv[expr->instance->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        return optimizeInstGet(inst, expr->field, klass);
    }
    // Creates the switch which returns either method or field
    return instGetUnoptimized(inst, expr->field, expr->dbgInfo.accessor);
}
llvm::Value* Compiler::visitInstSet(typedAST::InstSet* expr) {
    auto inst = expr->instance->codegen(this);
    debugEmitter.emitNewLocation(builder, expr->dbgInfo.field);
    llvm::Value* fieldPtr = nullptr;
    if(typeEnv[expr->instance->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        fieldPtr = getOptInstFieldPtr(inst, klass, expr->field);
    }else{
        fieldPtr = getUnoptInstFieldPtr(inst, expr->field, expr->dbgInfo.field);
    }
    auto val = expr->toStore->codegen(this);

    if(expr->operationType == typedAST::SetType::SET){
        builder.CreateStore(val, fieldPtr);
        return val;
    }else if(expr->operationType == typedAST::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateLoad(getESLValType(), fieldPtr);
        val = codegenBinaryAdd(storedVal, val, expr->dbgInfo.op);
        builder.CreateStore(val, fieldPtr);
        return val;
    }
    auto storedField = builder.CreateLoad(getESLValType(), fieldPtr);
    if(!exprIsType(expr->toStore, types::getBasicType(types::TypeFlag::NUMBER))) {
        string err = fmt::format("Operator '{}' expects numbers, field '{}' is '{}', rhs is '{}'.",
                                 expr->dbgInfo.op.getLexeme(), expr->field, "{}", "{}");
        createTypeCheckBinary(err, storedField, val, getNumberTypeMasks());
    }else{
        string err = fmt::format("Operator '{}' expects numbers, field '{}' is '{}'.", expr->dbgInfo.op.getLexeme(), expr->field, "{}");
        createTypeCheckUnary(err, storedField, getNumberTypeMasks());
    }

    val = CastToESLVal(decoupleSetOperation(storedField, val, expr->operationType, expr->dbgInfo.op));
    builder.CreateStore(val, fieldPtr);
    return val;
}

llvm::Value* Compiler::visitScopeBlock(typedAST::ScopeEdge* stmt) {
    // Erases local variables which will no longer be used, done to keep memory usage at least somewhat reasonable
    for(auto uuid : stmt->toPop){
        variables.erase(uuid);
    }
    // TODO: very hacky, but macroExpander creates synthetic scopes that shouldn't be a part of debug info
    if(stmt->location.isSynthetic) return nullptr;
    if(stmt->edgeType == typedAST::ScopeEdgeType::START) debugEmitter.addScope(builder, stmt->location);
    else debugEmitter.popScope(builder, stmt->location);
    return nullptr; // Stmts return nullptr on codegen
}

static void setFuncAttrs(vector<llvm::Attribute>& attrs, llvm::Function* func){
    for(auto& attr : attrs) func->addFnAttr(attr);
}
void Compiler::setupModule(const llvm::DataLayout& DL){
    curModule = std::make_unique<llvm::Module>("Module", *ctx);
    curModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                             llvm::DEBUG_METADATA_VERSION);
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    curModule->setDataLayout(DL);
    curModule->setTargetTriple(targetTriple);
    curModule->getOrInsertGlobal("gcFlag", builder.getInt64Ty());
    llvm::GlobalVariable* gvar = curModule->getNamedGlobal("gcFlag");
    gvar->setLinkage(llvm::GlobalVariable::PrivateLinkage);
    gvar->setInitializer(builder.getInt64(0));
    gvar->setAlignment(llvm::Align::Of<uint64_t>());
    ESLFuncAttrs.push_back(llvm::Attribute::get(*ctx, "uwtable", "sync"));
    ESLFuncAttrs.push_back(llvm::Attribute::get(*ctx, "no-trapping-math", "true"));
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

void Compiler::declareFunctions(){
    for(types::tyPtr type: typeEnv){
        if(type->type != types::TypeFlag::FUNCTION || functions.contains(type)) continue;
        std::shared_ptr<types::FunctionType> fnType = std::reinterpret_pointer_cast<types::FunctionType>(type);
        // First argument is always the thread data ptr
        vector<llvm::Type*> params = {builder.getPtrTy()};
        // Second argument is always the closure structure
        for(int i = 0; i < fnType->argCount + 1; i++) params.push_back(getESLValType());
        llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
        auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, "thunk", curModule.get());
        setFuncAttrs(ESLFuncAttrs, tmp);
        tmp->setGC("statepoint-example");
        // Creates a connection between function types and functions
        functions.insert_or_assign(fnType, tmp);
    }
}

void Compiler::createMainEntrypoint(string entrypointName){
    // Create internal entrypoint function, takes in the thread data ptr
    llvm::FunctionType* entryFT = llvm::FunctionType::get(builder.getVoidTy(),false);
    auto entryFn = llvm::Function::Create(entryFT, llvm::Function::PrivateLinkage, "entrypoint", curModule.get());
    setFuncAttrs(ESLFuncAttrs, entryFn);
    entryFn->setGC("statepoint-example");
    entryFn->addFnAttr(llvm::Attribute::AttrKind::NoInline);
    debugEmitter.addMainFunc(entryFn);
    // Create the runtime entrypoint that calls the internal entrypoint
    llvm::FunctionType* FT = llvm::FunctionType::get(builder.getInt32Ty(),{builder.getInt32Ty(), builder.getPtrTy()}, false);
    auto tmpfn = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, entrypointName, curModule.get());
    setFuncAttrs(ESLFuncAttrs, tmpfn);
    tmpfn->setGC("statepoint-example");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmpfn);
    builder.SetInsertPoint(BB);

    auto call = builder.CreateCall(entryFn);
    builder.CreateCall(safeGetFunc("exit"), builder.getInt32(0));
    builder.CreateRet(builder.getInt32(0));
    llvm::verifyFunction(*tmpfn);

    // Setup to start writing to internal entrypoint
    entryFn->setGC("statepoint-example");
    BB = llvm::BasicBlock::Create(*ctx, "entry", entryFn);
    builder.SetInsertPoint(BB);
    inProgressFuncs.emplace(entryFn);
}

#pragma region helpers

// Compile time type checking
bool Compiler::exprIsType(const typedExprPtr expr, const types::tyPtr ty){
    return typeEnv[expr->exprType] == ty;
}
bool Compiler::exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty) {
    return exprIsType(expr1, ty) && exprIsType(expr2, ty);
}
bool Compiler::exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag){
    return typeEnv[expr->exprType]->type == flag;
}


// Runtime type checking
// All type info is contained inside the 64bits of the NaN boxed value, so we can just ICmp
// runtimeError requires that the nonUsed arguments be UINT64_MAX
void Compiler::createTypeCheckUnary(const string err, llvm::Value* const val, std::tuple<uint64_t, uint64_t, bool> masks){
    auto [expected, mask, useNEQ] = masks;

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "no.error");

    llvm::Value* castVal = ESLValTo(val, builder.getInt64Ty());
    llvm::Value* expectedType = builder.getInt64(expected);
    llvm::Value* cond;
    if(useNEQ) cond = builder.CreateICmpNE(builder.CreateAnd(castVal, mask), expectedType, "type.check");
    else cond = builder.CreateICmpEQ(builder.CreateAnd(castVal, mask), expectedType, "type.check");

    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(true)});
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::WRONG_TYPE),
                                                    castVal, intMax, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
}
void Compiler::createTypeCheckBinary(const string err, llvm::Value* const lhs, llvm::Value* const rhs, std::tuple<uint64_t, uint64_t, bool> masks){
    auto [expected, mask, useNEQ] = masks;

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "no.error");

    llvm::Value* castLhs = ESLValTo(lhs, builder.getInt64Ty());
    llvm::Value* castRhs = ESLValTo(rhs, builder.getInt64Ty());
    llvm::Value* expectedType = builder.getInt64(expected);

    llvm::Value* condLhs;
    llvm::Value* condRhs;
    if(useNEQ) {
        condLhs = builder.CreateICmpNE(builder.CreateAnd(castLhs, mask), expectedType, "type.check.lhs");
        condRhs = builder.CreateICmpNE(builder.CreateAnd(castRhs, mask), expectedType, "type.check.rhs");
    }
    else {
        condLhs = builder.CreateICmpEQ(builder.CreateAnd(castLhs, mask), expectedType, "type.check.lhs");
        condRhs = builder.CreateICmpEQ(builder.CreateAnd(castRhs, mask), expectedType, "type.check.rhs");
    }
    llvm::Value* cond = builder.CreateAnd(condLhs, condRhs);
    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(true)});
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::WRONG_TYPE_BINARY),
                                                    castLhs, castRhs, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
}
void Compiler::createArgCountCheck(const string err, llvm::Value* closure, uint8_t expectedArity){
    llvm::Value* decodedClosure = builder.CreateCall(safeGetFunc("decodeObj"), closure, "decoded.closure");
    vector<llvm::Value *> idxList = {builder.getInt32(0), builder.getInt32(1)};
    auto argNumPtr = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], decodedClosure, idxList);
    auto argNum = builder.CreateLoad(builder.getInt8Ty(), argNumPtr);

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    auto errorBB = llvm::BasicBlock::Create(builder.getContext(), "error", F);
    llvm::BasicBlock *callBB = llvm::BasicBlock::Create(builder.getContext(), "call");

    auto cond = builder.CreateICmpEQ(argNum, builder.getInt8(expectedArity));
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, callBB);

    builder.SetInsertPoint(errorBB);
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::ARG_CNT),
                                                    ESLValTo(closure, builder.getInt64Ty()), builder.getInt64(expectedArity), intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), callBB);
    builder.SetInsertPoint(callBB);
}
void Compiler::createArrBoundsCheck(const string err, llvm::Value* arr, llvm::Value* index){
    llvm::Value* decodedArr = builder.CreateCall(safeGetFunc("decodeObj"), arr, "decoded.arr");
    llvm::Value* ptrToSize = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjArray"], decodedArr, 0, 2);
    llvm::Value* upperbound = builder.CreateLoad(builder.getInt32Ty(), ptrToSize, "arr.size");
    upperbound = builder.CreateZExt(upperbound, builder.getInt64Ty());

    auto castIndex = ESLValTo(index, builder.getDoubleTy());
    // Index can be negative
    castIndex = builder.CreateFPToSI(castIndex, builder.getInt64Ty(), "index");
    auto cond = builder.CreateICmpSGE(castIndex, upperbound);
    auto cond2 = builder.CreateICmpSLT(castIndex, builder.getInt64(0));
    cond = builder.CreateOr(cond, cond2, "out.of.bounds");
    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(false)});

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "exec.arr.op");
    builder.CreateCondBr(cond, errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    llvm::Value* castArray = ESLValTo(arr, builder.getInt64Ty());
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::OUT_OF_BOUNDS),
                                                    castArray, castIndex, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
}
// Doesn't actually perform any checks, that is done inside instUnoptGet/Set
void Compiler::createInstNoField(const string err, const string field, llvm::Value* inst){
    llvm::Value* castInst = ESLValTo(inst, builder.getInt64Ty());
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* fieldStr = createConstStr(field);
    // Have to case because runtimeError expects int64 for the 3 args
    fieldStr = llvm::ConstantExpr::getPtrToInt(fieldStr, builder.getInt64Ty());
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::INST_FIELD),
                                                    castInst, fieldStr, intMax});
}
void Compiler::createInstClassCheck(const string err, llvm::Value* inst, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd){
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "no.error");

    llvm::Value* cond = builder.CreateCall(safeGetFunc("isInstAndClass"), {inst, subClassIdxStart, subClassIdxEnd});

    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(true)});
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    llvm::Value* intMax = builder.getInt64(UINT64_MAX);
    llvm::Constant* str = createConstStr(err);
    builder.CreateCall(safeGetFunc("runtimeError"),{str, builder.getInt8(+runtimeErrorType::WRONG_TYPE),
                                                    ESLValTo(inst, builder.getInt64Ty()), intMax, intMax});
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
}

// Codegen functions
llvm::Value* Compiler::codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, Token op){
    debugEmitter.emitNewLocation(builder, op);
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If both are a number go to addNum, if not try adding as string
    llvm::BasicBlock *addNumBB = llvm::BasicBlock::Create(*ctx, "add.num", F);
    llvm::BasicBlock *addStringBB = llvm::BasicBlock::Create(*ctx, "add.string");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    // Call isNum on both values and && the results
    auto isnum = safeGetFunc("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);
    builder.CreateCondBr(builder.CreateAnd(c1, c2), addNumBB, addStringBB);

    // If both values are numbers, add them and go to mergeBB
    builder.SetInsertPoint(addNumBB);
    auto castlhs = ESLValTo(lhs, builder.getDoubleTy());
    auto castrhs = ESLValTo(rhs, builder.getDoubleTy());
    auto numAddRes = CastToESLVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
    builder.CreateBr(mergeBB);

    // Tries to add lhs and rhs as strings, if it fails throw a type error
    F->insert(F->end(), addStringBB);
    builder.SetInsertPoint(addStringBB);
    createTypeCheckBinary("Addition expects numbers or strings, got '{}' and '{}'.", lhs, rhs,
                          getObjectTypeMasks(object::ObjType::STRING));
    // Type check creates new blocks
    addStringBB = builder.GetInsertBlock();
    auto stringAddRes = builder.CreateCall(safeGetFunc("strAdd"), {lhs, rhs,});
    builder.CreateBr(mergeBB);

    // Final destination for both branches, if both values were numbers or strings(meaning no error was thrown)
    // Use a phi node to determine which one it is and then set it as returnValue
    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(getESLValType(), 2);
    phi->addIncoming(numAddRes, addNumBB);
    phi->addIncoming(stringAddRes, addStringBB);
    return phi;
}
llvm::Value* Compiler::codegenLogicOps(const typedExprPtr expr1, const typedExprPtr expr2, const typedAST::ComparisonOp op){
    bool canOptimize = exprIsType(expr1, expr2, types::getBasicType(types::TypeFlag::BOOL));
    auto castToBool = canOptimize ? safeGetFunc("decodeBool") : safeGetFunc("isTruthy");

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    // Original block is the one we're coming from, both originalBB and evalRhsBB go to mergeBB
    llvm::BasicBlock* originalBB = builder.GetInsertBlock();
    llvm::BasicBlock* evalRhsBB = llvm::BasicBlock::Create(*ctx, "rhs", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    // Cast lhs from val to bool and create a check
    // For 'or' operator if lhs is false we eval rhs
    // For 'and' operator if lhs is true we eval rhs
    llvm::Value* lhs = builder.CreateCall(castToBool, expr1->codegen(this));
    builder.CreateCondBr(op == typedAST::ComparisonOp::OR ? builder.CreateNot(lhs) : lhs, evalRhsBB, mergeBB);

    // If lhs is false(or in the case of 'and' operator true) eval rhs and then go into mergeBB
    builder.SetInsertPoint(evalRhsBB);
    llvm::Value* rhs = builder.CreateCall(castToBool, expr2->codegen(this));
    builder.CreateBr(mergeBB);
    // In case we have a nested 'or' or 'and' evalRhsBB could no longer be the block the builder is emitting to
    evalRhsBB = builder.GetInsertBlock();
    // Insert merge block, code from this point on will be generated into this block
    func->insert(func->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    llvm::PHINode *PN = builder.CreatePHI(builder.getInt1Ty(), 2, op == typedAST::ComparisonOp::OR ? "lortmp" : "landtmp");

    // If we're coming from the originalBB and the operator is 'or' it means that lhs is true, and thus the entire expression is true
    // For 'and' it's the opposite, if lhs is false, then the entire expression is false
    PN->addIncoming(builder.getInt1(op == typedAST::ComparisonOp::OR ? true : false), originalBB);
    // For both operators, if control flow is coming from evalRhsBB rhs becomes the value of the entire expression
    PN->addIncoming(rhs, evalRhsBB);

    // Cast the bool to a Value
    return builder.CreateCall(safeGetFunc("encodeBool"), PN);
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
        lhs = ESLValTo(lhs, builder.getDoubleTy());
        rhs = ESLValTo(rhs, builder.getDoubleTy());

        auto val = neg ? builder.CreateFCmpONE(lhs, rhs, "fcmp.one") : builder.CreateFCmpOEQ(lhs, rhs, "fcmp.oeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }
    else if(exprIsType(expr1, expr2, stringTy)){
        return builder.CreateCall(safeGetFunc("strCmp"), {lhs, rhs});
    }
    else if(!exprIsType(expr1, numTy) && !exprIsType(expr2, numTy) &&
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
    llvm::Value* fcmptmp = builder.CreateFCmpOEQ(ESLValTo(lhs, builder.getDoubleTy()),
                                                 ESLValTo(rhs, builder.getDoubleTy()), "fcmp.tmp");
    if(neg){
        icmptmp = builder.CreateNot(icmptmp, "icmp.neg.tmp");
        fcmptmp = builder.CreateNot(fcmptmp, "fcmp.neg.tmp");
    }
    // To reduce branching on a common operation, select instruction is used
    auto sel = builder.CreateSelect(builder.CreateAnd(c1, c2), fcmptmp, icmptmp);
    llvm::Value* cmpRes = builder.CreateCall(safeGetFunc("encodeBool"), sel);

    // Strings need to be compared using strcmp because some of them are not interned
    // TODO: maybe we should test if calling the function without branching is faster
    llvm::Function* F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *originalBB = builder.GetInsertBlock();
    llvm::BasicBlock *cmpStrBB = llvm::BasicBlock::Create(*ctx, "cmp.str", F);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto sc1 = builder.CreateCall(safeGetFunc("isObjType"), {lhs, builder.getInt8(+object::ObjType::STRING)});
    auto sc2 = builder.CreateCall(safeGetFunc("isObjType"), {rhs, builder.getInt8(+object::ObjType::STRING)});

    builder.CreateCondBr(builder.CreateAnd(sc1, sc2), cmpStrBB, mergeBB);
    // String comparison block
    builder.SetInsertPoint(cmpStrBB);
    llvm::Value* strCmpRes = builder.CreateCall(safeGetFunc("strCmp"), {lhs, rhs});
    builder.CreateBr(mergeBB);

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    // Resulting value depends on the block taken
    llvm::PHINode *PN = builder.CreatePHI(getESLValType(), 2, "cmp.res");
    PN->addIncoming(cmpRes, originalBB);
    PN->addIncoming(strCmpRes, cmpStrBB);
    return PN;
}
llvm::Value* Compiler::codegenNeg(llvm::Value* rhs, const types::tyVarIdx type, typedAST::UnaryOp op, Token dbg){
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If rhs is known to be a number, no need for the type check
    if(typeEnv[type] != types::getBasicType(types::TypeFlag::NUMBER)){
        string err = fmt::format("Operator '{}' expects a number, got '{}'.", dbg.getLexeme(), "{}");
        createTypeCheckUnary(err, rhs, getNumberTypeMasks());
    }
    // For binary negation, the casting is as follows Value -> double -> int64 -> double -> Value
    if(op == typedAST::UnaryOp::BIN_NEG){
        // Cast value to double, then convert to signed 64bit integer and negate
        auto tmp = ESLValTo(rhs, builder.getDoubleTy());
        auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"bin.neg.tmp");
        // Cast back to double and then to 64bit int
        auto castToDouble = builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx));
        return CastToESLVal(castToDouble);
    }else{
        auto tmp = ESLValTo(rhs, builder.getDoubleTy());
        return CastToESLVal(builder.CreateFNeg(tmp, "fneg.tmp"));
    }
}
void Compiler::codegenBlock(const typedAST::Block& block){
    for(auto stmt : block.stmts){
        stmt->codegen(this);
    }
}
llvm::Value * Compiler::codegenIncrement(const typedAST::UnaryOp op, const typedExprPtr expr, const Token dbg) {
    // No array/hashmap field access because it's too complicated
    if(expr->type == typedAST::NodeType::VAR_READ){
        return codegenVarIncrement(op, std::reinterpret_pointer_cast<typedAST::VarRead>(expr), dbg);
    }else if(expr->type == typedAST::NodeType::INST_GET){
        return codegenInstIncrement(op, std::reinterpret_pointer_cast<typedAST::InstGet>(expr), dbg);
    }
    // TODO: error
    errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
}
// Reuses var read and var store
llvm::Value * Compiler::codegenVarIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::VarRead> expr, Token dbg) {
    llvm::Value* val = codegenVarRead(expr->varPtr);
    debugEmitter.emitNewLocation(builder, dbg);
    // Right now we can only increment numbers, maybe change this when adding iterators?
    if(!exprIsType(expr, types::getBasicType(types::TypeFlag::NUMBER))){
        string err = fmt::format("Operator '{}' expects a number, but got '{}'.", dbg.getLexeme(), "{}");
        createTypeCheckUnary(err, val, getNumberTypeMasks());
    }
    llvm::Value* res = ESLValTo(val, builder.getDoubleTy());

    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    res = CastToESLVal(res);
    codegenVarStore(expr->varPtr, res);
    return val;
}
llvm::Value * Compiler::codegenInstIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::InstGet> expr, Token dbg) {
    auto inst = expr->instance->codegen(this);

    llvm::Value* fieldPtr = nullptr;
    // If type of instance if known optimize getting pointer to field
    if(exprIsType(expr->instance, types::getBasicType(types::TypeFlag::INSTANCE))) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        fieldPtr = getOptInstFieldPtr(inst, klass, expr->field);
    }else{
        fieldPtr = getUnoptInstFieldPtr(inst, expr->field, expr->dbgInfo.field);
    }
    llvm::Value* storedField = builder.CreateLoad(getESLValType(), fieldPtr);
    // Set debug to operator after getting field for more correct error messages
    debugEmitter.emitNewLocation(builder, dbg);
    string err = fmt::format("Operator '{}' expects a number, field '{}' is '{}'.", dbg.getLexeme(), expr->field, "{}");
    createTypeCheckUnary(err, storedField, getNumberTypeMasks());

    llvm::Value* res = ESLValTo(storedField, builder.getDoubleTy());
    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));

    res = CastToESLVal(res);
    builder.CreateStore(res, fieldPtr);
    return storedField;
}

// Function codegen helpers
llvm::Function* Compiler::startFuncDef(const string name, const std::shared_ptr<types::FunctionType> fnTy, Token& loc){
    llvm::Function* fn = functions[fnTy];
    fn->setName(name);
    debugEmitter.addNewFunc(builder, fn, *fnTy, loc);

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    return fn;
}
llvm::FunctionType* Compiler::getFuncType(int argCount){
    vector<llvm::Type*> params = {builder.getPtrTy()};
    // First argument is always the closure structure;
    for(int i = 0; i < argCount+1; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    return fty;
}
void Compiler::declareFuncArgs(const vector<std::shared_ptr<typedAST::VarDecl>>& args){
    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    // First argument is ALWAYS the thread data ptr, and second is obj closure ptr
    int argIndex = 1;
    for (auto var : args) {
        llvm::Value* varPtr;
        // Don't need to use temp builder when creating alloca since this happens in the first basicblock of the function
        if(var->varType == typedAST::VarType::LOCAL){
            varPtr = builder.CreateAlloca(getESLValType(), nullptr, var->dbgInfo.varName.getLexeme());
            builder.CreateStore(inProgressFuncs.top().fn->getArg(argIndex++), varPtr);
        }else{
            size_t freevarSize = curModule->getDataLayout().getTypeAllocSize(namedTypes["ObjFreevar"]);
            varPtr = builder.CreateCall(safeGetFunc("gcAlloc"), {builder.getInt64(freevarSize)}, var->dbgInfo.varName.getLexeme());
            llvm::Value* objType = builder.CreateInBoundsGEP(namedTypes["Obj"], varPtr, {builder.getInt32(0), builder.getInt32(1)});
            builder.CreateStore(builder.getInt8(+object::ObjType::FREEVAR), objType);
            // first index: access to the structure that's being pointed to,
            // second index: access to the second field(64bit field for the value)
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], varPtr, idxList, "freevar.addr");
            builder.CreateStore(inProgressFuncs.top().fn->getArg(argIndex++), tmpEle);
        }
        debugEmitter.addLocalVarDecl(builder, varPtr, var->dbgInfo.varName, true, argIndex);
        // Insert the argument into the pool of variables
        variables.insert_or_assign(var->uuid, varPtr);
    }
}

llvm::FunctionCallee Compiler::setupUnoptCall(llvm::Value* closureVal, int argc, Token dbg){
    createTypeCheckUnary("Expected a function for a callee, got '{}'.", closureVal, getObjectTypeMasks(object::ObjType::CLOSURE));

    // argc-2
    string err = fmt::format("Function being called with {} arguments when it accepts {}.", "{}", argc-2);
    createArgCountCheck(err, closureVal, argc-2);
    auto closurePtr = builder.CreateCall(safeGetFunc("decodeClosure"), closureVal);
    return getBitcastFunc(closurePtr, argc-2);
}

void Compiler::createRuntimeFuncArgCheck(llvm::Value* objClosurePtr, size_t argSize, Token dbg){
    vector<llvm::Value *> idxList = {builder.getInt32(0), builder.getInt32(1)};
    auto argNumPtr = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], objClosurePtr, idxList);
    auto argNum = builder.CreateLoad(builder.getInt8Ty(), argNumPtr);

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    auto errorBB = llvm::BasicBlock::Create(builder.getContext(), "error", F);
    llvm::BasicBlock *callBB = llvm::BasicBlock::Create(builder.getContext(), "call");

    auto cond = builder.CreateICmpEQ(argNum, builder.getInt8(argSize));
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, callBB);

    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    argCntError(dbg, argNum, argSize);
    builder.CreateUnreachable();

    F->insert(F->end(), callBB);
    builder.SetInsertPoint(callBB);
}

// closurePtr is detagged closure object
llvm::FunctionCallee Compiler::getBitcastFunc(llvm::Value* closurePtr, const int argc){
    // Index for accessing the fn ptr is at 3, not 2, because we have a Obj struct at index 0
    vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(3)};
    auto fnPtrAddr = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], closurePtr, idxList);
    llvm::Value* fnPtr = builder.CreateLoad(builder.getPtrTy(), fnPtrAddr);
    auto fnTy = getFuncType(argc);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    fnPtr = builder.CreateBitCast(fnPtr, fnPtrTy);
    return llvm::FunctionCallee(fnTy, fnPtr);
}

llvm::Value* Compiler::decoupleSetOperation(llvm::Value* storedVal, llvm::Value* newVal, typedAST::SetType opTy, Token dbg){
    auto num1 = ESLValTo(storedVal, builder.getDoubleTy());
    auto num2 = ESLValTo(newVal, builder.getDoubleTy());
    switch(opTy){
        case typedAST::SetType::ADD_SET:
            return builder.CreateFAdd(num1, num2);
        case typedAST::SetType::SUB_SET:
            return builder.CreateFSub(num1, num2);
        case typedAST::SetType::MUL_SET:
            return builder.CreateFMul(num1, num2);
        case typedAST::SetType::DIV_SET:
            return builder.CreateFDiv(num1, num2);
        case typedAST::SetType::REM_SET:
            return builder.CreateFRem(num1, num2);
        case typedAST::SetType::AND_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateAnd(num1, num2), builder.getDoubleTy());
        case typedAST::SetType::OR_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateOr(num1, num2), builder.getDoubleTy());
        case typedAST::SetType::XOR_SET:
            num1 = builder.CreateFPToUI(num1, builder.getInt64Ty());
            num2 = builder.CreateFPToUI(num2, builder.getInt64Ty());
            return builder.CreateUIToFP(builder.CreateXor(num1, num2), builder.getDoubleTy());
        default: __builtin_unreachable();
    }
    // This will never be hit
}

llvm::Value* Compiler::getArrElement(llvm::Value* arr, llvm::Value* field, bool opt, Token dbg){
    if(!opt) createTypeCheckUnary("Array accessor must be a number, got '{}'.", field, getNumberTypeMasks());
    // Check the index first because we need the untagged version of the index for error reporting
    createArrBoundsCheck("Index {} outside of array range, array size is {}.", arr, field);
    field = ESLValTo(field, builder.getDoubleTy());
    field = builder.CreateFPToSI(field, builder.getInt64Ty());


    arr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjArray"], arr, 0, 3);
    arr = builder.CreateLoad(namedTypes["ObjArrayStoragePtr"], storagePtr, "storage.ptr");
    arr = builder.CreateConstInBoundsGEP1_32(namedTypes["ObjArrayStorage"], arr, 1, "data.ptr");
    auto ptr = builder.CreateGEP(getESLValType(), arr, field, "item.ptr");
    return builder.CreateLoad(getESLValType(), ptr, "arr.elem");
}

llvm::Value* Compiler::getMapElement(llvm::Value* map, llvm::Value* field, bool opt, Token dbg){
    if(!opt) createTypeCheckUnary("Map accessor must be a string, got '{}'.", field,
                                  getObjectTypeMasks(object::ObjType::STRING));

    map = builder.CreateCall(safeGetFunc("decodeObj"), map);
    field = builder.CreateCall(safeGetFunc("decodeObj"), field);
    return builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field}, "map.elem");
}

llvm::Value* Compiler::setArrElement(llvm::Value* arr, llvm::Value* index, llvm::Value* val, bool optIdx, bool optVal,
                                     typedAST::SetType opTy, Token dbg){
    if(!optIdx) createTypeCheckUnary("Array accessor must be a number, got '{}'.", index, getNumberTypeMasks());

    createArrBoundsCheck("Index {} outside of array range, array size is {}.", arr, index);
    index = ESLValTo(index, builder.getDoubleTy());
    index = builder.CreateFPToUI(index, builder.getInt64Ty());
    arr = builder.CreateCall(safeGetFunc("decodeArray"), arr, "obj.arr.ptr");
    builder.CreateCall(safeGetFunc("arrWriteBarrier"), {arr, val}, "barrier");
    llvm::Value* storagePtr = builder.CreateConstInBoundsGEP2_32(namedTypes["ObjArray"], arr, 0, 3);
    arr = builder.CreateLoad(namedTypes["ObjArrayStoragePtr"], storagePtr, "storage.ptr");
    arr = builder.CreateConstInBoundsGEP1_32(namedTypes["ObjArrayStorage"], arr, 1, "data.ptr");
    llvm::Value* ptr = builder.CreateGEP(getESLValType(), arr, index);

    if(opTy == typedAST::SetType::SET){
        builder.CreateStore(val, ptr);
        return val;
    }else if(opTy == typedAST::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateLoad(getESLValType(), ptr);
        val = codegenBinaryAdd(storedVal, val, dbg);
        builder.CreateStore(val, ptr);
        return val;
    }
    auto storedVal = builder.CreateLoad(getESLValType(), ptr);
    if(!optVal) {
        createTypeCheckBinary("Operator expects numbers, array element is '{}', rhs is '{}'.", storedVal, val, getNumberTypeMasks());
    }else{
        createTypeCheckUnary("Operator expects numbers, array element is '{}'.", storedVal, getNumberTypeMasks());
    }

    val = CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
    builder.CreateStore(val, ptr);
    return val;
}
llvm::Value* Compiler::setMapElement(llvm::Value* map, llvm::Value* field, llvm::Value* val, bool optIdx, bool optVal,
                                     typedAST::SetType opTy, Token dbg){
    if(!optIdx) createTypeCheckUnary("Map accessor must be a string, got '{}'.", field,
                                  getObjectTypeMasks(object::ObjType::STRING));

    map = builder.CreateCall(safeGetFunc("decodeObj"), map);
    field = builder.CreateCall(safeGetFunc("decodeObj"), field);

    if(opTy == typedAST::SetType::SET){
        builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
        return val;
    }else if(opTy == typedAST::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
        val = codegenBinaryAdd(storedVal, val, dbg);
        builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
        return val;
    }
    auto storedVal = builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
    if(!optVal) {
        createTypeCheckBinary("Operator expects numbers, array element is '{}', rhs is '{}'.", storedVal, val, getNumberTypeMasks());
    }else{
        createTypeCheckUnary("Operator expects numbers, array element is '{}'.", storedVal, getNumberTypeMasks());
    }

    val = CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
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
        llvm::Value* val = createConstant(c.first);
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
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, fnName, curModule.get());

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    llvm::Value* idx = builder.getInt32(-1);
    for(auto& p : collection){
        auto toCmp = createESLString(p.first);
        auto cmp = builder.CreateICmpEQ(fn->getArg(0), toCmp);
        idx = builder.CreateSelect(cmp, builder.getInt32(p.second.second), idx);
    }
    builder.CreateRet(idx);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}
void Compiler::codegenMethod(string classname, typedAST::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd){
    llvm::Function* methodFn = functions[method.code->fnTy];
    inProgressFuncs.emplace(methodFn);
    debugEmitter.addNewFunc(builder, methodFn, *method.code->fnTy, method.dbg.name);
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", inProgressFuncs.top().fn);
    builder.SetInsertPoint(BB);
    declareFuncArgs(method.code->args);
    createInstClassCheck(fmt::format("Expected instance of class '{}', got '{}'.", classname, "{}"),
                         inProgressFuncs.top().fn->getArg(1), subClassIdxStart, subClassIdxEnd);

    for(auto s : method.code->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = inProgressFuncs.top();
    inProgressFuncs.pop();
    debugEmitter.popScope(builder, method.dbg.keyword);

    // Set insertion point to the end of the enclosing method
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
}

llvm::Constant* Compiler::createMethodObj(typedAST::ClassMethod& method, llvm::Function* methodPtr){
    // Every function is converted to a closure(if even it has 0 freevars) for ease of use when calling
    // Methods can't have freevars
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(methodPtr, builder.getPtrTy());
    auto arity = builder.getInt8(method.code->args.size());
    auto name = createConstStr(method.code->name);
    auto freeVarCnt = builder.getInt8(0);

    // Create method constant
    llvm::Constant* closure =  llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                     {createConstObjHeader(+object::ObjType::CLOSURE),
                                      arity, freeVarCnt, typeErasedFn, name});
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosureAligned"),
                                     {closure, builder.getInt64(0)});
}
// Creates an instance template with already nulled fields that is memcpy-ed when using new
llvm::GlobalVariable* Compiler::createInstanceTemplate(llvm::Constant* klass, int fieldN){
    vector<llvm::Constant*> fields(fieldN);
    std::fill(fields.begin(), fields.end(), ConstCastToESLVal(builder.getInt64(mask_signature_null)));
    // Template array that is already nulled
    llvm::Constant* fieldArr = llvm::ConstantArray::get(llvm::ArrayType::get(getESLValType(), fieldN), fields);
    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjInstance"), {
            createConstObjHeader(+object::ObjType::INSTANCE), builder.getInt32(fieldN), klass });
    // Struct and array should be one after another without any padding(?)
    auto inst =  llvm::ConstantStruct::get(llvm::StructType::create(*ctx,
                                     {namedTypes["ObjInstance"], llvm::ArrayType::get(getESLValType(), fieldN)}),
                                           {obj, fieldArr});
    return storeConstObj(inst);
}

llvm::Value* Compiler::optimizeInstGet(llvm::Value* inst, string field, Class& klass){
    if(klass.ty->methods.contains(field)){
        // Index into the array of ObjClosure contained in the class
        auto arrIdx = builder.getInt32(klass.ty->methods[field].second);

        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        llvm::Constant* val = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosureAligned"], klass.methodArrPtr, arrIdx);
        return constObjToVal(val, +object::ObjType::CLOSURE);
    }else if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(1)});
        ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        return builder.CreateLoad(getESLValType(), ptr);
    }else{
        // TODO: error
        errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    __builtin_unreachable();
}
llvm::Value* Compiler::instGetUnoptimized(llvm::Value* maybeInst, string fieldName, Token dbg){
    auto [inst, klass] = instGetClassPtr(maybeInst, dbg);
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, fieldName);

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(*ctx, "methods");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx), {{1, fieldBB}, {2, methodBB}},
                         errorBB, {0, 1<<31, 1<<31});

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);

    // Fields are stored just behind the instance
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst, {builder.getInt32(1)});
    // Loads the field
    ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(getESLValType(), ptr);
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);
    // Methods are just behind class
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(1)});
    llvm::Value* val = builder.CreateInBoundsGEP(namedTypes["ObjClosureAligned"], ptr, methodIdx);
    auto method = builder.CreateCall(safeGetFunc("encodeObj"), {val, builder.getInt64(+object::ObjType::CLOSURE)});
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);
    createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", fieldName, maybeInst);
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(getESLValType(), 2);
    phi->addIncoming(field, fieldBB);
    phi->addIncoming(method, methodBB);
    return phi;
}
// first el: field index, second el: method index
// Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
std::pair<llvm::Value*, llvm::Value*> Compiler::instGetUnoptIdx(llvm::Value* klass, string field){
    auto fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(6)});
    auto methodFunc = builder.CreateLoad(fnPtrTy, ptr);
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(7)});
    auto fieldsFunc = builder.CreateLoad(fnPtrTy, ptr);

    llvm::Constant* fieldConst = createESLString(field);
    llvm::Value* fieldIdx = builder.CreateCall(fnTy, fieldsFunc, fieldConst);
    llvm::Value* methodIdx = builder.CreateCall(fnTy, methodFunc, fieldConst);
    return std::make_pair(fieldIdx, methodIdx);
}

std::pair<llvm::Value*, llvm::Value*> Compiler::instGetClassPtr(llvm::Value* inst, Token dbg){
    createTypeCheckUnary("Expected an instance, got '{}'.", inst, getObjectTypeMasks(object::ObjType::INSTANCE));

    inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
    inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                         {builder.getInt32(0), builder.getInt32(2)});
    auto klass = builder.CreateLoad(namedTypes["ObjClassPtr"], ptr);
    return std::make_pair(inst, klass);
}

llvm::Value* Compiler::instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx){
    auto cmp1 = builder.CreateAnd(fieldIdx, builder.getInt32(1u << 31u));
    auto cmp2 = builder.CreateAnd(methodIdx, builder.getInt32(1u << 30u));
    auto dest = builder.CreateOr(cmp1, cmp2);
    dest = builder.CreateLShr(dest, 30, "res");
    return dest;
}

llvm::Value* Compiler::getOptInstFieldPtr(llvm::Value* inst, Class& klass, string field){
    if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(1)});
        ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        return ptr;
    }else{
        // TODO: error
        errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    // Unreachable (at least it should be)
    __builtin_unreachable();
}

llvm::Value* Compiler::getUnoptInstFieldPtr(llvm::Value* maybeInst, string field, Token dbg){
    auto [inst, klass] = instGetClassPtr(maybeInst, dbg);

    // Gets the function which determines index of field given a string
    llvm::FunctionType* fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    llvm::Value* ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(7)});
    auto fieldsFunc = builder.CreateLoad(fnPtrTy, ptr);

    llvm::Value* fieldIdx = builder.CreateCall(fnTy, fieldsFunc, createESLString(field));

    auto cmp = builder.CreateICmpEQ(fieldIdx, builder.getInt32(-1));

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    builder.CreateCondBr(cmp, errorBB, fieldBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);
    createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", field, maybeInst);
    builder.CreateUnreachable();

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);
    // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(1)});
    ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);

    return ptr;
}
// TODO: there should be errors if num of params passed and argc(if known) doesn't match
// Modifies callArgs to have correct args
llvm::FunctionCallee Compiler::optimizeInvoke(llvm::Value* inst, string field, Class& klass, vector<llvm::Value*>& callArgs, Token dbg){
    if(klass.ty->methods.contains(field)){
        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        auto closure = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosureAligned"], klass.methodArrPtr,
                                                                    builder.getInt32(klass.ty->methods[field].second));
        closure = llvm::ConstantExpr::getBitCast(closure, builder.getPtrTy());
        // Closures in class are stored as raw pointers, tag them and then pass to method
        closure = constObjToVal(closure, +object::ObjType::CLOSURE);
        llvm::Function* fn = functions[typeEnv[klass.ty->methods[field].first]];
        // Insert at begin + 1 to skip the thread data ptr
        callArgs.insert(callArgs.begin(), {closure, inst});
        return fn;
    }else if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(1)});
        llvm::Value* fieldPtr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        llvm::Value* closure = builder.CreateLoad(getESLValType(), fieldPtr);
        // Skip thread data ptr that is passed to every function
        callArgs.insert(callArgs.begin(), closure);
        // Arg check doesn't take closure into account
        return setupUnoptCall(closure, callArgs.size(), dbg);
    }else{
        // TODO: error since we're invoking a method/field that doesnt exist
        errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    }
    __builtin_unreachable();
}

llvm::Value* Compiler::unoptimizedInvoke(llvm::Value* encodedInst, string fieldName, vector<llvm::Value*> args, Token dbg){
    auto [inst, klass] = instGetClassPtr(encodedInst, dbg);
    auto [fieldIdx, methodIdx] = instGetUnoptIdx(klass, fieldName);

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(*ctx, "methods");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    createWeightedSwitch(instGetIdxType(fieldIdx, methodIdx), {{1, fieldBB}, {2, methodBB}},
                         errorBB, {0, 1<<31, 1<<31});

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);
    // Fields are stored just behind instance, using GEP with idx 1 points just past the object and into the start of the fields
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(1)});
    auto fieldPtr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(getESLValType(), fieldPtr);
    // Erases closure from args list because they are needed for method calling below
    // Skip over thread data ptr that is passed as the first arg
    args.insert(args.begin(), field);
    llvm::CallInst* callres1 = builder.CreateCall(setupUnoptCall(field, args.size(), dbg),  args);
    args.erase(args.begin());
    fieldBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);
    // First load the ObjClosurePtr(we treat the offset into the ObjClosure array that the class has as a standalone pointer to that closure)
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(1)});
    llvm::Value* val = builder.CreateInBoundsGEP(namedTypes["ObjClosureAligned"], ptr, methodIdx);
    // Since this is a raw ObjClosure pointer we tag it first
    auto method = builder.CreateCall(safeGetFunc("encodeObj"), {val, builder.getInt64(+object::ObjType::CLOSURE)});
    // Skip over thread data ptr that is passed as the first arg
    args.insert(args.begin(), {method, encodedInst});
    llvm::CallInst* callres2 = builder.CreateCall(setupUnoptCall(method, args.size(), dbg), args);
    methodBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);
    createInstNoField("Instance of type '{}' doesn't contain field or method '{}'", fieldName, encodedInst);
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(getESLValType(), 2);
    phi->addIncoming(callres1, fieldBB);
    phi->addIncoming(callres2, methodBB);
    return phi;
}

// Multithreading
llvm::Function* Compiler::createThreadWrapper(llvm::FunctionType* funcType, int numArgs){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getPtrTy(), {builder.getPtrTy()}, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, "threadWrapper", curModule.get());
    fn->addFnAttr("uwtable", "sync");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    // Loads arguments from stack of the thread that called spawn
    vector<llvm::Value*>args;
    for(int i = 0; i < numArgs; i++){
        llvm::Value* gep = builder.CreateConstInBoundsGEP1_32(getESLValType(), fn->getArg(0), i + 1);
        args.push_back(builder.CreateLoad(getESLValType(), gep));
    }
    // Loads the function pointer from the first element and then atomically change it to 0 since it acts as a flag
    llvm::Value* funcPtr = builder.CreateLoad(builder.getPtrTy(), fn->getArg(0));
    llvm::StoreInst* store = builder.CreateStore(builder.getInt64(0), fn->getArg(0));
    store->setAtomic(llvm::AtomicOrdering::Release);
    store->setAlignment(llvm::Align(8));

    llvm::Value* frameAddr = builder.CreateIntrinsic(builder.getPtrTy(), llvm::Intrinsic::frameaddress, {builder.getInt32(0)});
    builder.CreateCall(safeGetFunc("threadInit"), {frameAddr});

    builder.CreateCall(funcType, funcPtr, args);
    builder.CreateCall(safeGetFunc("threadDestruct"));
    builder.CreateRet(llvm::ConstantPointerNull::get(builder.getPtrTy()));
    llvm::verifyFunction(*fn);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}

void Compiler::setupThreadCreation(llvm::FunctionCallee callee, vector<llvm::Value*>& args){
    // args.size + 1 because first element is used as an atomic flag
    llvm::AllocaInst* alloca = builder.CreateAlloca(builder.getInt64Ty(), builder.getInt32(args.size()+1), "args");
    for(int i = 0; i < args.size(); i++){
        llvm::Value* ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt64Ty(), alloca, i+1, fmt::format("{}.arg", i));
        builder.CreateStore(args[i], ptr);
    }
    // Store pointer to func in first slot of alloca
    builder.CreateStore(callee.getCallee(), alloca);
    llvm::Function* wrapper = createThreadWrapper(callee.getFunctionType(), args.size());
    // C++ function that actually calls pthread_create and also does cleanup when a thread dies
    builder.CreateCall(safeGetFunc("createNewThread"), {wrapper, alloca});

    // Spinlock
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* header = llvm::BasicBlock::Create(*ctx, "spinlock.header", F);
    llvm::BasicBlock* next = llvm::BasicBlock::Create(*ctx, "spinlock.done", F);
    builder.CreateBr(header);
    builder.SetInsertPoint(header);
    // First slot in alloca is a flag that is reset atomically by the spawned thread
    llvm::LoadInst* flag = builder.CreateLoad(builder.getInt64Ty(), alloca);
    flag->setOrdering(llvm::AtomicOrdering::Acquire);
    flag->setAlignment(llvm::Align(8));

    llvm::Value* cmp = builder.CreateICmpEQ(flag, builder.getInt64(0), "lock.cond");
    builder.CreateCondBr(cmp, next, header);
    builder.SetInsertPoint(next);
}

// Misc
llvm::Constant* Compiler::createConstStr(const string& str){
    if(CStrings.contains(str)) return CStrings[str];
    auto constant = builder.CreateGlobalStringPtr(str, "internal.string", 0, curModule.get());

    CStrings[str] = constant;
    return constant;
}

llvm::Constant* Compiler::createESLString(const string& str){
    if(ESLStrings.contains(str)) return ESLStrings[str];
    auto obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjString"), {
        createConstObjHeader(+object::ObjType::STRING), builder.getInt32(str.size()), createConstStr(str)});
    auto val = constObjToVal(storeConstObj(obj), +object::ObjType::STRING);
    ESLStrings[str] = val;
    return val;
}

llvm::Function* Compiler::safeGetFunc(const string& name){
    auto* fn = curModule->getFunction(name);
    if(!fn){
        std::cerr<<fmt::format("Function {} hasn't been created yet.\n", name);
        exit(64);
    }
    return fn;
}

void Compiler::argCntError(Token token, llvm::Value* expected, const int got){
    llvm::Constant* str = createConstStr(fmt::format("Expected %d arguments but got {}.", got));
    llvm::Constant* file = createConstStr(token.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(token.str.computeLine());

    builder.CreateCall(safeGetFunc("printf"), {str, expected});
    builder.CreateCall(safeGetFunc("exit"), builder.getInt32(64));
}

// Returns i64 constant
llvm::Constant* Compiler::createConstant(std::variant<double, bool, void*,string>& constant){
    switch(constant.index()){
        case 0: {
            auto tmp = llvm::ConstantFP::get(*ctx, llvm::APFloat(get<double>(constant)));
            return llvm::ConstantExpr::getBitCast(tmp, builder.getInt64Ty());
        }
        case 1: {
            uInt64 val = get<bool>(constant) ? mask_signature_true : mask_signature_false;
            return builder.getInt64(val);
        }
        case 2: return builder.getInt64(mask_signature_null);
        case 3: {
            return ESLConstTo(createESLString(get<string>(constant)), builder.getInt64Ty());
        }
    }
    errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::GlobalVariable* Compiler::storeConstObj(llvm::Constant* obj){
    auto gv =  new llvm::GlobalVariable(*curModule, obj->getType(), true,
                                    llvm::GlobalVariable::LinkageTypes::PrivateLinkage, obj, "internal.const.obj");
    gv->setAlignment(llvm::Align(16));
    return gv;
}
llvm::Constant* Compiler::createConstObjHeader(int type){
    // 128 is a magic constant that tells the gc that this is a constant object
    auto padding = llvm::ConstantArray::get(llvm::ArrayType::get(builder.getInt8Ty(), 2),{builder.getInt8(3), builder.getInt8(0)});
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "Obj"),{padding, builder.getInt8(type)});
}

llvm::Constant* Compiler::constObjToVal(llvm::Constant* obj, uint8_t type){
    auto val = llvm::ConstantExpr::getPtrToInt(obj, builder.getInt64Ty());
    return ConstCastToESLVal(llvm::ConstantExpr::getAdd(val, builder.getInt64(mask_signature_obj | type)));
}

llvm::Value* Compiler::codegenVarRead(std::shared_ptr<typedAST::VarDecl> varPtr){
    switch(varPtr->varType){
        case typedAST::VarType::LOCAL:{
            return builder.CreateLoad(getESLValType(), variables.at(varPtr->uuid), "load.local");
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* upvalPtr = variables.at(varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], upvalPtr, idxList, "freevar.addr");
            return builder.CreateLoad(getESLValType(), tmpEle, "load.freevar");
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:{
            return builder.CreateLoad(getESLValType(), variables.at(varPtr->uuid), "load.gvar");
        }
    }
    errHandler.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}

llvm::Value* Compiler::codegenVarStore(std::shared_ptr<typedAST::VarDecl> varPtr, llvm::Value* toStore){
    switch(varPtr->varType){
        case typedAST::VarType::LOCAL:{
            builder.CreateStore(toStore, variables.at(varPtr->uuid));
            break;
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* freevarPtr = variables.at(varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the ref to the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], freevarPtr, idxList, "freevar.addr");
            builder.CreateStore(toStore, tmpEle);
            break;
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:{
            builder.CreateStore(toStore, variables.at(varPtr->uuid));
            break;
        }
    }
    return toStore;
}

void Compiler::generateNativeFuncs(fastMap<string, types::tyVarIdx>& natives){
    auto addNativeFn = [&](string name, int argc, types::tyPtr type){
        // Every function is declared in generateNativeFuncs, natives need to fix up the linkage
        llvm::Function* func = functions[type];
        func->setLinkage(llvm::Function::ExternalLinkage);
        func->setName(name);

        auto typeErasedFn = llvm::ConstantExpr::getBitCast(func, builder.getPtrTy());
        auto arity = builder.getInt8(argc);
        auto cname = createConstStr(name);
        auto freeVarCnt = builder.getInt8(0);

        // Create function constant
        llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                                        {createConstObjHeader(+object::ObjType::CLOSURE),
                                                         arity, freeVarCnt, typeErasedFn, cname});
        // Creates a place in memory for the function and stores it there
        llvm::Constant* fnLoc = storeConstObj(fnC);
        return constObjToVal(fnLoc, +object::ObjType::CLOSURE);
    };
    for(std::pair<string, types::tyVarIdx> p : natives){
        auto fnTy = std::reinterpret_pointer_cast<types::FunctionType>(typeEnv[p.second]);
        nativeFunctions[p.first] = addNativeFn(p.first, fnTy->argCount, fnTy);
    }
}
// Assumes first weight is for default case
void Compiler::createWeightedSwitch(llvm::Value* cond, vector<std::pair<int, llvm::BasicBlock*>> cases, llvm::BasicBlock* defaultBB, vector<int> weights){
    auto sw = builder.CreateSwitch(cond, defaultBB);
    for(auto [_case, BB] : cases){
        sw->addCase(builder.getInt32(_case), BB);
    }
    // Convert weights to LLVM constants
    std::vector<llvm::Metadata*> Vals;
    Vals.push_back(llvm::MDString::get(*ctx, "branch_weights"));
    for (int w : weights) {
        Vals.push_back(llvm::ConstantAsMetadata::get(builder.getInt32(w)));
    }
    // Create the metadata node
    llvm::MDNode* WeightsNode = llvm::MDNode::get(*ctx, Vals);

    // Add the metadata to the switch instruction
    sw->setMetadata(llvm::LLVMContext::MD_prof, WeightsNode);
}

// All of these functions are noops but are needed because of llvms type system
llvm::Value* Compiler::ESLValTo(llvm::Value* val, llvm::Type* ty){
    if(ty->isPointerTy()){
        return builder.CreateBitCast(val, ty);
    }else{
        auto i64Representation = builder.CreatePtrToInt(val, builder.getInt64Ty());
        return builder.CreateBitCast(i64Representation, ty);
    }
}
llvm::Constant* Compiler::ESLConstTo(llvm::Constant* constant, llvm::Type* ty){
    if(ty->isPointerTy()){
        return llvm::ConstantExpr::getBitCast(constant, ty);
    }else{
        auto i64Representation = llvm::ConstantExpr::getPtrToInt(constant, builder.getInt64Ty());
        return llvm::ConstantExpr::getBitCast(i64Representation, ty);
    }
}

llvm::Value* Compiler::CastToESLVal(llvm::Value* val){
    if(val->getType()->isPointerTy()){
        return builder.CreateBitCast(val, getESLValType());
    }else{
        auto i64Representation = builder.CreateBitCast(val, builder.getInt64Ty());
        return builder.CreateIntToPtr(i64Representation, getESLValType());
    }
}

llvm::Constant* Compiler::ConstCastToESLVal(llvm::Constant* constant){
    if(constant->getType()->isPointerTy()){
        return llvm::ConstantExpr::getBitCast(constant, getESLValType());
    }else{
        return llvm::ConstantExpr::getIntToPtr(constant, getESLValType());
    }
}

llvm::Type* Compiler::getESLValType(){
    return llvmHelpers::getESLValType(*ctx);
}
#pragma endregion
