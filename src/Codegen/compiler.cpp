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

#include <unordered_set>
#include <iostream>

using namespace compileCore;

Compiler::Compiler(std::shared_ptr<typedAST::Function> _code, vector<File*>& _srcFiles, vector<types::tyPtr>& _tyEnv,
                   fastMap<string, std::pair<int, int>>& _classHierarchy, fastMap<string, types::tyVarIdx>& natives)
    : ctx(std::make_unique<llvm::LLVMContext>()), builder(llvm::IRBuilder<>(*ctx)) {
    sourceFiles = _srcFiles;
    typeEnv = _tyEnv;
    classHierarchy = _classHierarchy;

    curModule = std::make_unique<llvm::Module>("Module", *ctx);

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    auto err = llvm::orc::KaleidoscopeJIT::Create().moveInto(JIT);
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    curModule->setDataLayout(JIT->getDataLayout());
    curModule->setTargetTriple(targetTriple);

    curModule->getOrInsertGlobal("gcFlag", builder.getInt8Ty());
    llvm::GlobalVariable* gvar = curModule->getNamedGlobal("gcFlag");
    gvar->setLinkage(llvm::GlobalVariable::PrivateLinkage);
    gvar->setInitializer(builder.getInt8(0));
    llvmHelpers::addHelperFunctionsToModule(curModule, ctx, builder, namedTypes);
    implementNativeFunctions(natives);

    compile(_code);
    llvmHelpers::runModule(curModule, JIT, ctx, true);
}

void Compiler::compile(std::shared_ptr<typedAST::Function> _code){
    llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx), false);
    auto tmpfn = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "func.main", curModule.get());
    tmpfn->setGC("statepoint-example");
    tmpfn->addFnAttr("frame-pointer", "all");
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmpfn);
    builder.SetInsertPoint(BB);
    inProgressFuncs.emplace(tmpfn);
    try {
        for (auto stmt: _code->block.stmts) {
            stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
        }
    }catch(CompilerError err){
        std::cout<<fmt::format("Compiler exited because of error: '{}'.", err.reason);
    }
    // Get all string constants into gc
    llvm::IRBuilder<> tempBuilder(*ctx);
    tempBuilder.SetInsertPointPastAllocas(tmpfn);
    tempBuilder.CreateCall(safeGetFunc("gcInit"), {curModule->getNamedGlobal("gcFlag")});
    for(auto strObj : ESLStrings){
        tempBuilder.CreateCall(safeGetFunc("gcInternStr"), {strObj.second});
    }

    // Ends the main function
    builder.CreateRetVoid();
    llvm::verifyFunction(*tmpfn);
#ifdef COMPILER_DEBUG
    curModule->print(llvm::errs(), nullptr);
#endif
}


llvm::Value* Compiler::visitVarDecl(typedAST::VarDecl* decl) {
    switch(decl->varType){
        case typedAST::VarType::LOCAL:{
            // Alloca at the beginning of the function to make use of mem2reg pass
            llvm::IRBuilder<> tempBuilder(*ctx);
            tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
            auto tmp = tempBuilder.CreateAlloca(getESLValType(), nullptr, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);
            break;
        }
        case typedAST::VarType::FREEVAR:{
            llvm::IRBuilder<> tempBuilder(*ctx);
            tempBuilder.SetInsertPointPastAllocas(inProgressFuncs.top().fn);
            // Creates a heap allocated free variable
            auto tmp = tempBuilder.CreateCall(safeGetFunc("createFreevar"), std::nullopt, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);

            break;
        }
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL:{
            string varName = decl->dbgInfo.varName.getLexeme() + std::to_string(decl->uuid);
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(*curModule, getESLValType(), false,
                                                                  llvm::GlobalVariable::PrivateLinkage,
                                                                  ConstCastToESLVal(builder.getInt64(MASK_SIGNATURE_NIL)),varName);
            gvar->setAlignment(llvm::Align::Of<Value>());
            // Globals aren't on the stack, so they need to be marked for GC collection separately
            if(decl->varType == typedAST::VarType::GLOBAL) builder.CreateCall(safeGetFunc("addGCRoot"), gvar);
            variables.insert_or_assign(decl->uuid, gvar);
            break;
        }
    }

    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitVarRead(typedAST::VarRead* expr) {
    return codegenVarRead(expr->varPtr);
}
llvm::Value* Compiler::visitVarStore(typedAST::VarStore* expr) {
    llvm::Value* valToStore = expr->toStore->codegen(this);

    return codegenVarStore(expr->varPtr, valToStore);
}
llvm::Value* Compiler::visitVarReadNative(typedAST::VarReadNative* expr) {
    // Since native variables are known at compile time reading them is noop
    return nativeFunctions[expr->nativeName];
}

static bool isFloatingPointOp(typedAST::ArithmeticOp op){
    return op == typedAST::ArithmeticOp::SUB || op == typedAST::ArithmeticOp::MUL ||
           op == typedAST::ArithmeticOp::DIV || op == typedAST::ArithmeticOp::MOD;
}

llvm::Value* Compiler::visitArithmeticExpr(typedAST::ArithmeticExpr* expr) {
    using typedAST::ArithmeticOp;
    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
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
        // If either or both aren't numbers, go to error, otherwise proceed as normal
        createRuntimeTypeCheck(safeGetFunc("isNum"), lhs, rhs, "binopexecute",
                               "Operands must be numbers, got '{}' and '{}'.", expr->dbgInfo.op);
    }

    // Transforms the operands into the required type
    lhs = ESLValTo(lhs, builder.getDoubleTy());
    rhs = ESLValTo(rhs, builder.getDoubleTy());

    // Operations that aren't sub, mul, div and mod require integers, so we convert to ints first
    llvm::Value* ilhs = nullptr;
    llvm::Value* irhs = nullptr;
    if(!isFloatingPointOp(expr->opType)){
        // TODO: this can break if val > 2^63
        ilhs = builder.CreateFPToSI(lhs, builder.getInt64Ty());
        irhs = builder.CreateFPToSI(rhs, builder.getInt64Ty());
    }

    llvm::Value* val = nullptr;
    switch(expr->opType){
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
            val = builder.CreateFDiv(tmp1, tmp2, "floordivtmp");
            break;
        }
        case ArithmeticOp::ADD: assert(false && "Unreachable");
    }

    if(!isFloatingPointOp(expr->opType)) val = builder.CreateSIToFP(val, builder.getDoubleTy());
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

    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        // If either or both aren't numbers, go to error, otherwise proceed as normal
        createRuntimeTypeCheck(safeGetFunc("isNum"), lhs, rhs, "binopexecute",
                               "Operands must be numbers, got '{}' and '{}'.", expr->dbgInfo.op);
    }

    lhs = ESLValTo(lhs, builder.getDoubleTy());
    rhs = ESLValTo(rhs, builder.getDoubleTy());
    llvm::Value* val;

    switch(expr->opType){
        case ComparisonOp::LESS: val = builder.CreateFCmpOLT(lhs, rhs, "olttmp"); break;
        case ComparisonOp::LESSEQ: val = builder.CreateFCmpOLE(lhs, rhs, "oletmp"); break;
        case ComparisonOp::GREAT: val = builder.CreateFCmpOGT(lhs, rhs, "ogttmp"); break;
        case ComparisonOp::GREATEQ: val = builder.CreateFCmpOGE(lhs, rhs, "ogetmp"); break;
        default: assert(false && "Unreachable");
    }
    return builder.CreateCall(safeGetFunc("encodeBool"), val);
}
llvm::Value* Compiler::visitInstanceofExpr(typedAST::InstanceofExpr* expr){
    llvm::Value* inst = expr->lhs->codegen(this);
    auto subclassesInterval = classHierarchy[expr->className];
    return builder.CreateCall(safeGetFunc("isInstAndClass"),
                              {inst, builder.getInt32(subclassesInterval.first), builder.getInt32(subclassesInterval.second)});
}
llvm::Value* Compiler::visitUnaryExpr(typedAST::UnaryExpr* expr) {
    using typedAST::UnaryOp;
    if(expr->opType == UnaryOp::NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        // If type is known to be a bool skip the runtime check and just execute the expr
        if(exprIsType(expr->rhs, types::getBasicType(types::TypeFlag::BOOL))) {
            return CastToESLVal(builder.CreateXor(ESLValTo(rhs, builder.getInt64Ty()), builder.getInt64(MASK_TYPE_TRUE)));
        }

        createRuntimeTypeCheck(safeGetFunc("isBool"), {rhs},
                               "Operand must be a boolean, got '{}'.", "negbool", expr->dbgInfo.op);
        // Quick optimization, instead of decoding bool, negating and encoding, we just XOR with the true flag
        // Since that's just a single bit flag that's flipped on when true and off when false
        // This does rely on the fact that true and false are the first flags and are thus represented with 00 and 01
        return CastToESLVal(builder.CreateXor(ESLValTo(rhs, builder.getInt64Ty()), builder.getInt64(MASK_TYPE_TRUE)));
    }else if(expr->opType == UnaryOp::FNEG || expr->opType == UnaryOp::BIN_NEG){
        return codegenNeg(expr->rhs, expr->opType, expr->dbgInfo.op);
    }else{
        return codegenIncrement(expr->opType, expr->rhs);
    }
}

llvm::Value* Compiler::visitLiteralExpr(typedAST::LiteralExpr* expr) {
    switch(expr->val.index()){
        case 0: {
            auto tmp = llvm::ConstantFP::get(*ctx, llvm::APFloat(get<double>(expr->val)));
            return CastToESLVal(tmp);
        }
        case 1: {
            uInt64 val = get<bool>(expr->val) ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE;
            return CastToESLVal(builder.getInt64(val));
        }
        case 2: return builder.getInt64(MASK_SIGNATURE_NIL);
        case 3: {
            return createESLString(get<string>(expr->val));
        }
    }
    assert(false && "Unreachable");
}
llvm::Value* Compiler::visitHashmapExpr(typedAST::HashmapExpr* expr) {
    vector<llvm::Value*> args;
    args.push_back(builder.getInt32(expr->fields.size()));
    // For each field, compile it and get the constant of the field name
    for (auto entry : expr->fields) {
        // This gets rid of quotes, ""Hello world""->"Hello world"
        args.push_back(builder.CreateCall(safeGetFunc("createStr"), createConstStr(entry.first)));
        args.push_back(entry.second->codegen(this));
    }

    return builder.CreateCall(safeGetFunc("createHashMap"), args);
}
llvm::Value* Compiler::visitArrayExpr(typedAST::ArrayExpr* expr) {
    vector<llvm::Value*> vals;
    for(auto mem : expr->fields){
        vals.push_back(mem->codegen(this));
    }
    auto arrNum = builder.getInt32(vals.size());
    auto arr = builder.CreateCall(safeGetFunc("createArr"), arrNum, "array");
    // I think this should be faster then passing everthing to "createArr", but i could be wrong
    auto arrPtr = builder.CreateCall(safeGetFunc("getArrPtr"), arr, "arrptr");
    for(int i = 0; i < vals.size(); i++){
        auto gep = builder.CreateInBoundsGEP(getESLValType(), arrPtr, builder.getInt32(i));
        builder.CreateStore(vals[i], gep);
    }
    return arr;
}

llvm::Value* Compiler::visitCollectionGet(typedAST::CollectionGet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);

    bool optArrIndex = exprIsType(expr->field, types::getBasicType(types::TypeFlag::NUMBER));
    bool optMapString = exprIsType(expr->field, types::getBasicType(types::TypeFlag::STRING));

    if(exprIsComplexType(expr->collection, types::TypeFlag::ARRAY)){
        return getArrElement(collection, field, optArrIndex, expr->dbgInfo.accessor);

    }else if(exprIsComplexType(expr->collection, types::TypeFlag::HASHMAP)){
        return getMapElement(collection, field, optMapString, expr->dbgInfo.accessor);
    }
    // Uses switch instead of chained comparisons, this should be faster?
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "isArr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "isMap");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    // Have to use i8 otherwise we won't know which function returned true
    llvm::Value* cond1 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::ARRAY)});
    cond1 = builder.CreateZExt(cond1, builder.getInt8Ty());

    llvm::Value* cond2 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::HASH_MAP)});
    cond2 = builder.CreateZExt(cond2,builder.getInt8Ty());
    cond2 = builder.CreateShl(cond2, 1, "shl", true, true);

    auto num = builder.CreateOr(cond1, cond2);
    auto _switch = builder.CreateSwitch(num, errorBB, 3);
    _switch->addCase(builder.getInt8(1), isArray);
    _switch->addCase(builder.getInt8(2), isHashmap);

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
    createTyErr("Expected an array or hashmap, got '{}'.", collection, expr->dbgInfo.accessor);
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(getESLValType(), 2, "collectionget");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;

}

llvm::Value* Compiler::visitCollectionSet(typedAST::CollectionSet* expr) {
    llvm::Value* collection = expr->collection->codegen(this);
    llvm::Value* field = expr->field->codegen(this);
    llvm::Value* val = expr->toStore->codegen(this);

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
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "isArr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "isMap");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    // Have to use i8 otherwise we won't know which function returned true
    llvm::Value* cond1 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::ARRAY)});
    cond1 = builder.CreateZExt(cond1, builder.getInt8Ty());

    llvm::Value* cond2 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::HASH_MAP)});
    cond2 = builder.CreateZExt(cond2,builder.getInt8Ty());
    cond2 = builder.CreateShl(cond2, 1, "shl", true, true);

    auto num = builder.CreateOr(cond1, cond2);
    auto _switch = builder.CreateSwitch(num, errorBB, 3);
    _switch->addCase(builder.getInt8(1), isArray);
    _switch->addCase(builder.getInt8(2), isHashmap);

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
    createTyErr("Expected an array or hashmap, got '{}'.", collection, expr->dbgInfo.accessor);
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);

    auto phi = builder.CreatePHI(getESLValType(), 2, "collectionset");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;
}

llvm::Value* Compiler::visitConditionalExpr(typedAST::ConditionalExpr* expr) {
    auto condtmp = expr->cond->codegen(this);
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

// TODO: there should be errors if num of params passed and argc(if known) doesn't match
llvm::Value* Compiler::visitCallExpr(typedAST::CallExpr* expr) {
    auto val = optimizedFuncCall(expr);
    if(val) return val;

    llvm::Value* closureVal = expr->callee->codegen(this);
    vector<llvm::Value*> args;
    for(auto arg : expr->args) args.push_back(arg->codegen(this));

    return createFuncCall(closureVal, args, expr->dbgInfo.paren1);
}
llvm::Value* Compiler::visitInvokeExpr(typedAST::InvokeExpr* expr) {
    auto inst = expr->inst->codegen(this);

    vector<llvm::Value*> args;
    for(auto arg : expr->args) args.push_back(arg->codegen(this));

    if(typeEnv[expr->inst->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->inst->exprType])->klass->name];
        return optimizeInvoke(inst, expr->field, klass, args, expr->dbgInfo.method);
    }

    auto field = createESLString(expr->field);
    createRuntimeTypeCheck(safeGetFunc("isObjType"), {inst, builder.getInt8(+object::ObjType::INSTANCE)},
                           "isInst", "Expected an instance, got '{}'", expr->dbgInfo.accessor);

    inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
    inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                         {builder.getInt32(0), builder.getInt32(2)});
    auto klass = builder.CreateLoad(namedTypes["ObjClassPtr"], ptr);

    // first elem: field index, second elem: method index
    // Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
    auto indicies = instGetUnoptIdx(klass, field);

    // Creates the switch which returns either method or field
    return unoptimizedInvoke(inst, indicies.first, indicies.second, klass, expr->field, args, expr->dbgInfo.method);
}

llvm::Value* Compiler::visitNewExpr(typedAST::NewExpr* expr) {
    auto& klass = classes[expr->className];
    string name = expr->className.substr(expr->className.rfind(".")+1, expr->className.size()-1);
    // Instead of initializing instance in some runtime function, we request memory, copy the template and adjust pointers
    // benefit of this is the template already has all the fields nulled, so we don't have to do this at every instantiation
    auto instSize = curModule->getDataLayout().getTypeAllocSize(klass.instTemplatePtr->getValueType());
    auto memptr = builder.CreateCall(safeGetFunc("gcAlloc"), {builder.getInt32(instSize)});
    // This flag is used by the gc and needs to be updated since the template will not(and should not) have the same gc flag
    auto flagPtr = builder.CreateInBoundsGEP(namedTypes["Obj"], memptr, {builder.getInt32(0), builder.getInt32(1)});
    auto flag = builder.CreateLoad(namedTypes["Obj"], flagPtr);
    builder.CreateMemCpy(memptr, memptr->getRetAlign(), klass.instTemplatePtr, klass.instTemplatePtr->getAlign(), instSize);
    // Restore flag
    builder.CreateStore(flag, flagPtr);
    // Fixes up pointers
    auto inst = builder.CreateBitCast(memptr, namedTypes["ObjInstancePtr"]);
    // Field arr ptr
    auto address = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst, {builder.getInt32(0), builder.getInt32(3)});
    // Fields are stored in place right after the instance, so a gep that accesses the "second" instance is just the beginning of the fields arr
    auto nextaddr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst, {builder.getInt32(1)});
    nextaddr = builder.CreateBitCast(nextaddr, llvm::PointerType::getUnqual(getESLValType()));
    builder.CreateStore(nextaddr, address);

    inst = builder.CreateBitCast(inst, namedTypes["ObjPtr"]);
    inst = builder.CreateCall(safeGetFunc("encodeObj"), {inst});
    // If there is a constructor declared in this class, call it
    if(klass.ty->methods.contains(name)){
        std::pair<types::tyVarIdx, uInt64> fnty = klass.ty->methods[name];
        auto fn  = functions[typeEnv[fnty.first]];
        auto ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosure"], klass.methodArrPtr,
                                                                 builder.getInt32(fnty.second));
        // Need to tag the method
        ptr = constObjToVal(ptr);
        vector<llvm::Value*> args = {ptr, inst};
        for(auto arg : expr->args) args.push_back(arg->codegen(this));
        return builder.CreateCall(fn, args);
    }else{
        // TODO: error if there are arguments passed when the constructor doesn't exist
        return inst;
    }
}

//TODO: create a wrapper function that takes a single argument(param) in an array and then calls the function with them
llvm::Value* Compiler::visitAsyncExpr(typedAST::AsyncExpr* expr) {

}
llvm::Value* Compiler::visitAwaitExpr(typedAST::AwaitExpr* expr) {

}

llvm::Value* Compiler::visitCreateClosureExpr(typedAST::CreateClosureExpr* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    inProgressFuncs.emplace(createNewFunc(expr->fn->name, expr->fn->fnTy));

    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        auto tmp = builder.CreateCall(safeGetFunc("getFreevar"), {inProgressFuncs.top().fn->getArg(0), builder.getInt32(i)});
        variables.insert_or_assign(freevar.second->uuid, tmp);
    }

    declareFuncArgs(expr->fn->args);

    for(auto stmt : expr->fn->block.stmts){
        stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto lambda = inProgressFuncs.top().fn;
    inProgressFuncs.pop();

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(lambda, builder.getInt8PtrTy());
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

llvm::Value* Compiler::visitRangeExpr(typedAST::RangeExpr* expr) {

}
llvm::Value* Compiler::visitFuncDecl(typedAST::FuncDecl* stmt) {
    inProgressFuncs.emplace(createNewFunc(stmt->fn->name, stmt->fn->fnTy));

    declareFuncArgs(stmt->fn->args);

    for(auto s : stmt->fn->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = inProgressFuncs.top().fn;
    inProgressFuncs.pop();

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    // Every function is converted to a closure(even if it has 0 freevars) for ease of use when calling
    // Since this is a global function declaration number of freevars is always going to be 0
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(fn, builder.getInt8PtrTy());
    auto arity = builder.getInt8(stmt->fn->args.size());
    auto name = createConstStr(stmt->fn->name);
    auto freeVarCnt = builder.getInt8(0);
    auto ty = llvm::PointerType::getUnqual(namedTypes["ObjFreevarPtr"]);
    auto freeVarArr = llvm::ConstantPointerNull::get(ty);

    // Create function constant
    llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                         {createConstObjHeader(+object::ObjType::CLOSURE),
                                          arity, freeVarCnt, typeErasedFn, name, freeVarArr});
    // Creates a place in memory for the function and stores it there
    llvm::Constant* fnLoc = storeConstObj(fnC);
    replaceGV(stmt->globalVarUuid, constObjToVal(fnLoc));
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitReturnStmt(typedAST::ReturnStmt* stmt) {
    builder.CreateRet(stmt->expr->codegen(this));
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitUncondJump(typedAST::UncondJump* stmt) {
    switch(stmt->jmpType){
        case typedAST::JumpType::BREAK: builder.CreateBr(breakJumpDest.top()); break;
        case typedAST::JumpType::CONTINUE: builder.CreateBr(continueJumpDest.top()); break;
        case typedAST::JumpType::ADVANCE: builder.CreateBr(advanceJumpDest.top()); break;
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

    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*ctx, "while.cond", func);
    llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*ctx, "while.loop");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "while.merge");

    continueJumpDest.push(condBB);
    breakJumpDest.push(mergeBB);
    builder.CreateBr(condBB);
    builder.SetInsertPoint(condBB);
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
        builder.CreateBr(condBB);
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
    auto fieldsLen = builder.getInt64(stmt->fields.size());
    auto methodsLen = builder.getInt64(stmt->methods.size());
    // Generates functions to call when type of instance is not known at compile time to get the index into the field/method array
    auto fieldsFunc = createFieldChooseFunc(stmt->fullName, stmt->fields);
    auto methodsFunc = createMethodChooseFunc(stmt->fullName, stmt->methods);
    // Result of a dfs done on the class graph
    auto subClassIdxStart = builder.getInt32(classHierarchy[stmt->fullName].first);
    auto subClassIdxEnd = builder.getInt32(classHierarchy[stmt->fullName].second);

    fastMap<string, llvm::Function*> methodDecl;
    vector<llvm::Constant*> methods(stmt->methods.size());
    for(auto p : stmt->methods){
        // Forward declare all methods first so that they can be used inside of other methods
        llvm::Function* methodFn = forwardDeclMethod(p.second.first);
        methodDecl[p.first] = methodFn;
        // Creates an ObjClosure associated with this method
        methods[p.second.second] = createMethodObj(p.second.first, methodFn);
    }
    llvm::GlobalVariable* methodArr = storeConstObj(llvm::ConstantArray::get(llvm::ArrayType::get(namedTypes["ObjClosure"],
                                                                   methods.size()), methods));

    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClass"), {
            createConstObjHeader(+object::ObjType::CLASS), name, subClassIdxStart, subClassIdxEnd, methodsFunc,
            fieldsFunc, methodsLen, fieldsLen, methodArr});
    llvm::GlobalVariable* klass = new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                                           llvm::GlobalVariable::PrivateLinkage, obj);
    // Associates a full class name with the class object and instance template
    classes[stmt->fullName] = Class(klass, createInstanceTemplate(klass, stmt->fields.size()),
                                    stmt->classType, methodArr);

    for(auto p : stmt->methods){
        codegenMethod(p.second.first, subClassIdxStart, subClassIdxEnd, methodDecl[p.first]);
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitInstGet(typedAST::InstGet* expr) {
    auto inst = expr->instance->codegen(this);
    if(typeEnv[expr->instance->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        return optimizeInstGet(inst, expr->field, klass);
    }
    auto field = createESLString(expr->field);
    createRuntimeTypeCheck(safeGetFunc("isObjType"), {inst, builder.getInt8(+object::ObjType::INSTANCE)},
                           "isInst", "Expected an instance, got '{}'", expr->dbgInfo.accessor);

    inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
    inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                         {builder.getInt32(0), builder.getInt32(2)});
    auto klass = builder.CreateLoad(namedTypes["ObjClassPtr"], ptr);

    // first el: field index, second el: method index
    // Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
    auto indicies = instGetUnoptIdx(klass, field);

    // Creates the switch which returns either method or field
    return instGetUnoptimized(inst, indicies.first, indicies.second, klass, expr->field);
}
llvm::Value* Compiler::visitInstSet(typedAST::InstSet* expr) {
    auto inst = expr->instance->codegen(this);
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
        createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", expr->dbgInfo.op);
    }
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedField},
                           "valIsNum", "Expected a number, got {}", expr->dbgInfo.field);

    val = CastToESLVal(decoupleSetOperation(storedField, val, expr->operationType, expr->dbgInfo.op));
    builder.CreateStore(val, fieldPtr);
    return val;
}

llvm::Value* Compiler::visitScopeBlock(typedAST::ScopeEdge* stmt) {
    // Erases local variables which will no longer be used, done to keep memory usage at least somewhat reasonable
    for(auto uuid : stmt->toPop){
        variables.erase(uuid);
    }
    return nullptr; // Stmts return nullptr on codegen
}

#pragma region old stuff
/*
void Compiler::visitUnaryExpr(AST::UnaryExpr* expr) {
    updateLine(expr->op);
    //incrementing and decrementing a variable or an object field is optimized using INCREMENT opcode
    //the value from a variable is fetched, incremented/decremented and put into back into the variable in a single dispatch iteration
    /*if (expr->op.type == TokenType::INCREMENT || expr->op.type == TokenType::DECREMENT) {
        int arg = -1;
        //type definition and arg size
        //0: local(8bit index), 1: local upvalue(8bit index), 2: upvalue(8bit index), 3: global(8bit constant), 4: global(16bit constant)
        //5: dot access(8bit constant), 6: dot access(16bit constant), 7: field access(none, field is compiled to stack)
        byte type = 0;
        if (expr->right->type == AST::ASTType::LITERAL) {
            //if a variable is being incremented, first get what kind of variable it is(local, upvalue or global)
            //also get argument(local: stack position, upvalue: upval position in func, global: name constant index)
            Token token = probeToken(expr->right);

            updateLine(token);
            arg = resolveLocal(token);
            if (arg != -1) {
                type = 0;
                if(current->locals[arg].isLocalUpvalue) type = 1;
            }
            else if ((arg = resolveUpvalue(current, token)) != -1) type = 2;
            else if((arg = resolveClassField(token, true)) != -1){
                if(current->type == FuncType::TYPE_FUNC) error(token, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", token.getLexeme()));
                namedVar(syntheticToken("this"), false);
                type = arg > SHORT_CONSTANT_LIMIT ? 6 : 5;
            }
            else if((arg = resolveGlobal(token, true)) != -1){
                if(arg == -2) error(token, fmt::format("Trying to access variable '{}' before it's initialized.", token.getLexeme()));
                if(!definedGlobals[arg]) error(token, fmt::format("Use of undefined variable '{}'.", token.getLexeme()));
                type = arg > SHORT_CONSTANT_LIMIT ? 4 : 3;
            }
            else error(token, fmt::format("Variable '{}' isn't declared.", token.getLexeme()));
        }
        else if (expr->right->type == AST::ASTType::FIELD_ACCESS) {
            // If a field is being incremented, compile the object, and then if it's not a dot access also compile the field
            auto left = std::static_pointer_cast<AST::FieldAccessExpr>(expr->right);
            updateLine(left->accessor);
            left->callee->accept(this);

            if (left->accessor.type == TokenType::DOT) {
                // Little check to see if field access is to 'this', in which case the correct(public/private) name must be chosen
                if(isLiteralThis(left->callee)){
                    Token name = probeToken(left->field);
                    int res = resolveClassField(name, true);
                    if(res != -1) {
                        namedVar(syntheticToken("this"), false);
                        arg = res;
                    }
                }
                else arg = identifierConstant(probeToken(left->field));
                type = arg > SHORT_CONSTANT_LIMIT ? 6 : 5;
            }
            else {
                left->field->accept(this);
                type = 7;
            }
        }
        else error(expr->op, "Left side is not incrementable.");

        //0b00000001: increment or decrement
        //0b00000010: prefix or postfix increment/decrement
        //0b00011100: type
        byte args = 0;
        args = (expr->op.type == TokenType::INCREMENT ? 1 : 0) |
               ((expr->isPrefix ? 1 : 0) << 1) |
               (type << 2);
        emitBytes(+OpCode::INCREMENT, args);

        if (arg != -1) arg > SHORT_CONSTANT_LIMIT ? emit16Bit(arg) : emitByte(arg);

        return;
    }

    // Regular unary operations
    if (expr->isPrefix) {
        llvm::Value* rhs = evalASTExpr(expr->right);
        llvm::Function *F = builder.GetInsertBlock()->getParent();
        // If rhs isn't of the correct type, go to error, otherwise proceed as normal
        llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
        // Name is set in the switch
        llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx);

        switch (expr->op.type) {
            case TokenType::TILDA:
            case TokenType::MINUS: {
                if(expr->op.type == TokenType::TILDA) executeOpBB->setName("binnegnum");
                else executeOpBB->setName("negnum");

                builder.CreateCondBr(builder.CreateNot(builder.CreateCall(curModule->getFunction("isNum"), rhs)), errorBB, executeOpBB);
                // Calls the type error function which throws
                builder.SetInsertPoint(errorBB);
                createTyErr("Operand must be a number, got '{}'.", rhs);
                // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
                builder.CreateBr(executeOpBB);

                F->insert(F->end(), executeOpBB);
                builder.SetInsertPoint(executeOpBB);
                // For binary negation, the casting is as follows Value -> double -> int64 -> double -> Value
                if(expr->op.type == TokenType::TILDA){
                    auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
                    auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"binnegtmp");
                    retVal(builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx)));
                }else{
                    auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
                    retVal(builder.CreateFNeg(tmp, "fnegtmp"));
                }
                break;
            }
            case TokenType::BANG: {
                executeOpBB->setName("negbool");

                builder.CreateCondBr(builder.CreateNot(builder.CreateCall(curModule->getFunction("isBool"), rhs)), errorBB, executeOpBB);
                // Calls the type error function which throws
                builder.SetInsertPoint(errorBB);
                createTyErr("Operand must be a boolean, got '{}'.", rhs);
                // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
                builder.CreateBr(executeOpBB);

                F->insert(F->end(), executeOpBB);
                builder.SetInsertPoint(executeOpBB);
                // Quick optimization, instead of decoding bool, negating and encoding, we just XOR with the true flag
                // Since that's just a single bit flag that's flipped on when true and off when false
                // This does rely on the fact that true and false are the first flags and are thus represented with 00 and 01
                returnValue = builder.CreateXor(rhs, builder.getInt64(MASK_TYPE_TRUE));
                break;
            }

        }
        return;
    }

    error(expr->op, "Unexpected operator.");
}

void Compiler::visitCallExpr(AST::CallExpr* expr) {
    // Invoking is field access + call, when the compiler recognizes this pattern it optimizes
    /*if (invoke(expr)) return;
    //todo: tail recursion optimization
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->args.size());
    auto callee = evalASTExpr(expr->callee);
    vector<llvm::Value*> args;
    for (AST::ASTNodePtr arg : expr->args) {
        args.push_back(evalASTExpr(arg));
    }
    auto objFnTy = llvm::PointerType::getUnqual(namedTypes["ObjFunc"]);
    auto tmp = builder.CreateCall(curModule->getFunction("decodeObj"), callee);
    auto objFnPtr = builder.CreatePointerCast(tmp, objFnTy);
    auto fnPtrPtr = builder.CreateInBoundsGEP(namedTypes["ObjFunc"], objFnPtr, {builder.getInt32(0), builder.getInt32(1)});
    auto fnPtr = builder.CreateLoad(builder.getInt8PtrTy(), fnPtrPtr);
    auto fnTy = llvm::FunctionType::get(builder.getInt64Ty(), vector<llvm::Type*>(expr->args.size(), builder.getInt64Ty()), false);
    returnValue = builder.CreateCall(fnTy, fnPtr, args);
    builder.CreateCall(curModule->getFunction("print"), args);
}

void Compiler::visitNewExpr(AST::NewExpr* expr){
    // Parser guarantees that expr->call->callee is either a literal or a module access
    /*auto klass = getClassFromExpr(expr->call->callee);

    emitConstant(encodeObj(klass));

    for (AST::ASTNodePtr arg : expr->call->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->call->args.size());
}

void Compiler::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    /*updateLine(expr->accessor);
    //array[index] or object["propertyAsString"]
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        expr->callee->accept(this);
        expr->field->accept(this);
        emitByte(+OpCode::GET);
        return;
    }

    if(tryResolveThis(expr)) return;
    expr->callee->accept(this);
    uint16_t name = identifierConstant(probeToken(expr->field));
    if (name <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::GET_PROPERTY, name);
    else emitByteAnd16Bit(+OpCode::GET_PROPERTY_LONG, name);
    return;
}

void Compiler::visitSuperExpr(AST::SuperExpr* expr) {
    /*int name = identifierConstant(expr->methodName);
    if (currentClass == nullptr) {
        error(expr->methodName, "Can't use 'super' outside of a class.");
    }
    else if (!currentClass->klass->superclass) {
        error(expr->methodName, "Can't use 'super' in a class with no superclass.");
    }
    string fieldName = expr->methodName.getLexeme();
    auto res = classContainsMethod(fieldName, currentClass->klass->superclass->methods);
    if(!res.first){
        error(expr->methodName, fmt::format("Superclass '{}' doesn't contain method '{}'.", currentClass->klass->superclass->name->str, expr->methodName.getLexeme()));
    }
    // We use a synthetic token since 'this' is defined if we're currently compiling a class method
    namedVar(syntheticToken("this"), false);
    emitConstant(encodeObj(currentClass->klass->superclass));
    if (name <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::GET_SUPER, name);
    else emitByteAnd16Bit(+OpCode::GET_SUPER_LONG, name);
}

void Compiler::visitFuncLiteral(AST::FuncLiteral* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    auto upvalsToCapture = upvalueMap.at(expr);
    createNewFunc(expr->arity + (upvalsToCapture.size() > 0 ? 1 : 0), "Anonymous function", FuncType::TYPE_FUNC);
    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    for(int i = 0; i < upvalsToCapture.size(); i++){
        auto& upval = upvalsToCapture[i];
        auto tmp = builder.CreateCall(curModule->getFunction("getUpvalue"), {current->func->getArg(0), builder.getInt32(i)});
        current->freevars.emplace_back(upval.name, tmp);
    }

    // No need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    // If a closure is passed(as the first arg), skip over it
    int argIndex = upvalsToCapture.size() > 0 ? 1 : 0;
    for (AST::ASTVar& var : expr->args) {
        declareLocalVar(var);
        defineLocalVar(current->func->getArg(argIndex++));
    }
    for(auto stmt : expr->body->statements){
        try {
            stmt->accept(this);
        }catch(CompilerException e){

        }
    }
    llvm::Value* func = endFuncDecl(expr->args.size(), "Anonymous function");
    // If this lambda doesn't use any freevars bail early and don't convert it to a closure
    if(upvalsToCapture.size() == 0) {
        returnValue = func;
        return;
    }
    vector<llvm::Value*> closureConstructorArgs = {func, builder.getInt32(upvalsToCapture.size())};

    // Upvalues are gathered after calling endFuncDecl because it returns to the enclosing function
    for(int i = 0; i < upvalsToCapture.size(); i++){
        auto& upval = upvalsToCapture[i];
        if(upval.isLocal) {
            closureConstructorArgs.push_back(current->locals[upval.index].val);
        }else{
            closureConstructorArgs.push_back(current->freevars[upval.index].val);
        }
    }
    // Create the closure and stuff the freevars in it
    returnValue = builder.CreateCall(curModule->getFunction("createClosure"), closureConstructorArgs);


void Compiler::visitAsyncExpr(AST::AsyncExpr* expr) {
   /*updateLine(expr->token);
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::LAUNCH_ASYNC, expr->args.size());
}

void Compiler::visitAwaitExpr(AST::AwaitExpr* expr) {
    updateLine(expr->token);
    expr->expr->accept(this);
    emitByte(+OpCode::AWAIT);
}

void Compiler::visitClassDecl(AST::ClassDecl* decl) {
    /*Token className = decl->getName();
    uInt16 index = declareGlobalVar(className);
    auto klass = new object::ObjClass(className.getLexeme(), nullptr);


    currentClass = std::make_unique<ClassChunkInfo>(klass);

    if (decl->inheritedClass) {
        //if the class inherits from some other class, load the parent class and declare 'super' as a local variable which holds the superclass
        //decl->inheritedClass is always either a LiteralExpr with an identifier token or a ModuleAccessExpr

        //if a class wants to inherit from a class in another file of the same name, the import has to use an alias, otherwise we get
        //undefined behavior (eg. class a : a)
        try {
            auto superclass = getClassFromExpr(decl->inheritedClass);
            klass->superclass = superclass;
            //Copies methods and fields from superclass
            klass->methods = superclass->methods;
            klass->fieldsInit = superclass->fieldsInit;
        }catch(CompilerException& e){

        }
    }else{
        // If no superclass is defined, paste the base class methods into this class, but don't make it the superclass
        // as far as the user is concerned, methods of "baseClass" get implicitly defined for each class
    }

    // Put field and method names into the class
    for(auto& field : decl->fields){
        klass->fieldsInit.insert_or_assign(ObjString::createStr((field.isPublic ? "" : "!") + field.field.getLexeme()), encodeNil());
    }
    // First put the method names into the class, then compiled the methods later to be able to correctly detect the methods when
    // resolveClassField is called
    for(auto& method : decl->methods){
        klass->methods.insert_or_assign(ObjString::createStr((method.isPublic ? "" : "!") + method.method->getName().getLexeme()), nullptr);
    }

    // Define the class here, so that it can be called inside its own methods
    // Defining after inheriting so that a class can't be used as its own parent
    defineGlobalVar(index);
    // Assigning at compile time to save on bytecode, also to get access to the class in getClassFromExpr
    globals[index].val = encodeObj(klass);

    for (auto& _method : decl->methods) {
        //At this point the name is guaranteed to exist as a string, so createStr just returns the already created string
        klass->methods[ObjString::createStr((_method.isPublic ? "" : "!") + _method.method->getName().getLexeme())] = method(_method.method.get(), className);
    }
    currentClass = nullptr;
}*/
#pragma endregion

#pragma region helpers
/*
void Compiler::error(const string& message) noexcept(false) {
    errorHandler::addSystemError("System compile error [line " + std::to_string(current->line) + "] in '" + curUnit->file->name + "': \n" + message + "\n");
    throw CompilerException();
}

void Compiler::error(Token token, const string& msg) noexcept(false) {
    errorHandler::addCompileError(msg, token);
    throw CompilerException();
}*/

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
void Compiler::createTyErr(const string err, llvm::Value* const val, const Token token){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(token.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(token.str.line);
    builder.CreateCall(safeGetFunc("tyErrSingle"), {str, file, line, val});
}
void Compiler::createTyErr(const string err, llvm::Value* const lhs, llvm::Value* const rhs, const Token token){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(token.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(token.str.line);
    builder.CreateCall(safeGetFunc("tyErrDouble"), {str, file, line, lhs, rhs});
}
void Compiler::createRuntimeTypeCheck(llvm::Function* predicate, vector<llvm::Value*> args, string executeBBName,
                                      string errMsg, Token dbg){
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, executeBBName);
    auto cond = builder.CreateCall(predicate, args);
    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(true)});
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    createTyErr(errMsg, args[0], dbg);
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
}
void Compiler::createRuntimeTypeCheck(llvm::Function* predicate, llvm::Value* lhs, llvm::Value* rhs,
                                      string executeBBName, string errMsg, Token dbg){
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, executeBBName);

    auto c1 = builder.CreateCall(predicate, lhs);
    auto c2 = builder.CreateCall(predicate, rhs);
    auto cond = builder.CreateAnd(c1, c2);
    cond = builder.CreateIntrinsic(builder.getInt1Ty(), llvm::Intrinsic::expect, {cond, builder.getInt1(true)});
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);

    builder.SetInsertPoint(errorBB);
    // Calls the type error function which throws
    createTyErr(errMsg, lhs, rhs, dbg);
    builder.CreateUnreachable();
    F->insert(F->end(), executeOpBB);
    // Actual operation goes into this block
    builder.SetInsertPoint(executeOpBB);
}

// Codegen functions
llvm::Value* Compiler::codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, const Token op){
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If both are a number go to addNum, if not try adding as string
    // If both aren't strings, throw error(error is thrown inside strTryAdd C++ function)
    llvm::BasicBlock *addNumBB = llvm::BasicBlock::Create(*ctx, "addnum", F);
    llvm::BasicBlock *addStringBB = llvm::BasicBlock::Create(*ctx, "addstring");
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
    // Have to pass file and line since strAdd might throw an error, and it needs to know where the error occurred
    llvm::Constant* file = createConstStr(op.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(op.str.line);
    // Returns Value
    auto stringAddRes = builder.CreateCall(safeGetFunc("strTryAdd"), {lhs, rhs, file, line});
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
    const auto anyTy = types::getBasicType(types::TypeFlag::ANY);
    if(exprIsType(expr1, expr2, numTy)){
        // fcmp when both values are numbers
        lhs = ESLValTo(lhs, builder.getDoubleTy());
        rhs = ESLValTo(rhs, builder.getDoubleTy());

        auto val = neg ? builder.CreateFCmpONE(lhs, rhs, "fcmpone") : builder.CreateFCmpOEQ(lhs, rhs, "fcmpoeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }
    else if(!exprIsType(expr1, numTy) && !exprIsType(expr2, numTy) && !exprIsType(expr1, anyTy) && !exprIsType(expr2, anyTy)){
        auto val = neg ? builder.CreateICmpNE(lhs, rhs, "icmpne") : builder.CreateICmpEQ(lhs, rhs, "icmpeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }

    // If both values are numbers, use the floating comparison, if there is type mismatch/values are of some other type use icmp
    auto isnum = safeGetFunc("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);

    llvm::Value* icmptmp = builder.CreateICmpEQ(lhs, rhs, "icmptmp");
    llvm::Value* fcmptmp = builder.CreateICmpEQ(ESLValTo(lhs, builder.getDoubleTy()),
                                                 ESLValTo(rhs, builder.getDoubleTy()), "fcmptmp");
    if(neg){
        icmptmp = builder.CreateNot(icmptmp, "icmptmpneg");
        fcmptmp = builder.CreateNot(fcmptmp, "fcmptmpneg");
    }
    // To reduce branching on a common operation, select instruction is used
    auto sel = builder.CreateSelect(builder.CreateAnd(c1, c2), fcmptmp, icmptmp);
    return builder.CreateCall(safeGetFunc("encodeBool"), sel);
}
llvm::Value* Compiler::codegenNeg(const typedExprPtr _rhs, typedAST::UnaryOp op, Token dbg){
    llvm::Value* rhs = _rhs->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If rhs is known to be a number, no need for the type check
    if(!exprIsType(_rhs, types::getBasicType(types::TypeFlag::NUMBER))){
        createRuntimeTypeCheck(safeGetFunc("isNum"), {rhs},
                               "Operand must be a number, got '{}'.", "numneg", dbg);
    }
    // For binary negation, the casting is as follows Value -> double -> int64 -> double -> Value
    if(op == typedAST::UnaryOp::BIN_NEG){
        // Cast value to double, then convert to signed 64bit integer and negate
        auto tmp = ESLValTo(rhs, builder.getDoubleTy());
        auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"binnegtmp");
        // Cast back to double and then to 64bit int
        auto castToDouble = builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx));
        return CastToESLVal(castToDouble);
    }else{
        auto tmp = ESLValTo(rhs, builder.getDoubleTy());
        return CastToESLVal(builder.CreateFNeg(tmp, "fnegtmp"));
    }
}
void Compiler::codegenBlock(const typedAST::Block& block){
    for(auto stmt : block.stmts){
        stmt->codegen(this);
    }
}
llvm::Value * Compiler::codegenIncrement(const typedAST::UnaryOp op, const typedExprPtr expr) {
    // No array/hashmap field access because it's to complicated
    if(expr->type == typedAST::NodeType::VAR_READ){
        return codegenVarIncrement(op, std::reinterpret_pointer_cast<typedAST::VarRead>(expr));
    }else if(expr->type == typedAST::NodeType::INST_GET){
        return codegenInstIncrement(op, std::reinterpret_pointer_cast<typedAST::InstGet>(expr));
    }
    // TODO: error
}
// Reuses var read and var store
llvm::Value * Compiler::codegenVarIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::VarRead> expr) {
    llvm::Value* val = codegenVarRead(expr->varPtr);
    // Right now we can only increment numbers, maybe change this when adding iterators?
    if(!exprIsType(expr, types::getBasicType(types::TypeFlag::NUMBER))){
        createRuntimeTypeCheck(safeGetFunc("isNum"), {val}, "isNum",
                               "Expected a number, got '{}'.", expr->dbgInfo.varName);
    }
    llvm::Value* res = ESLValTo(val, builder.getDoubleTy());

    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    res = CastToESLVal(res);
    codegenVarStore(expr->varPtr, res);
    return val;
}
llvm::Value * Compiler::codegenInstIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::InstGet> expr) {
    auto inst = expr->instance->codegen(this);

    llvm::Value* fieldPtr = nullptr;
    // If type of instance if known optimize getting *pointer* to field
    if(exprIsType(expr->instance, types::getBasicType(types::TypeFlag::INSTANCE))) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        fieldPtr = getOptInstFieldPtr(inst, klass, expr->field);
    }else{
        fieldPtr = getUnoptInstFieldPtr(inst, expr->field, expr->dbgInfo.field);
    }
    llvm::Value* storedField = builder.CreateLoad(getESLValType(), fieldPtr);
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedField},
                           "valIsNum", "Expected a number, got {}", expr->dbgInfo.field);

    llvm::Value* res = ESLValTo(storedField, builder.getDoubleTy());
    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));

    res = CastToESLVal(res);
    builder.CreateStore(res, fieldPtr);
    return storedField;
}

// Function codegen helpers
llvm::Function* Compiler::createNewFunc(const string name, const std::shared_ptr<types::FunctionType> fnTy){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    // First argument is always the closure structure
    for(int i = 0; i < fnTy->argCount + 1; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, name, curModule.get());
    tmp->addFnAttr("frame-pointer", "all");
    tmp->addFnAttr("no-stack-arg-probe");
    // Creates a connection between function types and functions
    functions.insert_or_assign(fnTy, tmp);
    tmp->setGC("statepoint-example");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmp);
    builder.SetInsertPoint(BB);
    return tmp;
}
llvm::FunctionType* Compiler::getFuncType(int argCount){
    vector<llvm::Type*> params;
    // First argument is always the closure structure;
    for(int i = 0; i < argCount+1; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    return fty;
}
void Compiler::declareFuncArgs(const vector<std::shared_ptr<typedAST::VarDecl>>& args){
    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    // First argument is ALWAYS the objclosure ptr
    int argIndex = 1;
    for (auto var : args) {
        llvm::Value* varPtr;
        // Don't need to use temp builder when creating alloca since this happens in the first basicblock of the function
        if(var->varType == typedAST::VarType::LOCAL){
            varPtr = builder.CreateAlloca(getESLValType(), nullptr, var->dbgInfo.varName.getLexeme());
            builder.CreateStore(inProgressFuncs.top().fn->getArg(argIndex++), varPtr);
        }else{
            varPtr = builder.CreateCall(safeGetFunc("createFreevar"), std::nullopt, var->dbgInfo.varName.getLexeme());
            // first index: access to the structure that's being pointed to,
            // second index: access to the second field(64bit field for the value)
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], varPtr, idxList, "freevarAddr");
            builder.CreateStore(inProgressFuncs.top().fn->getArg(argIndex++), tmpEle);
        }
        // Insert the argument into the pool of variables
        variables.insert_or_assign(var->uuid, varPtr);
    }
}

llvm::Value* Compiler::createFuncCall(llvm::Value* closureVal, vector<llvm::Value*> args, Token dbg){
    createRuntimeTypeCheck(safeGetFunc("isObjType"), {closureVal, builder.getInt8(+object::ObjType::CLOSURE)},
                           "call","Expected a function for a callee, got '{}'.", dbg);

    auto closurePtr = builder.CreateCall(safeGetFunc("decodeClosure"), closureVal);
    vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
    auto argNumPtr = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], closurePtr, idxList);
    auto argNum = builder.CreateLoad(builder.getInt8Ty(), argNumPtr);

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    auto errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *callBB = llvm::BasicBlock::Create(*ctx, "call");

    auto cond = builder.CreateICmpEQ(argNum, builder.getInt8(args.size()));
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, callBB);

    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    argCntError(dbg, argNum, args.size());
    builder.CreateUnreachable();

    F->insert(F->end(), callBB);
    builder.SetInsertPoint(callBB);
    std::pair<llvm::Value*, llvm::FunctionType*> func = getBitcastFunc(closurePtr, args.size());

    args.insert(args.begin(), closureVal);
    auto c =  builder.CreateCall(func.second, func.first, args, "callres");

    return c;
}
// closurePtr is detagged closure object
std::pair<llvm::Value*, llvm::FunctionType*> Compiler::getBitcastFunc(llvm::Value* closurePtr, const int argc){
    // Index for accessing the fn ptr is at 3, not 2, because we have a Obj struct at index 0
    vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(3)};
    auto fnPtrAddr = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], closurePtr, idxList);
    llvm::Value* fnPtr = builder.CreateLoad(builder.getInt8PtrTy(), fnPtrAddr);
    auto fnTy = getFuncType(argc);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    fnPtr = builder.CreateBitCast(fnPtr, fnPtrTy);
    return std::make_pair(fnPtr, fnTy);
}
llvm::Value* Compiler::optimizedFuncCall(const typedAST::CallExpr* expr){
    if(!exprIsComplexType(expr->callee, types::TypeFlag::FUNCTION)) return nullptr;

    llvm::Function* fn = nullptr;
    auto funcType = std::reinterpret_pointer_cast<types::FunctionType>(typeEnv[expr->callee->exprType]);
    if(functions.contains(funcType)) {
        // Function might not be codegen-ed at this point,
        // but if it is then use its signature instead of casting the char ptr in ObjClosure to a function
        fn = functions[funcType];
    }
    // Have to codegen closurePtr even if function is known because of side effects
    llvm::Value* closurePtr =  closurePtr = expr->callee->codegen(this);
    llvm::Value* castClosurePtr = builder.CreateCall(safeGetFunc("decodeClosure"), closurePtr);

    vector<llvm::Value*> args;
    // Every function contains the closure struct as the first argument
    args.push_back(closurePtr);
    for(auto arg : expr->args) args.push_back(arg->codegen(this));
    if(funcType->argCount != expr->args.size()){
        errorHandler::addCompileError(fmt::format("Function expects {} parameters, got {} arguments.", funcType->argCount, expr->args.size()),
                                      expr->dbgInfo.paren1);
        throw CompilerError("Incorrect number of arguments passed");
    }

    // If function is known and codegen-ed, call it
    if(fn) return builder.CreateCall(fn, args, "callres");

    // If a functions type is known, but it hasn't been codegen-ed yet use the object to get the func ptr
    // TODO: this can be optimized by reordering compilation order of functions(maybe)
    std::pair<llvm::Value*, llvm::FunctionType*> func = getBitcastFunc(castClosurePtr, funcType->argCount);
    return builder.CreateCall(func.second, func.first, args, "callres");
}

// Array bounds checking
void Compiler::createArrBoundsCheck(llvm::Value* arr, llvm::Value* index, string errMsg, Token dbg){
    llvm::Value* upperbound = builder.CreateCall(safeGetFunc("getArrSize"), arr);
    auto castIndex = ESLValTo(index, builder.getDoubleTy());
    castIndex = builder.CreateFPToUI(castIndex, builder.getInt64Ty());
    auto cond = builder.CreateICmpUGE(castIndex, upperbound);
    auto cond2 = builder.CreateICmpULT(castIndex, builder.getInt64(0));
    cond = builder.CreateOr(cond, cond2);

    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "executeBBName");

    builder.CreateCondBr(cond, errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    createTyErr(errMsg, index, dbg);
    builder.CreateUnreachable();

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
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
    }
    // This will never be hit
}

llvm::Value* Compiler::getArrElement(llvm::Value* arr, llvm::Value* field, bool opt, Token dbg){
    if(!opt) createRuntimeTypeCheck(safeGetFunc("isNum"), {field},
                                    "getArrAddr", "Expected a number, got {}", dbg);
    // Check the index first because we need the untagged version of the index for error reporting
    createArrBoundsCheck(arr, field, "Index {} outside of array range.", dbg);
    field = ESLValTo(field, builder.getDoubleTy());
    field = builder.CreateFPToUI(field, builder.getInt64Ty());


    arr = builder.CreateCall(safeGetFunc("getArrPtr"), arr);
    auto ptr = builder.CreateGEP(getESLValType(), arr, field);
    return builder.CreateLoad(getESLValType(), ptr);
}

llvm::Value* Compiler::getMapElement(llvm::Value* map, llvm::Value* field, bool opt, Token dbg){
    if(!opt) createRuntimeTypeCheck(safeGetFunc("isObjType"), {field, builder.getInt8(+object::ObjType::STRING)},
                               "getHashmapV", "Expected a string, got {}", dbg);
    map = builder.CreateCall(safeGetFunc("decodeObj"), map);
    field = builder.CreateCall(safeGetFunc("decodeObj"), field);
    return builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
}

llvm::Value* Compiler::setArrElement(llvm::Value* arr, llvm::Value* index, llvm::Value* val, bool optIdx, bool optVal,
                                     typedAST::SetType opTy, Token dbg){
    if(!optIdx) createRuntimeTypeCheck(safeGetFunc("isNum"), {index},
                                    "getArrAddr", "Expected a number, got {}", dbg);

    createArrBoundsCheck(arr, index, "Index {} outside of array range.", dbg);
    index = ESLValTo(index, builder.getDoubleTy());
    index = builder.CreateFPToUI(index, builder.getInt64Ty());
    arr = builder.CreateCall(safeGetFunc("getArrPtr"), arr);
    auto ptr = builder.CreateGEP(getESLValType(), arr, index);

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
    if(!optVal) createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", dbg);
    // Since the type system assumes all collections store "any" datatype this runtime check can't be skipped
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedVal},
                           "valIsNum", "Expected a number, got {}", dbg);

    val = CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
    builder.CreateStore(val, ptr);
    return val;
}
llvm::Value* Compiler::setMapElement(llvm::Value* map, llvm::Value* field, llvm::Value* val, bool optIdx, bool optVal,
                                     typedAST::SetType opTy, Token dbg){
    if(!optIdx) createRuntimeTypeCheck(safeGetFunc("isObjType"), {field, builder.getInt8(+object::ObjType::STRING)},
                               "setHashmapV", "Expected a string, got {}", dbg);

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
    if(!optVal) createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", dbg);
    // Since the type system assumes all collections store "any" datatype this runtime check can't be skipped
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedVal},
                           "valIsNum", "Expected a number, got {}", dbg);

    val = CastToESLVal(decoupleSetOperation(storedVal, val, opTy, dbg));
    builder.CreateCall(safeGetFunc("hashmapSetV"), {map, field, val});
    return val;
}

// Switch stmt stuff
// For everything except strings
llvm::ConstantInt* Compiler::createSwitchConstantInt(std::variant<double, bool, void*, string>& constant){
    switch(constant.index()){
        case 0: return builder.getInt64(*reinterpret_cast<uInt64*>(&get<double>(constant)));
        case 1: return builder.getInt64(get<bool>(constant) ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE);
        case 2: return builder.getInt64(MASK_SIGNATURE_NIL);
        default: assert(false && "Unreachable");
    }
}

vector<llvm::BasicBlock*> Compiler::createNCaseBlocks(int n){
    vector<llvm::BasicBlock*> blocks;
    for(int i = 0; i < n; i++){
        auto caseBB = llvm::BasicBlock::Create(*ctx, fmt::format("case{}", i));
        blocks.emplace_back(caseBB);
    }
    return blocks;
}

llvm::Value* Compiler::createSeqCmp(llvm::Value* compVal, vector<std::pair<std::variant<double, bool, void*, string>, int>>& constants){
    // Starting index is outside the range of blocks so that if switch executes with it control flow goes to default dest
    llvm::Value* BBIdx = builder.getInt32(-1);
    for(auto c : constants){
        llvm::Value* val = createConstant(c.first);
        // All constants(including strings because of interning) have a unique representation as I64, so ICmpEQ is sufficient
        // compVal is i64
        llvm::Value* cmp = builder.CreateICmpEQ(compVal, val);
        // If comparison is successful BBIdx becomes the index of the block that the switch needs to jump to
        BBIdx = builder.CreateSelect(cmp, builder.getInt32(c.second), BBIdx);
    }
    return BBIdx;
}

// Class helpers
llvm::Function* Compiler::createFieldChooseFunc(string className, std::unordered_map<string, int>& fields){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, className+":fieldChoose", curModule.get());

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    llvm::Value* idx = builder.getInt32(-1);
    for(auto p : fields){
        auto toCmp = createESLString(p.first);
        auto cmp = builder.CreateICmpEQ(fn->getArg(0), toCmp);
        idx = builder.CreateSelect(cmp, builder.getInt32(p.second), idx);
    }
    builder.CreateRet(idx);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}
llvm::Function* Compiler::createMethodChooseFunc(string className, std::unordered_map<string, std::pair<typedAST::ClassMethod, int>>& methods){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, className+":methodChoose", curModule.get());

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder.SetInsertPoint(BB);
    llvm::Value* idx = builder.getInt32(-1);
    for(auto p : methods){
        auto toCmp = createESLString(p.first);
        auto cmp = builder.CreateICmpEQ(fn->getArg(0), toCmp);
        idx = builder.CreateSelect(cmp, builder.getInt32(p.second.second), idx);
    }
    builder.CreateRet(idx);

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
    return fn;
}

llvm::Function* Compiler::forwardDeclMethod(typedAST::ClassMethod& method){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    // First argument is always the closure structure
    for(int i = 0; i < method.code->fnTy->argCount+1; i++) params.push_back(getESLValType());
    llvm::FunctionType* fty = llvm::FunctionType::get(getESLValType(), params, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, method.code->name, curModule.get());
    fn->addFnAttr("frame-pointer", "all");
    fn->addFnAttr("no-stack-arg-probe");
    // Creates a connection between function types and functions
    functions.insert_or_assign(method.code->fnTy, fn);
    fn->setGC("statepoint-example");
    return fn;
}
void Compiler::codegenMethod(typedAST::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd,
                             llvm::Function* methodFn){
    inProgressFuncs.emplace(methodFn);
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", inProgressFuncs.top().fn);
    builder.SetInsertPoint(BB);
    declareFuncArgs(method.code->args);
    createRuntimeTypeCheck(safeGetFunc("isInstAndClass"),{inProgressFuncs.top().fn->getArg(1), subClassIdxStart, subClassIdxEnd},
                           "isInst", "Expected instance of class, got '{}'.", method.dbg.name);

    for(auto s : method.code->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = inProgressFuncs.top();
    inProgressFuncs.pop();

    // Set insertion point to the end of the enclosing method
    builder.SetInsertPoint(&inProgressFuncs.top().fn->back());
}

llvm::Constant* Compiler::createMethodObj(typedAST::ClassMethod& method, llvm::Function* methodPtr){
    // Every function is converted to a closure(if even it has 0 freevars) for ease of use when calling
    // Methods can't have freevars
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(methodPtr, builder.getInt8PtrTy());
    auto arity = builder.getInt8(method.code->args.size());
    auto name = createConstStr(method.code->name);
    auto freeVarCnt = builder.getInt8(0);
    auto ty = llvm::PointerType::getUnqual(namedTypes["ObjFreevarPtr"]);
    auto freeVarArr = llvm::ConstantPointerNull::get(ty);

    // Create method constant
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                     {createConstObjHeader(+object::ObjType::CLOSURE),
                                      arity, freeVarCnt, typeErasedFn, name, freeVarArr});
}
// Creates an instance template with already nulled fields that is memcpy-ed when using new
llvm::GlobalVariable* Compiler::createInstanceTemplate(llvm::Constant* klass, int fieldN){
    vector<llvm::Constant*> fields(fieldN);
    std::fill(fields.begin(), fields.end(), ConstCastToESLVal(builder.getInt64(MASK_SIGNATURE_NIL)));
    // Template array that is already nulled
    llvm::Constant* fieldArr = llvm::ConstantArray::get(llvm::ArrayType::get(getESLValType(), fieldN), fields);
    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjInstance"), {
            createConstObjHeader(+object::ObjType::INSTANCE), builder.getInt32(fieldN), klass,
            llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(getESLValType()))
    });
    // Struct and array should be one after another without any padding(?)
    auto inst =  llvm::ConstantStruct::get(llvm::StructType::create(*ctx,{namedTypes["ObjInstance"],
                                                                    llvm::ArrayType::get(getESLValType(), fieldN)}),
                                     {obj, fieldArr});
    return storeConstObj(inst);
}

llvm::Value* Compiler::optimizeInstGet(llvm::Value* inst, string field, Class& klass){
    if(klass.ty->methods.contains(field)){
        // Index into the array of ObjClosure contained in the class
        auto arrIdx = builder.getInt32(klass.ty->methods[field].second);

        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        llvm::Constant* val = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosure"], klass.methodArrPtr, arrIdx);
        return constObjToVal(val);
    }else if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // TODO: maybe just use 2 GEPs without load in between? since first load just loads the pointer at the end of ObjInstance
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(3)});
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);

        ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        return builder.CreateLoad(getESLValType(), ptr);
    }else{
        // TODO: error
    }
}
llvm::Value* Compiler::instGetUnoptimized(llvm::Value* inst, llvm::Value* fieldIdx, llvm::Value* methodIdx, llvm::Value* klass, string fieldName){
    auto cmp1 = builder.CreateICmpSGT(fieldIdx, builder.getInt32(-1));
    auto cmp2 = builder.CreateICmpSGT(methodIdx, builder.getInt32(-1));

    llvm::Value* dest = builder.getInt32(0);
    dest = builder.CreateSelect(cmp1, builder.getInt32(1), dest);
    dest = builder.CreateSelect(cmp2, builder.getInt32(2), dest);

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(*ctx, "methods");

    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto sw = builder.CreateSwitch(dest, errorBB);
    sw->addCase(builder.getInt32(1), fieldBB);
    sw->addCase(builder.getInt32(2), methodBB);

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);

    // Loads the pointer to fields array
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);
    // Loads the field
    ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(getESLValType(), ptr);
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);
    // Loads the pointer to the methods array
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(8)});
    llvm::Value* val = builder.CreateLoad(namedTypes["ObjClosurePtr"], ptr);
    val = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], val, methodIdx);
    auto method = builder.CreateCall(safeGetFunc("encodeObj"), {val});
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);

    builder.CreateCall(safeGetFunc("printf"), {createConstStr("Instance doesn't contain field or method %s"),
                                               createConstStr(fieldName)});
    builder.CreateCall(safeGetFunc("exit"), {builder.getInt32(64)});
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(getESLValType(), 2);
    phi->addIncoming(field, fieldBB);
    phi->addIncoming(method, methodBB);
    return phi;
}

std::pair<llvm::Value*, llvm::Value*> Compiler::instGetUnoptIdx(llvm::Value* klass, llvm::Constant* field){
    auto fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(4)});
    auto methodFunc = builder.CreateLoad(fnPtrTy, ptr);
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(5)});
    auto fieldsFunc = builder.CreateLoad(fnPtrTy, ptr);

    llvm::Value* fieldIdx = builder.CreateCall(fnTy, fieldsFunc, field);
    llvm::Value* methodIdx = builder.CreateCall(fnTy, methodFunc, field);
    return std::make_pair(fieldIdx, methodIdx);
}

llvm::Value* Compiler::getOptInstFieldPtr(llvm::Value* inst, Class& klass, string field){
    if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // TODO: maybe just use 2 GEPs without load in between? since first load just loads the pointer at the end of ObjInstance
        // Loads the pointer to array from instance
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(3)});
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);

        ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        return ptr;
    }else{
        // TODO: error
    }
    // Unreachable (at least it should be)
    return nullptr;
}

llvm::Value* Compiler::getUnoptInstFieldPtr(llvm::Value* inst, string field, Token dbg){
    createRuntimeTypeCheck(safeGetFunc("isObjType"), {inst, builder.getInt8(+object::ObjType::INSTANCE)},
                           "isInst", "Expected an instance, got '{}'", dbg);

    inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
    inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                         {builder.getInt32(0), builder.getInt32(2)});
    auto klass = builder.CreateLoad(namedTypes["ObjClassPtr"], ptr);

    // Gets the function which determines index of field given a string
    auto fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {getESLValType()}, false);
    auto fnPtrTy = llvm::PointerType::getUnqual(fnTy);
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(5)});
    auto fieldsFunc = builder.CreateLoad(fnPtrTy, ptr);

    llvm::Value* fieldIdx = builder.CreateCall(fnTy, fieldsFunc, createESLString(field));

    auto cmp = builder.CreateICmpEQ(fieldIdx, builder.getInt32(-1));

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    builder.CreateCondBr(cmp, errorBB, fieldBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);

    // TODO make better errors
    builder.CreateCall(safeGetFunc("printf"), {createConstStr("Instance doesn't contain field '%s'"),
                                               createConstStr(field)});
    builder.CreateCall(safeGetFunc("exit"), {builder.getInt32(64)});
    builder.CreateUnreachable();

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);
    // Get fields array
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);
    // Get field pointer
    ptr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);

    return ptr;
}

llvm::Value* Compiler::optimizeInvoke(llvm::Value* inst, string field, Class& klass, vector<llvm::Value*>& callArgs, Token dbg){
    if(klass.ty->methods.contains(field)){
        // Index into the array of ObjClosure contained in the class
        auto arrIdx = builder.getInt32(klass.ty->methods[field].second);

        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        auto closure = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosure"], klass.methodArrPtr, arrIdx);
        // Closures in class are stored as raw pointers, tag them and then pass to method
        closure = constObjToVal(closure);
        llvm::Function* fn = functions[typeEnv[klass.ty->methods[field].first]];

        vector<llvm::Value*> args = {closure, inst};
        args.insert(args.end(), callArgs.begin(), callArgs.end());
        return builder.CreateCall(fn, args);
    }else if(klass.ty->fields.contains(field)){
        // Index into the array of fields of the instance
        auto arrIdx = builder.getInt32(klass.ty->fields[field].second);

        inst = builder.CreateCall(safeGetFunc("decodeObj"), {inst});
        inst = builder.CreateBitCast(inst, namedTypes["ObjInstancePtr"]);
        // TODO: maybe just use 2 GEPs without load in between? since first load just loads the pointer at the end of ObjInstance
        // Get and load pointer to fields
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(3)});
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);
        // Actually get the field
        llvm::Value* fieldPtr = builder.CreateInBoundsGEP(getESLValType(), ptr, {arrIdx});
        auto closure = builder.CreateLoad(getESLValType(), fieldPtr);
        return createFuncCall(closure, callArgs, dbg);
    }else{
        // TODO: error
    }
}

llvm::Value* Compiler::unoptimizedInvoke(llvm::Value* inst, llvm::Value* fieldIdx, llvm::Value* methodIdx, llvm::Value* klass,
                                         string fieldName, vector<llvm::Value*> args, Token dbg){
    auto cmp1 = builder.CreateICmpSGT(fieldIdx, builder.getInt32(-1));
    auto cmp2 = builder.CreateICmpSGT(methodIdx, builder.getInt32(-1));

    // TODO: is this faster then bitshifting?
    llvm::Value* dest = builder.getInt32(0);
    dest = builder.CreateSelect(cmp1, builder.getInt32(1), dest);
    dest = builder.CreateSelect(cmp2, builder.getInt32(2), dest);

    llvm::Function *F = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *fieldBB = llvm::BasicBlock::Create(*ctx, "fields");
    llvm::BasicBlock *methodBB = llvm::BasicBlock::Create(*ctx, "methods");

    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    auto sw = builder.CreateSwitch(dest, errorBB);
    sw->addCase(builder.getInt32(1), fieldBB);
    sw->addCase(builder.getInt32(2), methodBB);

    F->insert(F->end(), fieldBB);
    builder.SetInsertPoint(fieldBB);
    // If this is a field get the fields arr ptr first
    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    // Loads the arr ptr
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(getESLValType()), ptr);
    // Gets and loads the actual field
    auto fieldPtr = builder.CreateInBoundsGEP(getESLValType(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(getESLValType(), fieldPtr);
    auto callres1 = createFuncCall(field, args, dbg);
    fieldBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);
    // First load the ObjClosurePtr(we treat the offset into the ObjClosure array that the class has as a standalone pointer to that closure)
    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(8)});
    llvm::Value* val = builder.CreateLoad(namedTypes["ObjClosurePtr"], ptr);
    val = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], val, methodIdx);
    // Since this is a raw ObjClosure pointer we tag it first
    auto method = builder.CreateCall(safeGetFunc("encodeObj"), val);
    // Inst passed to this function is decoded so it also needs to be tagged again
    auto encodedInst = builder.CreateCall(safeGetFunc("encodeObj"), inst);
    args.insert(args.begin(), encodedInst);

    auto callres2 = createFuncCall(method, args, dbg);
    methodBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);

    // TODO: better errors
    builder.CreateCall(safeGetFunc("printf"), {createConstStr("Instance doesn't contain field or method %s"),
                                               createConstStr(fieldName)});
    builder.CreateCall(safeGetFunc("exit"), {builder.getInt32(64)});
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(getESLValType(), 2);
    phi->addIncoming(callres1, fieldBB);
    phi->addIncoming(callres2, methodBB);
    return phi;
}
// Misc
llvm::Constant* Compiler::createConstStr(const string& str){
    if(CStrings.contains(str)) return CStrings[str];
    auto constant = builder.CreateGlobalStringPtr(str, "internalString", 0, curModule.get());

    CStrings[str] = constant;
    return constant;
}

llvm::Constant* Compiler::createESLString(const string& str){
    if(ESLStrings.contains(str)) return ESLStrings[str];
    auto obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjString"), {
        createConstObjHeader(+object::ObjType::STRING), createConstStr(str)});
    auto val = constObjToVal(storeConstObj(obj));
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
    llvm::Constant* line = builder.getInt32(token.str.line);

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
            uInt64 val = get<bool>(constant) ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE;
            return builder.getInt64(val);
        }
        case 2: return builder.getInt64(MASK_SIGNATURE_NIL);
        case 3: {
            return ESLConstTo(createESLString(get<string>(constant)), builder.getInt64Ty());
        }
    }
    assert(false && "Unreachable");
}

llvm::GlobalVariable* Compiler::storeConstObj(llvm::Constant* obj){
    return new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                    llvm::GlobalVariable::LinkageTypes::PrivateLinkage, obj, "internalConstObj");
}
llvm::Constant* Compiler::createConstObjHeader(int type){
    // 128 is a magic constant that tells the gc that this is a constant object
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "Obj"),{builder.getInt8(type),
                                         builder.getInt8(128),builder.getInt1(true)});
}

llvm::Constant* Compiler::constObjToVal(llvm::Constant* obj){
    auto val = llvm::ConstantExpr::getPtrToInt(obj, builder.getInt64Ty());
    return ConstCastToESLVal(llvm::ConstantExpr::getAdd(val, builder.getInt64(MASK_SIGNATURE_OBJ)));
}

void Compiler::replaceGV(uInt64 uuid, llvm::Constant* newInit){
    auto gv = (llvm::dyn_cast<llvm::GlobalVariable>(variables.at(uuid)));
    llvm::GlobalVariable* newgv = new llvm::GlobalVariable(*curModule, getESLValType(), false,
                                                           llvm::GlobalVariable::PrivateLinkage,
                                                           newInit, gv->getName());
    newgv->setAlignment(llvm::Align::Of<Value>());
    gv->replaceAllUsesWith(newgv);
    variables.insert_or_assign(uuid, newgv);
    gv->eraseFromParent();
}

llvm::Value* Compiler::codegenVarRead(std::shared_ptr<typedAST::VarDecl> varPtr){
    switch(varPtr->varType){
        case typedAST::VarType::LOCAL:{
            return builder.CreateLoad(getESLValType(), variables.at(varPtr->uuid), "loadlocal");
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* upvalPtr = variables.at(varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], upvalPtr, idxList, "freevarAddr");
            return builder.CreateLoad(getESLValType(), tmpEle, "loadfreevar");
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:{
            return builder.CreateLoad(getESLValType(), variables.at(varPtr->uuid), "loadgvar");
        }
    }
    assert(false && "Unreachable");
    return nullptr;
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
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], freevarPtr, idxList, "freevarAddr");
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

void Compiler::implementNativeFunctions(fastMap<string, types::tyVarIdx>& natives){
    auto addNativeFn = [&](string name, int argc, types::tyPtr type){
        // +1 for the closure
        vector<llvm::Type*> args(argc+1, getESLValType());
        auto func = llvm::Function::Create(llvm::FunctionType::get(getESLValType(), args, false),
                                         llvm::Function::ExternalLinkage, name, curModule.get());
        functions[type] = func;
        auto typeErasedFn = llvm::ConstantExpr::getBitCast(func, builder.getInt8PtrTy());
        auto arity = builder.getInt8(argc);
        auto cname = createConstStr(name);
        auto freeVarCnt = builder.getInt8(0);
        auto ty = llvm::PointerType::getUnqual(namedTypes["ObjFreevarPtr"]);
        auto freeVarArr = llvm::ConstantPointerNull::get(ty);

        // Create function constant
        llvm::Constant* fnC = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClosure"),
                                                        {createConstObjHeader(+object::ObjType::CLOSURE),
                                                         arity, freeVarCnt, typeErasedFn, cname, freeVarArr});
        // Creates a place in memory for the function and stores it there
        llvm::Constant* fnLoc = storeConstObj(fnC);
        return constObjToVal(fnLoc);
    };
    for(std::pair<string, types::tyVarIdx> p : natives){
        auto fnTy = std::reinterpret_pointer_cast<types::FunctionType>(typeEnv[p.second]);
        nativeFunctions[p.first] = addNativeFn(p.first, fnTy->argCount, fnTy);
    }
}

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
