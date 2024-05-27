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
    currentFunction = nullptr;

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
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmpfn);
    builder.SetInsertPoint(BB);
    currentFunction = tmpfn;
    try {
        for (auto stmt: _code->block.stmts) {
            stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
        }
    }catch(CompilerError err){
        std::cout<<fmt::format("Compiler exited because of error: '{}'.", err.reason);
    }
    // Get all string constants into gc
    auto it = currentFunction->getEntryBlock().begin();
    std::advance(it, getIP());
    llvm::IRBuilder<> tempBuilder(&tmpfn->getEntryBlock(), it);
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
            llvm::IRBuilder<> tempBuilder(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
            auto tmp = tempBuilder.CreateAlloca(builder.getInt64Ty(), nullptr, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);
            break;
        }
        case typedAST::VarType::FREEVAR:{
            auto it = currentFunction->getEntryBlock().begin();
            std::advance(it, getIP());
            llvm::IRBuilder<> tempBuilder(&currentFunction->getEntryBlock(), it);
            auto tmp = tempBuilder.CreateCall(safeGetFunc("createFreevar"), std::nullopt, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);

            break;
        }
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL:{
            string varName = decl->dbgInfo.varName.getLexeme() + std::to_string(decl->uuid);
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(*curModule, builder.getInt64Ty(), false,
                                                                  llvm::GlobalVariable::PrivateLinkage,
                                                                  builder.getInt64(MASK_SIGNATURE_NIL), varName);
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
        if(exprIsType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))){
            auto castlhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
            auto castrhs = builder.CreateBitCast(rhs, builder.getDoubleTy());
            return castToVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
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
    lhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
    rhs = builder.CreateBitCast(rhs, builder.getDoubleTy());

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
    return builder.CreateBitCast(val, builder.getInt64Ty());
}
llvm::Value* Compiler::visitComparisonExpr(typedAST::ComparisonExpr* expr) {
    using typedAST::ComparisonOp;
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

    lhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
    rhs = builder.CreateBitCast(rhs, builder.getDoubleTy());
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
    // TODO: implement incrementing
    if(expr->opType == UnaryOp::NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        // If type is known to be a bool skip the runtime check and just execute the expr
        if(exprIsType(expr->rhs, types::getBasicType(types::TypeFlag::BOOL))) {
            return builder.CreateXor(rhs, builder.getInt64(MASK_TYPE_TRUE));
        }

        createRuntimeTypeCheck(safeGetFunc("isBool"), {rhs},
                               "Operand must be a boolean, got '{}'.", "negbool", expr->dbgInfo.op);
        // Quick optimization, instead of decoding bool, negating and encoding, we just XOR with the true flag
        // Since that's just a single bit flag that's flipped on when true and off when false
        // This does rely on the fact that true and false are the first flags and are thus represented with 00 and 01
        return builder.CreateXor(rhs, builder.getInt64(MASK_TYPE_TRUE));
    }else if(expr->opType == UnaryOp::FNEG || expr->opType == UnaryOp::BIN_NEG){
        return codegenNeg(expr->rhs, expr->opType, expr->dbgInfo.op);
    }else{
        return codegenIncrement(expr->opType, expr->rhs);
    }
    assert(false && "Unreachable");
}

llvm::Value* Compiler::visitLiteralExpr(typedAST::LiteralExpr* expr) {
    switch(expr->val.index()){
        case 0: {
            auto tmp = llvm::ConstantFP::get(*ctx, llvm::APFloat(get<double>(expr->val)));
            return builder.CreateBitCast(tmp, builder.getInt64Ty());
        }
        case 1: {
            uInt64 val = get<bool>(expr->val) ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE;
            return builder.getInt64(val);
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
    //for each field, compile it and get the constant of the field name
    for (auto entry : expr->fields) {
        //this gets rid of quotes, ""Hello world""->"Hello world"
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
    auto arrPtr = builder.CreateCall(safeGetFunc("getArrPtr"), arr, "arrptr");
    for(int i = 0; i < vals.size(); i++){
        auto gep = builder.CreateInBoundsGEP(builder.getInt64Ty(), arrPtr, builder.getInt32(i));
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
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "isArr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "isMap");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    llvm::Value* cond1 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::ARRAY)});
    cond1 = builder.CreateZExt(cond1, builder.getInt8Ty());

    llvm::Value* cond2 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::HASH_MAP)});
    cond2 = builder.CreateZExt(cond2,builder.getInt8Ty());
    cond2 = builder.CreateShl(cond2, 1, "shl", true, true);

    auto num = builder.CreateOr(cond1, cond2);
    auto _switch = builder.CreateSwitch(num, errorBB, 3);
    _switch->addCase(builder.getInt8(1), isArray);
    _switch->addCase(builder.getInt8(2), isHashmap);

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

    auto phi = builder.CreatePHI(builder.getInt64Ty(), 2, "collectionget");
    phi->addIncoming(arrVal, isArray);
    phi->addIncoming(mapVal, isHashmap);
    return phi;

}
// TODO: this only works when stored value is a number, what about strings?
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
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *isArray = llvm::BasicBlock::Create(*ctx, "isArr", F);
    llvm::BasicBlock *isHashmap = llvm::BasicBlock::Create(*ctx, "isMap");
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    llvm::Value* cond1 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::ARRAY)});
    cond1 = builder.CreateZExt(cond1, builder.getInt8Ty());

    llvm::Value* cond2 = builder.CreateCall(safeGetFunc("isObjType"),{collection, builder.getInt8(+object::ObjType::HASH_MAP)});
    cond2 = builder.CreateZExt(cond2,builder.getInt8Ty());
    cond2 = builder.CreateShl(cond2, 1, "shl", true, true);

    auto num = builder.CreateOr(cond1, cond2);
    auto _switch = builder.CreateSwitch(num, errorBB, 3);
    _switch->addCase(builder.getInt8(1), isArray);
    _switch->addCase(builder.getInt8(2), isHashmap);

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

    auto phi = builder.CreatePHI(builder.getInt64Ty(), 2, "collectionset");
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
    llvm::PHINode *PN = builder.CreatePHI(builder.getInt64Ty(), 2, "condexpr.res");

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

    // first el: field index, second el: method index
    // Atleast one of these 2 is guaranteed to be -1 since methods and fields can't share names
    auto indicies = instGetUnoptIdx(klass, field);

    // Creates the switch which returns either method or field
    return unoptimizedInvoke(inst, indicies.first, indicies.second, klass, expr->field, args, expr->dbgInfo.method);
}

llvm::Value* Compiler::visitNewExpr(typedAST::NewExpr* expr) {
    auto& klass = classes[expr->className];
    string name = expr->className.substr(expr->className.rfind(".")+1, expr->className.size()-1);
    auto instSize = curModule->getDataLayout().getTypeAllocSize(klass.instTemplatePtr->getValueType());
    auto memptr = builder.CreateCall(safeGetFunc("gcAlloc"), {builder.getInt32(instSize)});
    builder.CreateMemCpy(memptr, memptr->getRetAlign(), klass.instTemplatePtr, klass.instTemplatePtr->getAlign(), instSize);
    auto inst = builder.CreateBitCast(memptr, namedTypes["ObjInstancePtr"]);
    auto address = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst, {builder.getInt32(0), builder.getInt32(3)});
    auto nextaddr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst, {builder.getInt32(1)});
    nextaddr = builder.CreateBitCast(nextaddr, llvm::PointerType::getUnqual(builder.getInt64Ty()));
    builder.CreateStore(nextaddr, address);
    inst = builder.CreateBitCast(inst, namedTypes["ObjPtr"]);
    inst = builder.CreateCall(safeGetFunc("encodeObj"), {inst});
    if(klass.ty->methods.contains(name)){
        std::pair<types::tyVarIdx, uInt64> fnty = klass.ty->methods[name];
        auto fn  = functions[typeEnv[fnty.first]];
        auto ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosure"], klass.methodArrPtr,
                                                                 builder.getInt32(fnty.second));
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
    // TODO: this relies on the fact that no errors are thrown and the stack is never unwinded, fix that
    auto enclosingFunction = currentFunction;
    currentFunction = createNewFunc(expr->fn->name, expr->fn->fnTy);

    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        auto tmp = builder.CreateCall(safeGetFunc("getFreevar"), {currentFunction->getArg(0), builder.getInt32(i)});
        variables.insert_or_assign(freevar.second->uuid, tmp);
    }

    declareFuncArgs(expr->fn->args);

    for(auto stmt : expr->fn->block.stmts){
        stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto lambda = currentFunction;
    currentFunction = enclosingFunction;

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&currentFunction->back());
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
    // Create the closure and put the freevars in it
    return builder.CreateCall(safeGetFunc("createClosure"), closureConstructorArgs);
}

llvm::Value* Compiler::visitRangeExpr(typedAST::RangeExpr* expr) {

}
llvm::Value* Compiler::visitFuncDecl(typedAST::FuncDecl* stmt) {
    // TODO: this relies on the fact that no errors are thrown and the stack is never unwinded, fix that
    auto enclosingFunction = currentFunction;
    currentFunction = createNewFunc(stmt->fn->name, stmt->fn->fnTy);

    declareFuncArgs(stmt->fn->args);

    for(auto s : stmt->fn->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = currentFunction;
    currentFunction = enclosingFunction;

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&currentFunction->back());
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
    // stmt->cond might be null if the for statement that got transformed into while stmt didn't have a condition
    llvm::Value* cond = builder.getInt1(true);
    if(stmt->cond) cond = builder.CreateCall(decodeFn, stmt->cond->codegen(this));

    llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*ctx, "while.loop", func);
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*ctx, "while.cond");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "while.merge");

    continueJumpDest.push(condBB);
    breakJumpDest.push(mergeBB);
    // If check before do-while
    builder.CreateCondBr(cond, loopBB, mergeBB);

    // Loop body
    builder.SetInsertPoint(loopBB);
    codegenBlock(stmt->loopBody);
    if(!stmt->loopBody.terminates){
        // Unconditional fallthrough to condition block
        builder.CreateBr(condBB);

        func->insert(func->end(), condBB);
        builder.SetInsertPoint(condBB);

        // Afterloop expression gets eval-ed here before the condition check
        if(stmt->afterLoopExpr) stmt->afterLoopExpr->codegen(this);
        // Eval the condition again
        // If stmt->cond is null we can reuse the true constant that's in cond
        // Otherwise reevaluate the condition
        if(stmt->cond) cond = builder.CreateCall(decodeFn, stmt->cond->codegen(this));

        // Conditional jump to beginning of body(if cond is true), or to the end of the loop
        builder.CreateCondBr(cond, loopBB, mergeBB);
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
    compVal = builder.CreateBitCast(compVal, builder.getInt64Ty());
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
    auto fieldsFunc = createFieldChooseFunc(stmt->fullName, stmt->fields);
    auto methodsFunc = createMethodChooseFunc(stmt->fullName, stmt->methods);
    auto subClassIdxStart = builder.getInt32(classHierarchy[stmt->fullName].first);
    auto subClassIdxEnd = builder.getInt32(classHierarchy[stmt->fullName].second);

    fastMap<string, llvm::Function*> methodDecl;
    vector<llvm::Constant*> methods(stmt->methods.size());
    for(auto p : stmt->methods){
        llvm::Function* methodFn = forwardDeclMethod(p.second.first);
        methodDecl[p.first] = methodFn;
        methods[p.second.second] = createMethodObj(p.second.first, methodFn);
    }
    llvm::GlobalVariable* methodArr = storeConstObj(llvm::ConstantArray::get(llvm::ArrayType::get(namedTypes["ObjClosure"],
                                                                   methods.size()), methods));

    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjClass"), {
            createConstObjHeader(+object::ObjType::CLASS), name, subClassIdxStart, subClassIdxEnd, methodsFunc,
            fieldsFunc, methodsLen, fieldsLen, methodArr});
    llvm::GlobalVariable* klass = new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                                           llvm::GlobalVariable::PrivateLinkage, obj);

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
        auto storedVal = builder.CreateLoad(builder.getInt64Ty(), fieldPtr);
        val = codegenBinaryAdd(storedVal, val, expr->dbgInfo.op);
        return val;
    }
    auto storedField = builder.CreateLoad(builder.getInt64Ty(), fieldPtr);
    if(!exprIsType(expr->toStore, types::getBasicType(types::TypeFlag::NUMBER))) {
        createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", expr->dbgInfo.op);
    }
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedField},
                           "valIsNum", "Expected a number, got {}", expr->dbgInfo.field);

    val = builder.CreateBitCast(decoupleSetOperation(storedField, val, expr->operationType, expr->dbgInfo.op),
                                builder.getInt64Ty());
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
    auto castlhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
    auto castrhs = builder.CreateBitCast(rhs, builder.getDoubleTy());
    auto numAddRes = castToVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
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
    auto phi = builder.CreatePHI(builder.getInt64Ty(), 2);
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
        lhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
        rhs = builder.CreateBitCast(rhs, builder.getDoubleTy());

        auto val = neg ? builder.CreateFCmpONE(lhs, rhs, "fcmpone") : builder.CreateFCmpOEQ(lhs, rhs, "fcmpoeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }
    else if(!exprIsType(expr1, numTy) && !exprIsType(expr2, numTy) && !exprIsType(expr1, anyTy) && !exprIsType(expr2, anyTy)){
        auto val = neg ? builder.CreateICmpNE(lhs, rhs, "icmpne") : builder.CreateICmpEQ(lhs, rhs, "icmpeq");
        return builder.CreateCall(safeGetFunc("encodeBool"), val);
    }

    llvm::Value* icmptmp;
    llvm::Value* fcmptmp;
    if(neg){
        icmptmp = builder.CreateICmpNE(lhs, rhs, "icmptmp");
        fcmptmp = builder.CreateFCmpONE(builder.CreateBitCast(lhs, builder.getDoubleTy()),
                                        builder.CreateBitCast(rhs, builder.getDoubleTy()), "fcmptmp");
    }else {
        icmptmp = builder.CreateICmpEQ(lhs, rhs, "icmptmp");
        fcmptmp = builder.CreateFCmpOEQ(builder.CreateBitCast(lhs, builder.getDoubleTy()),
                                        builder.CreateBitCast(rhs, builder.getDoubleTy()), "fcmptmp");
    }
    // If both values are numbers, use the floating comparison, if there is type mismatch/values are of some other type use icmp
    auto isnum = safeGetFunc("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);
    // To reduce branching on a common operation, select instruction is used
    auto sel = builder.CreateSelect(builder.CreateAnd(c1, c2), fcmptmp, icmptmp);
    return builder.CreateCall(safeGetFunc("encodeBool"), sel);
}
llvm::Value* Compiler::codegenNeg(const typedExprPtr _rhs, typedAST::UnaryOp op, Token dbg){
    llvm::Value* rhs = _rhs->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If rhs is known to be a number, no need for the type check
    if(exprIsType(_rhs, types::getBasicType(types::TypeFlag::NUMBER))){
        if(op == typedAST::UnaryOp::BIN_NEG){
            // Cast value to double, then convert to signed 64bit integer and negate
            auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
            auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"binnegtmp");
            // Cast back to double and then to 64bit int
            auto castToDouble = builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx));
            return builder.CreateBitCast(castToDouble, builder.getInt64Ty());
        }else{
            auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
            return builder.CreateBitCast(builder.CreateFNeg(tmp, "fnegtmp"), builder.getInt64Ty());
        }
    }

    createRuntimeTypeCheck(safeGetFunc("isNum"), {rhs},
                           "Operand must be a number, got '{}'.", "numneg", dbg);

    // For binary negation, the casting is as follows Value -> double -> int64 -> double -> Value
    if(op == typedAST::UnaryOp::BIN_NEG){
        // Cast value to double, then convert to signed 64bit integer and negate
        auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
        auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, builder.getInt64Ty()),"binnegtmp");
        // Cast back to double and then to 64bit int
        auto castToDouble = builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx));
        return builder.CreateBitCast(castToDouble, builder.getInt64Ty());
    }else{
        auto tmp = builder.CreateBitCast(rhs, builder.getDoubleTy());
        return builder.CreateBitCast(builder.CreateFNeg(tmp, "fnegtmp"), builder.getInt64Ty());
    }
    assert(false && "Unreachable");
    return nullptr;
}
void Compiler::codegenBlock(const typedAST::Block& block){
    for(auto stmt : block.stmts){
        stmt->codegen(this);
    }
}
llvm::Value * Compiler::codegenIncrement(const typedAST::UnaryOp op, const typedExprPtr expr) {
    if(expr->type == typedAST::NodeType::VAR_READ){
        return codegenVarIncrement(op, std::reinterpret_pointer_cast<typedAST::VarRead>(expr));
    }else if(expr->type == typedAST::NodeType::INST_GET){
        return codegenInstIncrement(op, std::reinterpret_pointer_cast<typedAST::InstGet>(expr));
    }
    // TODO: error
}
llvm::Value * Compiler::codegenVarIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::VarRead> expr) {
    llvm::Value* val = codegenVarRead(expr->varPtr);
    if(!exprIsType(expr, types::getBasicType(types::TypeFlag::NUMBER))){
        createRuntimeTypeCheck(safeGetFunc("isNum"), {val}, "isNum",
                               "Expected a number, got '{}'.", expr->dbgInfo.varName);
    }
    llvm::Value* res = builder.CreateBitCast(val, builder.getDoubleTy());
    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    else res = builder.CreateFSub(res, llvm::ConstantFP::get(builder.getDoubleTy(), 1.));
    res = builder.CreateBitCast(res, builder.getInt64Ty());
    codegenVarStore(expr->varPtr, res);
    return val;
}
llvm::Value * Compiler::codegenInstIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::InstGet> expr) {
    auto inst = expr->instance->codegen(this);
    llvm::Value* fieldPtr = nullptr;
    if(typeEnv[expr->instance->exprType]->type == types::TypeFlag::INSTANCE) {
        auto &klass = classes[std::reinterpret_pointer_cast<types::InstanceType>(typeEnv[expr->instance->exprType])->klass->name];
        fieldPtr = getOptInstFieldPtr(inst, klass, expr->field);
    }else{
        fieldPtr = getUnoptInstFieldPtr(inst, expr->field, expr->dbgInfo.field);
    }
    auto val = llvm::ConstantFP::get(builder.getDoubleTy(), 1.);
    llvm::Value* storedField = builder.CreateLoad(builder.getInt64Ty(), fieldPtr);
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedField},
                           "valIsNum", "Expected a number, got {}", expr->dbgInfo.field);
    llvm::Value* res = builder.CreateBitCast(storedField, builder.getDoubleTy());
    if(op == typedAST::UnaryOp::INC_POST) res = builder.CreateFAdd(res, val);
    else res = builder.CreateFSub(res, val);

    res = builder.CreateBitCast(res,builder.getInt64Ty());
    builder.CreateStore(val, fieldPtr);
    return storedField;
}

// Function codegen helpers
llvm::Function* Compiler::createNewFunc(const string name, const std::shared_ptr<types::FunctionType> fnTy){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    // First argument is always the closure structure
    params.push_back(llvm::PointerType::getUnqual(namedTypes["ObjClosure"]));
    for(int i = 0; i < fnTy->argCount; i++) params.push_back(builder.getInt64Ty());
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt64Ty(), params, false);
    auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, name, curModule.get());
    // Creates a connection between function types and functions
    functions.insert_or_assign(fnTy, tmp);
    tmp->setGC("statepoint-example");

    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmp);
    builder.SetInsertPoint(BB);
    return tmp;
}
llvm::FunctionType* Compiler::getFuncType(int argCount){
    vector<llvm::Type*> params;
    // First argument is always the closure structure
    params.push_back(llvm::PointerType::getUnqual(namedTypes["ObjClosure"]));
    for(int i = 0; i < argCount; i++) params.push_back(builder.getInt64Ty());
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt64Ty(), params, false);
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
            varPtr = builder.CreateAlloca(builder.getInt64Ty(), nullptr, var->dbgInfo.varName.getLexeme());
            builder.CreateStore(currentFunction->getArg(argIndex++), varPtr);
        }else{
            varPtr = builder.CreateCall(safeGetFunc("createFreevar"), std::nullopt, var->dbgInfo.varName.getLexeme());
            // first index: access to the structure that's being pointed to,
            // second index: access to the second field(64bit field for the value)
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], varPtr, idxList, "freevarAddr");
            builder.CreateStore(currentFunction->getArg(argIndex++), tmpEle);
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

    args.insert(args.begin(), closurePtr);

    return builder.CreateCall(func.second, func.first, args, "callres");
}

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
    closurePtr = builder.CreateCall(safeGetFunc("decodeClosure"), closurePtr);

    vector<llvm::Value*> args;
    // Every function contains the closure struct as the first argument
    args.push_back(closurePtr);
    for(auto arg : expr->args) args.push_back(arg->codegen(this));
    if(funcType->argCount != expr->args.size()){
        errorHandler::addCompileError(fmt::format("Function expects {} parameters, got {} arguments.", funcType->argCount, expr->args.size()),
                                      expr->dbgInfo.paren1);
        throw CompilerError("Incorrect number of arguments passed");
    }

    // If function is known and codegen-ed, call it and discard closurePtr
    if(fn) return builder.CreateCall(fn, args, "callres");

    // If a functions type is known, but it hasn't been codegen-ed yet use the object to get the func ptr
    // TODO: this can be optimized by reordering compilation order of functions
    std::pair<llvm::Value*, llvm::FunctionType*> func = getBitcastFunc(closurePtr, funcType->argCount);
    return builder.CreateCall(func.second, func.first, args, "callres");
}

// Array bounds checking
void Compiler::createArrBoundsCheck(llvm::Value* arr, llvm::Value* index, string errMsg, Token dbg){
    llvm::Value* upperbound = builder.CreateCall(safeGetFunc("getArrSize"), arr);
    auto cond = builder.CreateICmpUGE(index, upperbound);
    auto cond2 = builder.CreateICmpULT(index, builder.getInt64(0));
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
    auto num1 = builder.CreateBitCast(storedVal, builder.getDoubleTy());
    auto num2 = builder.CreateBitCast(newVal, builder.getDoubleTy());
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

    field = builder.CreateBitCast(field, builder.getDoubleTy());
    field = builder.CreateFPToUI(field, builder.getInt64Ty());
    createArrBoundsCheck(arr, field, "Index {} outside of array range.", dbg);
    arr = builder.CreateCall(safeGetFunc("getArrPtr"), arr);
    auto ptr = builder.CreateGEP(builder.getInt64Ty(), arr, field);
    return builder.CreateLoad(builder.getInt64Ty(), ptr);
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
    index = builder.CreateBitCast(index, builder.getDoubleTy());
    index = builder.CreateFPToUI(index, builder.getInt64Ty());
    createArrBoundsCheck(arr, index, "Index {} outside of array range.", dbg);
    arr = builder.CreateCall(safeGetFunc("getArrPtr"), arr);
    auto ptr = builder.CreateGEP(builder.getInt64Ty(), arr, index);
    if(opTy == typedAST::SetType::SET){
        builder.CreateStore(val, ptr);
        return val;
    }else if(opTy == typedAST::SetType::ADD_SET){
        // Special case because of strings
        auto storedVal = builder.CreateLoad(builder.getInt64Ty(), ptr);
        val = codegenBinaryAdd(storedVal, val, dbg);
        return val;
    }
    auto storedVal = builder.CreateLoad(builder.getInt64Ty(), ptr);
    if(!optVal) createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", dbg);
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedVal},
                           "valIsNum", "Expected a number, got {}", dbg);

    val = builder.CreateBitCast(decoupleSetOperation(storedVal, val, opTy, dbg), builder.getInt64Ty());
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
        val = codegenBinaryAdd(storedVal, val, dbg), builder.getInt64Ty();
        return val;
    }
    auto storedVal = builder.CreateCall(safeGetFunc("hashmapGetV"), {map, field});
    if(!optVal) createRuntimeTypeCheck(safeGetFunc("isNum"), {val},
                               "valIsNum", "Expected a number, got {}", dbg);
    createRuntimeTypeCheck(safeGetFunc("isNum"), {storedVal},
                           "valIsNum", "Expected a number, got {}", dbg);
    val = builder.CreateBitCast(decoupleSetOperation(storedVal, val, opTy, dbg), builder.getInt64Ty());
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
        llvm::Value* cmp = builder.CreateICmpEQ(compVal, val);
        // If comparison is successful BBIdx becomes the index of the block that the switch needs to jump to
        BBIdx = builder.CreateSelect(cmp, builder.getInt32(c.second), BBIdx);
    }
    return BBIdx;
}

// Class helpers
llvm::Function* Compiler::createFieldChooseFunc(string className, std::unordered_map<string, int>& fields){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt64Ty()}, false);
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
    builder.SetInsertPoint(&currentFunction->back());
    return fn;
}
llvm::Function* Compiler::createMethodChooseFunc(string className, std::unordered_map<string, std::pair<typedAST::ClassMethod, int>>& methods){
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt64Ty()}, false);
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
    builder.SetInsertPoint(&currentFunction->back());
    return fn;
}

llvm::Function* Compiler::forwardDeclMethod(typedAST::ClassMethod& method){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    // First argument is always the closure structure
    params.push_back(llvm::PointerType::getUnqual(namedTypes["ObjClosure"]));
    for(int i = 0; i < method.code->fnTy->argCount; i++) params.push_back(builder.getInt64Ty());
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt64Ty(), params, false);
    auto fn = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, method.code->name, curModule.get());
    // Creates a connection between function types and functions
    functions.insert_or_assign(method.code->fnTy, fn);
    fn->setGC("statepoint-example");
    return fn;
}
void Compiler::codegenMethod(typedAST::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd,
                             llvm::Function* methodFn){
    // TODO: this relies on the fact that no errors are thrown and the stack is never unwinded, fix that
    auto enclosingFunction = currentFunction;
    currentFunction = methodFn;
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", currentFunction);
    builder.SetInsertPoint(BB);

    declareFuncArgs(method.code->args);
    createRuntimeTypeCheck(safeGetFunc("isInstAndClass"),{currentFunction->getArg(1), subClassIdxStart, subClassIdxEnd},
                           "isInst", "Expected instance of class, got '{}'.", method.dbg.name);

    for(auto s : method.code->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = currentFunction;
    currentFunction = enclosingFunction;

    // Set insertion point to the end of the enclosing method
    builder.SetInsertPoint(&currentFunction->back());
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

llvm::GlobalVariable* Compiler::createInstanceTemplate(llvm::Constant* klass, int fieldN){
    vector<llvm::Constant*> fields(fieldN);
    std::fill(fields.begin(), fields.end(), builder.getInt64(MASK_SIGNATURE_NIL));
    llvm::Constant* fieldArr = llvm::ConstantArray::get(llvm::ArrayType::get(builder.getInt64Ty(), fieldN), fields);
    llvm::Constant* obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "ObjInstance"), {
            createConstObjHeader(+object::ObjType::INSTANCE), builder.getInt32(fieldN), klass,
            llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(builder.getInt64Ty()))
    });
    auto inst =  llvm::ConstantStruct::get(llvm::StructType::create(*ctx,{namedTypes["ObjInstance"],
                                                                    llvm::ArrayType::get(builder.getInt64Ty(), fieldN)}),
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
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);

        ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, {arrIdx});
        return builder.CreateLoad(builder.getInt64Ty(), ptr);
    }else{
        // Error
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

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);
    ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(builder.getInt64Ty(), ptr);
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);

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
    auto phi = builder.CreatePHI(builder.getInt64Ty(), 2);
    phi->addIncoming(field, fieldBB);
    phi->addIncoming(method, methodBB);
    return phi;
}

std::pair<llvm::Value*, llvm::Value*> Compiler::instGetUnoptIdx(llvm::Value* klass, llvm::Constant* field){
    auto fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt64Ty()}, false);
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
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(3)});
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);

        ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, {arrIdx});
        return ptr;
    }else{
        // Error
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

    auto fnTy = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt64Ty()}, false);
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

    ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);
    ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, fieldIdx);

    return ptr;
}

llvm::Value* Compiler::optimizeInvoke(llvm::Value* inst, string field, Class& klass, vector<llvm::Value*>& callArgs, Token dbg){
    if(klass.ty->methods.contains(field)){
        // Index into the array of ObjClosure contained in the class
        auto arrIdx = builder.getInt32(klass.ty->methods[field].second);

        // Doesn't actually access the array, but treats the pointer as a pointer to allocated ObjClosure
        auto closure = llvm::ConstantExpr::getInBoundsGetElementPtr(namedTypes["ObjClosure"], klass.methodArrPtr, arrIdx);
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
        auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,
                                             {builder.getInt32(0), builder.getInt32(3)});
        ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);

        ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, {arrIdx});
        auto closure = builder.CreateLoad(builder.getInt64Ty(), ptr);
        return createFuncCall(closure, callArgs, dbg);
    }else{
        // Error
    }
}

llvm::Value* Compiler::unoptimizedInvoke(llvm::Value* inst, llvm::Value* fieldIdx, llvm::Value* methodIdx, llvm::Value* klass,
                                         string fieldName, vector<llvm::Value*> args, Token dbg){
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

    auto ptr = builder.CreateInBoundsGEP(namedTypes["ObjInstance"], inst,{builder.getInt32(0), builder.getInt32(3)});
    ptr = builder.CreateLoad(llvm::PointerType::getUnqual(builder.getInt64Ty()), ptr);
    ptr = builder.CreateInBoundsGEP(builder.getInt64Ty(), ptr, fieldIdx);
    auto field =  builder.CreateLoad(builder.getInt64Ty(), ptr);
    auto callres1 = createFuncCall(field, args, dbg);
    builder.CreateBr(mergeBB);

    F->insert(F->end(), methodBB);
    builder.SetInsertPoint(methodBB);

    ptr = builder.CreateInBoundsGEP(namedTypes["ObjClass"], klass, {builder.getInt32(0), builder.getInt32(8)});
    llvm::Value* val = builder.CreateLoad(namedTypes["ObjClosurePtr"], ptr);
    val = builder.CreateInBoundsGEP(namedTypes["ObjClosure"], val, methodIdx);
    auto method = builder.CreateCall(safeGetFunc("encodeObj"), val);
    auto encodedInst = builder.CreateCall(safeGetFunc("encodeObj"), inst);
    args.insert(args.begin(), encodedInst);
    auto callres2 = createFuncCall(method, args, dbg);
    builder.CreateBr(mergeBB);

    F->insert(F->end(), errorBB);
    builder.SetInsertPoint(errorBB);

    builder.CreateCall(safeGetFunc("printf"), {createConstStr("Instance doesn't contain field or method %s"),
                                               createConstStr(fieldName)});
    builder.CreateCall(safeGetFunc("exit"), {builder.getInt32(64)});
    builder.CreateUnreachable();

    F->insert(F->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto phi = builder.CreatePHI(builder.getInt64Ty(), 2);
    phi->addIncoming(callres1, fieldBB);
    phi->addIncoming(callres2, methodBB);
    return phi;
}
// Misc
llvm::Value* Compiler::castToVal(llvm::Value* val){
    return builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}
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
            return createESLString(get<string>(constant));
        }
    }
    assert(false && "Unreachable");
}

int Compiler::getIP(){
    auto it = currentFunction->getEntryBlock().begin();
    int i = 0;
    while(it->getOpcode() == llvm::Instruction::Alloca){
        it++;
        i++;
    }
    return i;
}

llvm::GlobalVariable* Compiler::storeConstObj(llvm::Constant* obj){
    return new llvm::GlobalVariable(*curModule, obj->getType(), false,
                                    llvm::GlobalVariable::LinkageTypes::PrivateLinkage, obj, "internalConstObj");
}
llvm::Constant* Compiler::createConstObjHeader(int type){
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(*ctx, "Obj"),{builder.getInt8(type),
                                         builder.getInt1(true)});
}

llvm::Constant* Compiler::constObjToVal(llvm::Constant* obj){
    auto val = llvm::ConstantExpr::getPtrToInt(obj, builder.getInt64Ty());
    return llvm::ConstantExpr::getAdd(val, builder.getInt64(MASK_SIGNATURE_OBJ));
}

void Compiler::replaceGV(uInt64 uuid, llvm::Constant* newInit){
    auto gv = (llvm::dyn_cast<llvm::GlobalVariable>(variables.at(uuid)));
    llvm::GlobalVariable* newgv = new llvm::GlobalVariable(*curModule, builder.getInt64Ty(), false,
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
            return builder.CreateLoad(builder.getInt64Ty(), variables.at(varPtr->uuid), "loadlocal");
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* upvalPtr = variables.at(varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], upvalPtr, idxList, "freevarAddr");
            return builder.CreateLoad(builder.getInt64Ty(), tmpEle, "loadfreevar");
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:{
            return builder.CreateLoad(builder.getInt64Ty(), variables.at(varPtr->uuid), "loadgvar");
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
        vector<llvm::Type*> args(argc, builder.getInt64Ty());
        args.insert(args.begin(), namedTypes["ObjClosurePtr"]);
        auto func = llvm::Function::Create(llvm::FunctionType::get(builder.getInt64Ty(), args, false),
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

#pragma endregion
