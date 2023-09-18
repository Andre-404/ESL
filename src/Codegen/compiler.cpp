#include "compiler.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "LLVMHelperFunctions.h"
#include "valueHelpers.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include <unordered_set>
#include <iostream>

using namespace compileCore;

Compiler::Compiler(std::shared_ptr<typedAST::Function> _code, vector<File*>& _srcFiles, vector<vector<types::tyPtr>>& _tyEnv)
    : ctx(std::make_unique<llvm::LLVMContext>()), builder(llvm::IRBuilder<>(*ctx)) {
    sourceFiles = _srcFiles;
    typeEnv = _tyEnv;

    curModule = std::make_unique<llvm::Module>("Module", *ctx);
    continueJumpDest = nullptr;
    breakJumpDest = nullptr;
    advanceJumpDest = nullptr;
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

    compile(_code);
    llvmHelpers::runModule(curModule, JIT, ctx, true);
}

void Compiler::compile(std::shared_ptr<typedAST::Function> _code){
    llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx), false);
    auto tmpfn = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "func.main", curModule.get());
    tmpfn->setGC("statepoint-example");
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmpfn);
    builder.SetInsertPoint(BB);
    builder.CreateStore(builder.getInt8(0), curModule->getNamedGlobal("gcFlag"));
    currentFunction = tmpfn;
    for(auto stmt : _code->block.stmts){
        stmt->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Ends the main function
    builder.CreateRetVoid();
    llvm::verifyFunction(*tmpfn);
    curModule->print(llvm::errs(), nullptr);
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
            llvm::IRBuilder<> tempBuilder(&currentFunction->getEntryBlock(), currentFunction->getEntryBlock().begin());
            auto tmp = tempBuilder.CreateCall(curModule->getFunction("createFreevar"), std::nullopt, decl->dbgInfo.varName.getLexeme());
            variables.insert_or_assign(decl->uuid, tmp);
            break;
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL_CLASS:{
            string varName = decl->dbgInfo.varName.getLexeme() + std::to_string(decl->uuid);
            curModule->getOrInsertGlobal(varName, builder.getInt64Ty());
            llvm::GlobalVariable* gvar = curModule->getNamedGlobal(varName);
            gvar->setLinkage(llvm::GlobalVariable::PrivateLinkage);
            gvar->setAlignment(llvm::Align::Of<Value>());
            gvar->setInitializer(builder.getInt64(MASK_SIGNATURE_NIL));
            // Globals aren't on the stack, so they need to be marked for GC collection separately
            builder.CreateCall(curModule->getFunction("addGCRoot"), gvar);

            variables.insert_or_assign(decl->uuid, gvar);
            break;
        }
    }

    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitVarRead(typedAST::VarRead* expr) {
    switch(expr->varPtr->varType){
        case typedAST::VarType::LOCAL:{
            return builder.CreateLoad(builder.getInt64Ty(), variables.at(expr->varPtr->uuid), "loadlocal");
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* upvalPtr = variables.at(expr->varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], upvalPtr, idxList, "freevarAddr");
            return builder.CreateLoad(builder.getInt64Ty(), tmpEle, "loadfreevar");
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL_CLASS:{
            return builder.CreateLoad(builder.getInt64Ty(), variables.at(expr->varPtr->uuid), "loadgvar");
        }
    }
    assert(false && "Unreachable");
    return nullptr;
}
llvm::Value* Compiler::visitVarStore(typedAST::VarStore* expr) {
    llvm::Value* valToStore = expr->toStore->codegen(this);

    switch(expr->varPtr->varType){
        case typedAST::VarType::LOCAL:{
            builder.CreateStore(valToStore, variables.at(expr->varPtr->uuid));
            break;
        }
        case typedAST::VarType::FREEVAR:{
            llvm::Value* freevarPtr = variables.at(expr->varPtr->uuid);
            // first index: gets the "first element" of the memory being pointed to by upvalPtr(a single struct is there)
            // second index: gets the ref to the second element of the ObjFreevar struct
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], freevarPtr, idxList, "freevarAddr");
            builder.CreateStore(valToStore, tmpEle);
            break;
        }
        case typedAST::VarType::GLOBAL:
        case typedAST::VarType::GLOBAL_FUNC:
        case typedAST::VarType::GLOBAL_CLASS:{
            builder.CreateStore(valToStore, variables.at(expr->varPtr->uuid));
            break;
        }
    }
    return valToStore;
}
llvm::Value* Compiler::visitVarReadNative(typedAST::VarReadNative* expr) {
    // TODO: implement this
}

static bool isFloatingPointOp(typedAST::ArithmeticOp op){
    return op == typedAST::ArithmeticOp::SUB || op == typedAST::ArithmeticOp::MUL ||
           op == typedAST::ArithmeticOp::DIV || op == typedAST::ArithmeticOp::MOD;
}

llvm::Value* Compiler::visitArithmeticExpr(typedAST::ArithmeticExpr* expr) {
    if(expr->opType == typedAST::ArithmeticOp::ADD) return codegenBinaryAdd(expr->lhs, expr->rhs, expr->dbgInfo.op);

    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();

    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprConstrainedToType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        // If either or both aren't numbers, go to error, otherwise proceed as normal
        llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
        llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "binopexecute");

        auto isnum = curModule->getFunction("isNum");
        auto c1 = builder.CreateCall(isnum, lhs);
        auto c2 = builder.CreateCall(isnum, rhs);
        builder.CreateCondBr(builder.CreateNot(builder.CreateAnd(c1, c2)), errorBB, executeOpBB);

        // Calls the type error function which throws
        builder.SetInsertPoint(errorBB);
        createTyErr("Operands must be numbers, got '{}' and '{}'.", lhs, rhs, expr->dbgInfo.op);
        // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
        builder.CreateBr(executeOpBB);
        F->insert(F->end(), executeOpBB);
        // Actual operation goes into this block
        builder.SetInsertPoint(executeOpBB);
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
        case typedAST::ArithmeticOp::SUB: val = builder.CreateFSub(lhs, rhs, "fsub"); break;
        case typedAST::ArithmeticOp::MUL: val = builder.CreateFMul(lhs, rhs, "fmul"); break;
        case typedAST::ArithmeticOp::DIV: val = builder.CreateFDiv(lhs, rhs, "fdiv"); break;
        case typedAST::ArithmeticOp::MOD: val = builder.CreateFRem(lhs, rhs, "frem"); break;
        case typedAST::ArithmeticOp::AND: val = builder.CreateAnd(ilhs, irhs, "and"); break;
        case typedAST::ArithmeticOp::OR: val = builder.CreateOr(ilhs, irhs, "or"); break;
        case typedAST::ArithmeticOp::XOR: val =builder.CreateXor(ilhs, irhs, "xor"); break;
        case typedAST::ArithmeticOp::BITSHIFT_L: val =builder.CreateShl(ilhs, irhs, "shl"); break;
        case typedAST::ArithmeticOp::BITSHIFT_R: val =builder.CreateAShr(ilhs, irhs, "ashr"); break;
        case typedAST::ArithmeticOp::IDIV: {
            auto tmp1 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, lhs);
            auto tmp2 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, rhs);
            val = builder.CreateFDiv(tmp1, tmp2, "floordivtmp");
            break;
        }
        case typedAST::ArithmeticOp::ADD: assert(false && "Unreachable");
    }

    if(!isFloatingPointOp(expr->opType)) val = builder.CreateSIToFP(val, builder.getDoubleTy());
    return builder.CreateBitCast(val, builder.getInt64Ty());
}
llvm::Value* Compiler::visitComparisonExpr(typedAST::ComparisonExpr* expr) {
    if(expr->opType == typedAST::ComparisonOp::OR || expr->opType == typedAST::ComparisonOp::AND){
        return codegenLogicOps(expr->lhs, expr->rhs, expr->opType);
    }
    else if(expr->opType == typedAST::ComparisonOp::EQUAL || expr->opType == typedAST::ComparisonOp::NOT_EQUAL){
        return codegenCmp(expr->lhs, expr->rhs, expr->opType == typedAST::ComparisonOp::NOT_EQUAL);
    }
    else if(expr->opType == typedAST::ComparisonOp::INSTANCEOF){
        // TODO: implement this
    }

    llvm::Value* lhs = expr->lhs->codegen(this);
    llvm::Value* rhs = expr->rhs->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();

    // If both lhs and rhs are known to be numbers at compile time there's no need for runtime checks
    if(!exprConstrainedToType(expr->lhs, expr->rhs, types::getBasicType(types::TypeFlag::NUMBER))) {
        // If either or both aren't numbers, go to error, otherwise proceed as normal
        llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
        llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "binopexecute");

        auto isnum = curModule->getFunction("isNum");
        auto c1 = builder.CreateCall(isnum, lhs);
        auto c2 = builder.CreateCall(isnum, rhs);
        builder.CreateCondBr(builder.CreateNot(builder.CreateAnd(c1, c2)), errorBB, executeOpBB);

        // Calls the type error function which throws
        builder.SetInsertPoint(errorBB);
        createTyErr("Operands must be numbers, got '{}' and '{}'.", lhs, rhs, expr->dbgInfo.op);
        // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
        builder.CreateBr(executeOpBB);
        F->insert(F->end(), executeOpBB);
        // Actual operation goes into this block
        builder.SetInsertPoint(executeOpBB);
    }

    lhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
    rhs = builder.CreateBitCast(rhs, builder.getDoubleTy());
    llvm::Value* val;

    switch(expr->opType){
        case typedAST::ComparisonOp::LESS: val = builder.CreateFCmpOLT(lhs, rhs, "olttmp"); break;
        case typedAST::ComparisonOp::LESSEQ: val = builder.CreateFCmpOLE(lhs, rhs, "oletmp"); break;
        case typedAST::ComparisonOp::GREAT: val = builder.CreateFCmpOGT(lhs, rhs, "ogttmp"); break;
        case typedAST::ComparisonOp::GREATEQ: val = builder.CreateFCmpOGE(lhs, rhs, "ogetmp"); break;
        default: assert(false && "Unreachable");
    }
    return builder.CreateCall(curModule->getFunction("encodeBool"), val);
}
llvm::Value* Compiler::visitUnaryExpr(typedAST::UnaryExpr* expr) {
    // TODO: implement incrementing
    if(expr->opType == typedAST::UnaryOp::NEG){
        llvm::Value* rhs = expr->rhs->codegen(this);
        // If type is known to be a bool skip the runtime check and just execute the expr
        if(exprConstrainedToType(expr->rhs, types::getBasicType(types::TypeFlag::BOOL))) {
            return builder.CreateXor(rhs, builder.getInt64(MASK_TYPE_TRUE));
        }
        llvm::Function *F = builder.GetInsertBlock()->getParent();
        // If rhs isn't of the correct type, go to error, otherwise proceed as normal
        llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
        // Name is set in the switch
        llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, "negbool");

        builder.CreateCondBr(builder.CreateNot(builder.CreateCall(curModule->getFunction("isBool"), rhs)), errorBB, executeOpBB);
        // Calls the type error function which throws
        builder.SetInsertPoint(errorBB);
        createTyErr("Operand must be a boolean, got '{}'.", rhs, expr->dbgInfo.op);
        // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
        builder.CreateBr(executeOpBB);

        F->insert(F->end(), executeOpBB);
        builder.SetInsertPoint(executeOpBB);
        // Quick optimization, instead of decoding bool, negating and encoding, we just XOR with the true flag
        // Since that's just a single bit flag that's flipped on when true and off when false
        // This does rely on the fact that true and false are the first flags and are thus represented with 00 and 01
        return builder.CreateXor(rhs, builder.getInt64(MASK_TYPE_TRUE));
    }else if(expr->opType == typedAST::UnaryOp::FNEG || expr->opType == typedAST::UnaryOp::BIN_NEG){
        return codegenNeg(expr->rhs, expr->opType, expr->dbgInfo.op);
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
        case 2: return builder.getInt64(MASK_SIGNATURE_NIL);;
        case 3: {
            auto str = createConstStr(get<string>(expr->val));
            return builder.CreateCall(curModule->getFunction("createStr"), str);
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
        args.push_back(builder.CreateCall(curModule->getFunction("createStr"), createConstStr(entry.first)));
        args.push_back(entry.second->codegen(this));
    }

    return builder.CreateCall(curModule->getFunction("createHashMap"), args);
}
llvm::Value* Compiler::visitArrayExpr(typedAST::ArrayExpr* expr) {
    vector<llvm::Value*> vals;
    for(auto mem : expr->fields){
        vals.push_back(mem->codegen(this));
    }
    auto arrNum = builder.getInt32(vals.size());
    auto arr = builder.CreateCall(curModule->getFunction("createArr"), arrNum, "array");
    auto arrPtr = builder.CreateCall(curModule->getFunction("getArrPtr"), arr, "arrptr");
    for(int i = 0; i < vals.size(); i++){
        auto gep = builder.CreateInBoundsGEP(builder.getInt64Ty(), arrPtr, builder.getInt32(i));
        builder.CreateStore(vals[i], gep);
    }
    return arr;
}

llvm::Value* Compiler::visitCollectionGet(typedAST::CollectionGet* expr) {

}
llvm::Value* Compiler::visitCollectionSet(typedAST::CollectionSet* expr) {

}

llvm::Value* Compiler::visitConditionalExpr(typedAST::ConditionalExpr* expr) {
    auto condtmp = expr->cond->codegen(this);
    llvm::Value* cond = nullptr;
    // If condition is known to be a boolean the isNull check can be skipped
    if(exprConstrainedToType(expr->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(curModule->getFunction("decodeBool"), condtmp);
    }
    else cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);

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
    llvm::Function* fn;
    // Optimize if this is a native var read
    if(expr->callee->type == typedAST::NodeType::VAR_NATIVE_READ){
        auto native = std::reinterpret_pointer_cast<typedAST::VarReadNative>(expr->callee);
        fn = nativeFunctions[native->nativeName];
    }
    auto funcType = getFuncFromType(expr->callee->exprType);
    // Function might not be codegen-ed at this point, if it's not then just use its func type to do static arg count check
    if(functions.contains(funcType)) fn = functions[funcType];

    // TODO: remove this if there are no codegen-ing this has no side effects
    auto closureObj = expr->callee->codegen(this);
    vector<llvm::Value*> args;
    for(auto arg : expr->args) args.push_back(arg->codegen(this));
    // If function is known and codegen-ed, call it and discard closureObj
    if(fn){
        return builder.CreateCall(fn, args, "callres");
    }
    // TODO: implement calling when function is not compiled/known
}
llvm::Value* Compiler::visitInvokeExpr(typedAST::InvokeExpr* expr) {

}

llvm::Value* Compiler::visitNewExpr(typedAST::NewExpr* expr) {

}

llvm::Value* Compiler::visitAsyncExpr(typedAST::AsyncExpr* expr) {

}
llvm::Value* Compiler::visitAwaitExpr(typedAST::AwaitExpr* expr) {

}

llvm::Value* Compiler::visitCreateClosureExpr(typedAST::CreateClosureExpr* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    auto enclosingFunction = currentFunction;
    currentFunction = createNewFunc(expr->fn->args.size() + (expr->freevars.size() > 0 ? 1 : 0), expr->fn->name, expr->fn->fnTy);

    // Essentially pushes all freevars to the machine stack, the pointer to ObjFreevar is stored in the vector 'freevars'
    for(int i = 0; i < expr->freevars.size(); i++){
        auto& freevar = expr->freevars[i];
        auto tmp = builder.CreateCall(curModule->getFunction("getFreevar"), {currentFunction->getArg(0), builder.getInt32(i)});
        variables.insert_or_assign(freevar.second->uuid, tmp);
    }

    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    // If a closure is passed(as the first arg), skip over it
    int argIndex = expr->freevars.size() > 0 ? 1 : 0;
    for (auto var : expr->fn->args) {
        llvm::Value* varPtr;
        // Don't need to use temp builder when creating alloca since this happens in the first basicblock of the function
        if(var->varType == typedAST::VarType::LOCAL){
            varPtr = builder.CreateAlloca(builder.getInt64Ty(), nullptr, var->dbgInfo.varName.getLexeme());
            builder.CreateStore(currentFunction->getArg(argIndex++), varPtr);
        }else{
            varPtr = builder.CreateCall(curModule->getFunction("createFreevar"), std::nullopt, var->dbgInfo.varName.getLexeme());
            // first index: access to the structure that's being pointed to,
            // second index: access to the second field(64bit field for the value)
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], varPtr, idxList, "freevarAddr");
            builder.CreateStore(currentFunction->getArg(argIndex++), tmpEle);
        }
        // Insert the argument into the pool of variables
        variables.insert_or_assign(var->uuid, varPtr);
    }

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
    return builder.CreateCall(curModule->getFunction("createClosure"), closureConstructorArgs);
}

llvm::Value* Compiler::visitRangeExpr(typedAST::RangeExpr* expr) {

}
llvm::Value* Compiler::visitFuncDecl(typedAST::FuncDecl* stmt) {
    auto enclosingFunction = currentFunction;
    currentFunction = createNewFunc(stmt->fn->args.size(), stmt->fn->name, stmt->fn->fnTy);

    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // We just assign those positions to each arg
    int argIndex = 0;
    for (auto var : stmt->fn->args) {
        llvm::Value* varPtr;
        // Don't need to use temp builder when creating alloca since this happens in the first basicblock of the function
        if(var->varType == typedAST::VarType::LOCAL){
            varPtr = builder.CreateAlloca(builder.getInt64Ty(), nullptr, stmt->dbgInfo.params[argIndex].getLexeme());
            builder.CreateStore(currentFunction->getArg(argIndex++), varPtr);
        }else{
            varPtr = builder.CreateCall(curModule->getFunction("createFreevar"), std::nullopt, var->dbgInfo.varName.getLexeme());
            // first index: access to the structure that's being pointed to,
            // second index: access to the second field(64bit field for the value)
            vector<llvm::Value*> idxList = {builder.getInt32(0), builder.getInt32(1)};
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjFreevar"], varPtr, idxList, "freevarAddr");
            builder.CreateStore(currentFunction->getArg(argIndex++), tmpEle);
        }
        // Insert the argument into the pool of variables
        variables.insert_or_assign(var->uuid, varPtr);
    }

    for(auto s : stmt->fn->block.stmts){
        s->codegen(this); // Codegen of statements returns nullptr, so we can safely discard it
    }

    // Enclosing function become the active one, the function that was just compiled is stored in fn
    auto fn = currentFunction;
    currentFunction = enclosingFunction;

    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&currentFunction->back());
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(fn, builder.getInt8PtrTy());
    auto arity = builder.getInt8(stmt->fn->args.size());
    auto name = createConstStr(stmt->fn->name);

    // Every function is converted to a closure(if even it has 0 freevars) for ease of use when calling
    // Since this is a global function declaration number of freevars is always going to be 0
    vector<llvm::Value*> closureConstructorArgs = {typeErasedFn, arity, name, builder.getInt32(0)};

    // Create the closure
    auto tmp = builder.CreateCall(curModule->getFunction("createClosure"), closureConstructorArgs);
    // Store closure in var
    builder.CreateStore(tmp, variables.at(stmt->globalVarUuid));
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitReturnStmt(typedAST::ReturnStmt* stmt) {
    builder.CreateRet(stmt->expr->codegen(this));
    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitUncondJump(typedAST::UncondJump* stmt) {
    switch(stmt->jmpType){
        case typedAST::JumpType::BREAK: builder.CreateBr(breakJumpDest); break;
        case typedAST::JumpType::CONTINUE: builder.CreateBr(continueJumpDest); break;
        case typedAST::JumpType::ADVANCE: builder.CreateBr(advanceJumpDest); break;
    }
    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitIfStmt(typedAST::IfStmt* stmt) {
    auto condtmp = stmt->cond->codegen(this);
    llvm::Value* cond;
    if(exprConstrainedToType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL))){
        cond = builder.CreateCall(curModule->getFunction("decodeBool"), condtmp);
    }else cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);

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
    bool canOptimize = stmt->cond ? exprConstrainedToType(stmt->cond, types::getBasicType(types::TypeFlag::BOOL)) : true;
    auto decodeFn = canOptimize ? curModule->getFunction("decodeBool") : curModule->getFunction("isTruthy");

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    // stmt->cond might be null if the for statement that got transformed into while stmt didn't have a condition
    llvm::Value* cond = builder.getInt1(true);
    if(stmt->cond) cond = builder.CreateCall(decodeFn, stmt->cond->codegen(this));

    llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*ctx, "while.loop", func);
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*ctx, "while.cond");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "while.merge");

    // Sets the destination blocks for both continue and break, works like a stack
    auto continueJumpDestTmp = continueJumpDest;
    continueJumpDest = condBB;
    auto breakJumpDestTmp = breakJumpDest;
    breakJumpDest = mergeBB;
    // If check before do-while
    builder.CreateCondBr(cond, loopBB, mergeBB);

    // Loop body
    builder.SetInsertPoint(loopBB);
    codegenBlock(stmt->loopBody);
    if(!stmt->loopBody.terminates){
        // Unconditional fallthrough to condition block
        builder.CreateBr(condBB);

        // Only compile the condition if the loop bb hasn't been terminated, no point in compiling it if it has no predecessors
        func->insert(func->end(), condBB);
        builder.SetInsertPoint(condBB);

        // Afterloop expression gets eval-ed here before the condition check
        if(stmt->afterLoopExpr) stmt->afterLoopExpr->codegen(this);
        // Eval the condition again
        // If stmt->cond is null we can reuse the true constant that's in cond
        if(stmt->cond) cond = builder.CreateCall(decodeFn, stmt->cond->codegen(this));

        // Conditional jump to beginning of body(if cond is true), or to the end of the loop
        builder.CreateCondBr(cond, loopBB, mergeBB);
    }

    func->insert(func->end(), mergeBB);
    // Sets the builder up to emit code after the while stmt
    builder.SetInsertPoint(mergeBB);
    // Pop destinations so that any break/continue for an outer loop works correctly
    continueJumpDest = continueJumpDestTmp;
    breakJumpDest = breakJumpDestTmp;

    return nullptr; // Stmts return nullptr on codegen
}
llvm::Value* Compiler::visitSwitchStmt(typedAST::SwitchStmt* stmt) {


    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitClassDecl(typedAST::ClassDecl* stmt) {

    return nullptr; // Stmts return nullptr on codegen
}

llvm::Value* Compiler::visitInstGet(typedAST::InstGet* expr) {

}
llvm::Value* Compiler::visitInstSuperGet(typedAST::InstSuperGet* expr) {

}
llvm::Value* Compiler::visitInstSet(typedAST::InstSet* expr) {

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
}

// This shouldn't ever be visited as every macro should be expanded before compilation
void Compiler::visitMacroExpr(AST::MacroExpr* expr) {
    error("Non-expanded macro encountered during compilation.");
}

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

void Compiler::visitFuncDecl(AST::FuncDecl* decl) {
    string name = declareGlobalVar(decl->getName());
    // Defining the function here to allow for recursion
    defineGlobalVar(name, builder.getInt64(encodeNil()));
    // Creating a new compilerInfo sets us up with a clean slate for writing bytecode, the enclosing functions info
    //is stored in parserCurrent->enclosing
    createNewFunc(decl->arity, decl->getName().getLexeme(), FuncType::TYPE_FUNC);
    // No need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    // We define the args as locals, when the function is called, the args will be sitting on the stack in order
    // we just assign those positions to each arg
    int i = 0;
    for (AST::ASTVar& var : decl->args) {
        declareLocalVar(var);
        defineLocalVar(current->func->getArg(i++));
    }
    for(auto stmt : decl->body->statements){
        try {
            stmt->accept(this);
        }catch(CompilerException e){

        }
    }
    llvm::Value* func = endFuncDecl(decl->args.size(), decl->getName().getLexeme());
    // Store directly to global var since name is the full symbol
    builder.CreateStore(func, globals.at(name).val);
}
/*
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
/*
// Applies loop inversion
void Compiler::visitWhileStmt(AST::WhileStmt* stmt) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    auto condtmp = evalASTExpr(stmt->condition);
    auto cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);

    llvm::BasicBlock* loopBB = llvm::BasicBlock::Create(*ctx, "while.loop", func);
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*ctx, "while.cond");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "while.merge");

    // Sets the destination blocks for both continue and break, works like a stack
    continueJumpDest.push_back(condBB);
    breakJumpDest.push_back(mergeBB);


    // If check before do-while
    builder.CreateCondBr(cond, loopBB, mergeBB);

    // Loop body
    builder.SetInsertPoint(loopBB);
    stmt->body->accept(this);
    // If the current BB ends with a break/continue/advance/return, don't emit the final break since this basic block is already terminated
    if(BBTerminated) {
        // Reset flag since the break/continue/advance/return statement has been handled
        BBTerminated = false;
    }
    else {
        // Unconditional fallthrough to condition block
        builder.CreateBr(condBB);

        // Only compile the condition if the loop bb hasn't been terminated, no point in compiling it if it has no predecessors
        func->insert(func->end(), condBB);
        builder.SetInsertPoint(condBB);
        // Eval the condition again
        condtmp = evalASTExpr(stmt->condition);
        cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);
        // Conditional jump to beginning of body(if cond is true), or to the end of the loop
        builder.CreateCondBr(cond, loopBB, mergeBB);
    }

    func->insert(func->end(), mergeBB);
    // Sets the builder up to emit code after the while stmt
    builder.SetInsertPoint(mergeBB);
    // Pop destinations so that any break/continue for an outer loop works correctly
    continueJumpDest.pop_back();
    breakJumpDest.pop_back();
}
*//*
void Compiler::visitSwitchStmt(AST::SwitchStmt* stmt) {
    /*current->scopeWithSwitch.push_back(current->scopeDepth);
    //compile the expression in parentheses
    stmt->expr->accept(this);
    vector<uint16_t> constants;
    vector<uint16_t> jumps;
    bool isLong = false;
    for (const auto & _case : stmt->cases) {
        //a single case can contain multiple constants(eg. case 1 | 4 | 9:), each constant is compiled and its jump will point to the
        //same case code block
        for (const Token& constant : _case->constants) {
            Value val;
            updateLine(constant);
            //create constant and add it to the constants array
            try {
                switch (constant.type) {
                    case TokenType::NUMBER: {
                        double num = std::stod(constant.getLexeme());//doing this becuase stod doesn't accept string_view
                        val = encodeNumber(num);
                        break;
                    }
                    case TokenType::TRUE: val = encodeBool(true); break;
                    case TokenType::FALSE: val = encodeBool(false); break;
                    case TokenType::NIL: val = encodeNil(); break;
                    case TokenType::STRING: {
                        //this gets rid of quotes, "Hello world"->Hello world
                        string temp = constant.getLexeme();
                        temp.erase(0, 1);
                        temp.erase(temp.size() - 1, 1);
                        val = encodeObj(ObjString::createStr(temp));
                        break;
                    }
                    default: {
                        error(constant, "Case expression can only be a constant.");
                    }
                }
                constants.push_back(makeConstant(val));
                if (constants.back() > SHORT_CONSTANT_LIMIT) isLong = true;
            }
            catch (CompilerException e) {}
        }
    }
    //the arguments for a switch op code are:
    //16-bit number n of case constants
    //n 8 or 16 bit numbers for each constant
    //n + 1 16-bit numbers of jump offsets(default case is excluded from constants, so the number of jumps is the number of constants + 1)
    //the default jump offset is always the last
    if (isLong) {
        emitByteAnd16Bit(+OpCode::SWITCH_LONG, constants.size());
        for (uInt16 constant : constants) {
            emit16Bit(constant);
        }
    }
    else {
        emitByteAnd16Bit(+OpCode::SWITCH, constants.size());
        for (uInt16 constant : constants) {
            emitByte(constant);
        }
    }

    for (int i = 0; i < constants.size(); i++) {
        jumps.push_back(getChunk()->bytecode.size());
        emit16Bit(0xffff);
    }
    //default jump
    jumps.push_back(getChunk()->bytecode.size());
    emit16Bit(0xffff);

    //at the end of each case is a implicit break
    vector<uint32_t> implicitBreaks;

    //compile the code of all cases, before each case update the jump for that case to the parserCurrent ip
    int i = 0;
    for (const std::shared_ptr<AST::CaseStmt>& _case : stmt->cases) {
        if (_case->caseType.getLexeme() == "default") {
            patchJump(jumps[jumps.size() - 1]);
        }
        else {
            //a single case can contain multiple constants(eg. case 1 | 4 | 9:), need to update jumps for each constant
            int constantNum = _case->constants.size();
            for (int j = 0; j < constantNum; j++) {
                patchJump(jumps[i]);
                i++;
            }
        }
        //new scope because patchScopeJumps only looks at scope deeper than the one it's called at
        beginScope();
        _case->accept(this);
        endScope();
        //end scope takes care of freevars
        implicitBreaks.push_back(emitJump(+OpCode::JUMP));
        //patch advance after the implicit break jump for fallthrough
        patchScopeJumps(ScopeJumpType::ADVANCE);
    }
    //if there is no default case the default jump goes to the end of the switch stmt
    if (!stmt->hasDefault) jumps[jumps.size() - 1] = getChunk()->bytecode.size();

    //all implicit breaks lead to the end of the switch statement
    for (uInt jmp : implicitBreaks) {
        patchJump(jmp);
    }
    current->scopeWithSwitch.pop_back();
    patchScopeJumps(ScopeJumpType::BREAK);
}
*/
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


void Compiler::createRuntimeErrCall(string fmtErr, std::vector<llvm::Value*> args, int exitCode){
    llvm::Constant* str = createConstStr(fmtErr);
    auto argNum = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), args.size());
    auto alloca = builder.CreateAlloca(llvm::Type::getInt8PtrTy(*ctx), argNum, "allocArr");
    // Gets size(in bytes) of a pointer
    int ptrSize = curModule->getDataLayout().getTypeAllocSize(llvm::Type::getInt8PtrTy(*ctx));
    builder.CreateLifetimeStart(alloca, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), ptrSize * args.size()));
    for(int i = 0; i < args.size(); i++){
        auto gep = builder.CreateInBoundsGEP(llvm::Type::getInt8PtrTy(*ctx), alloca, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), i));
        builder.CreateStore(args[i], gep);
    }
    auto val = builder.CreateBitCast(alloca, llvm::PointerType::getUnqual(llvm::Type::getInt8PtrTy(*ctx)));
    builder.CreateCall(curModule->getFunction("runtimeErr"), {str, val, argNum});
    builder.CreateLifetimeEnd(alloca, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), ptrSize * args.size()));
}

llvm::Constant* Compiler::createConstStr(const string& str){
    if(stringConstants.contains(str)) return stringConstants[str];
    auto constant = builder.CreateGlobalStringPtr(str, "internalString", 0, curModule.get());
    stringConstants[str] = constant;
    return constant;
}

void Compiler::createTyErr(const string err, llvm::Value* const val, const Token token){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(token.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(token.str.line);
    builder.CreateCall(curModule->getFunction("tyErrSingle"), {str, file, line, val});
}
void Compiler::createTyErr(const string err, llvm::Value* const lhs, llvm::Value* const rhs, const Token token){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(token.str.sourceFile->path);
    llvm::Constant* line = builder.getInt32(token.str.line);
    builder.CreateCall(curModule->getFunction("tyErrDouble"), {str, file, line, lhs, rhs});
}

llvm::Value* Compiler::castToVal(llvm::Value* val){
    return builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}

llvm::Function* Compiler::createNewFunc(const int argCount, const string name, const std::shared_ptr<types::FunctionType> fnTy){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    for(int i = 0; i < argCount; i++) params.push_back(builder.getInt64Ty());
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt64Ty(), params, false);
    auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, name, curModule.get());
    // Creates a connection between function types and functions
    functions.insert_or_assign(fnTy, tmp);
    tmp->setGC("statepoint-example");
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", tmp);
    builder.SetInsertPoint(BB);
    return tmp;
}

bool Compiler::exprConstrainedToType(const typedExprPtr expr, const types::tyPtr ty){
    if(typeEnv[expr->exprType].size() == 1){
        // If this expression is constrained to a single type it's safe to do typeArr[0]
        return typeEnv[expr->exprType][0] == ty;
    }
    return false;
}

bool Compiler::exprConstrainedToType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty){
    return exprConstrainedToType(expr1, ty) && exprConstrainedToType(expr2, ty);
}

llvm::Value* Compiler::codegenBinaryAdd(const typedExprPtr expr1, const typedExprPtr expr2, const Token op){
    llvm::Value* lhs = expr1->codegen(this);
    llvm::Value* rhs = expr2->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();

    // If types of both lhs and rhs are known unnecessary runtime checks are skipped
    if(exprConstrainedToType(expr1, expr2, types::getBasicType(types::TypeFlag::NUMBER))){
        auto castlhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
        auto castrhs = builder.CreateBitCast(rhs, builder.getDoubleTy());
        return castToVal(builder.CreateFAdd(castlhs, castrhs, "addtmp"));
    }else if(exprConstrainedToType(expr1, expr2, types::getBasicType(types::TypeFlag::STRING))){
        return builder.CreateCall(curModule->getFunction("strAdd"), {lhs, rhs});
    }

    // If both are a number go to addNum, if not try adding as string
    // If both aren't strings, throw error(error is thrown inside strTryAdd C++ function)
    llvm::BasicBlock *addNumBB = llvm::BasicBlock::Create(*ctx, "addnum", F);
    llvm::BasicBlock *addStringBB = llvm::BasicBlock::Create(*ctx, "addstring");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

    // Call isNum on both values and && the results
    auto isnum = curModule->getFunction("isNum");
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
    auto stringAddRes = builder.CreateCall(curModule->getFunction("strTryAdd"), {lhs, rhs, file, line});
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

    bool canOptimize = exprConstrainedToType(expr1, expr2, types::getBasicType(types::TypeFlag::BOOL));
    auto castToBool = canOptimize ? curModule->getFunction("decodeBool") : curModule->getFunction("isTruthy");

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
    return builder.CreateCall(curModule->getFunction("encodeBool"), PN);
}

bool Compiler::exprWithoutType(const typedExprPtr expr, const types::tyPtr ty){
    for(auto type : typeEnv[expr->exprType]){
        if(type->type == types::TypeFlag::ANY || type == ty) return false;
    }
    return true;
}

bool Compiler::exprWithoutType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty){
    return exprWithoutType(expr1, ty) && exprWithoutType(expr2, ty);
}

llvm::Value* Compiler::codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg){
    llvm::Value* lhs = expr1->codegen(this);
    llvm::Value* rhs = expr2->codegen(this);
    // Numbers have to be compared using fcmp for rounding reasons,
    // other value types are compared as 64 bit ints since every object is unique(strings are interned)

    // Optimizations if types are known, no need to do runtime checks
    if(exprConstrainedToType(expr1, expr2, types::getBasicType(types::TypeFlag::NUMBER))){
        // fcmp when both values are numbers
        lhs = builder.CreateBitCast(lhs, builder.getDoubleTy());
        rhs = builder.CreateBitCast(rhs, builder.getDoubleTy());

        auto val = neg ? builder.CreateFCmpONE(lhs, rhs, "fcmpone") : builder.CreateFCmpOEQ(lhs, rhs, "fcmpoeq");
        return builder.CreateCall(curModule->getFunction("encodeBool"), val);
    }
    else if(exprWithoutType(expr1, expr2, types::getBasicType(types::TypeFlag::NUMBER))){
        auto val = neg ? builder.CreateICmpNE(lhs, rhs, "icmpne") : builder.CreateICmpEQ(lhs, rhs, "icmpeq");
        return builder.CreateCall(curModule->getFunction("encodeBool"), val);
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
    auto isnum = curModule->getFunction("isNum");
    auto c1 = builder.CreateCall(isnum, lhs);
    auto c2 = builder.CreateCall(isnum, rhs);
    // To reduce branching on a common operation, select instruction is used
    auto sel = builder.CreateSelect(builder.CreateAnd(c1, c2), fcmptmp, icmptmp);
    return builder.CreateCall(curModule->getFunction("encodeBool"), sel);
}

void Compiler::codegenBlock(const typedAST::Block& block){
    for(auto stmt : block.stmts){
        stmt->codegen(this);
    }
}

llvm::Value* Compiler::codegenNeg(const typedExprPtr _rhs, typedAST::UnaryOp op, Token dbg){
    llvm::Value* rhs = _rhs->codegen(this);
    llvm::Function *F = builder.GetInsertBlock()->getParent();
    // If rhs is known to be a number, no need for the type check
    if(exprConstrainedToType(_rhs, types::getBasicType(types::TypeFlag::NUMBER))){
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
    // If rhs isn't of the correct type, go to error, otherwise proceed as normal
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx);
    if(op == typedAST::UnaryOp::BIN_NEG) executeOpBB->setName("binnegnum");
    else executeOpBB->setName("negnum");

    auto cond = builder.CreateCall(curModule->getFunction("isNum"), rhs);
    builder.CreateCondBr(builder.CreateNot(cond), errorBB, executeOpBB);
    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    createTyErr("Operand must be a number, got '{}'.", rhs, dbg);
    // Is never actually hit since tyErr throws, but LLVM requires every block to have a terminator
    builder.CreateBr(executeOpBB);

    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);
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

std::shared_ptr<types::FunctionType> Compiler::getFuncFromType(const types::tyVarIdx ty){
    auto tyarr = typeEnv[ty];
    types::tyPtr fnTy = nullptr;
    bool isOnlyFunc = true;
    for(auto innerTy : tyarr){
        if(innerTy->type == types::TypeFlag::FUNCTION){
            // If this type contains multiple functions don't return any of them since it can't be statically determined
            // which function is being called
            if(fnTy) return nullptr;
            fnTy = innerTy;
        }
    }

    return std::reinterpret_pointer_cast<types::FunctionType>(fnTy);
}

#pragma endregion

// Only used when debugging _LONG versions of op codes
#undef SHORT_CONSTANT_LIMIT

#undef CHECK_SCOPE_FOR_LOOP
#undef CHECK_SCOPE_FOR_SWITCH
