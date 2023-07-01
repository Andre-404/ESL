#include "compiler.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "upvalueFinder.h"
#include "../Runtime/thread.h"
#include "../Runtime/nativeFunctions.h"
#include "llvmHelperFunctions.h"
#include "LLVMNativeFunctions.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include <unordered_set>
#include <iostream>

using namespace compileCore;
using namespace object;

CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, llvm::Function* _func) {
    hasReturnStmt = false;
    hasCapturedLocals = false;
    enclosing = _enclosing;
    type = _type;
    line = 0;
    func = _func;
}


Compiler::Compiler(vector<ESLModule*>& _units) : ctx(std::make_unique<llvm::LLVMContext>()), builder(llvm::IRBuilder<>(*ctx)) {
    upvalueFinder::UpvalueFinder f(_units);
    upvalueMap = f.generateUpvalueMap();

    currentClass = nullptr;
    curUnitIndex = 0;
    units = _units;
    BBTerminated = false;

    curModule = std::make_unique<llvm::Module>("Module", *ctx);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    auto err = llvm::orc::KaleidoscopeJIT::Create().moveInto(JIT);
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    curModule->setDataLayout(JIT->getDataLayout());
    curModule->setTargetTriple(targetTriple);

    llvmHelpers::addHelperFunctionsToModule(curModule, ctx, builder, namedTypes);

    compile();
    llvmHelpers::runModule(curModule, JIT, ctx, true);

}

void Compiler::compile(){
    llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx), false);
    auto tmpfn = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "mainFn", curModule.get());
    current = new CurrentChunkInfo(nullptr, FuncType::TYPE_SCRIPT, tmpfn);
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", current->func);
    builder.SetInsertPoint(BB);
    curModule->getOrInsertGlobal("gcFlag", builder.getInt8PtrTy());

    for (ESLModule* unit : units) {
        curUnit = unit;
        sourceFiles.push_back(unit->file);
        for (const auto decl : unit->topDeclarations) {
            string varName = unit->file->name + std::to_string(curUnit->id) + "." + decl->getName().getLexeme();
            // TODO: add this pointer to the gc
            curModule->getOrInsertGlobal(varName, builder.getInt64Ty());
            llvm::GlobalVariable* gvar = curModule->getNamedGlobal(varName);
            gvar->setLinkage(llvm::GlobalVariable::PrivateLinkage);
            gvar->setAlignment(llvm::Align::Of<Value>());
            gvar->setInitializer(builder.getInt64(encodeNil()));
            globals.insert_or_assign(varName, Globalvar(decl->getType(), gvar));
        }
        for (int i = 0; i < unit->stmts.size(); i++) {
            // Doing this here so that even if a error is detected, we go on and possibly catch other(valid) errors
            try {
                unit->stmts[i]->accept(this);
            }
            catch (CompilerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curUnitIndex++;
    }

    // Ends the main function
    builder.CreateRetVoid();
    llvm::verifyFunction(*current->func);
    curModule->print(llvm::errs(), nullptr);
    for (ESLModule* unit : units) delete unit;
}

static Token probeToken(AST::ASTNodePtr ptr){
    AST::ASTProbe p;
    ptr->accept(&p);
    return p.getProbedToken();
}

static bool isLiteralThis(AST::ASTNodePtr ptr){
    if(ptr->type != AST::ASTType::LITERAL) return false;
    return probeToken(ptr).type == TokenType::THIS;
}

void Compiler::visitAssignmentExpr(AST::AssignmentExpr* expr) {
    llvm::Value* rhs = evalASTExpr(expr->value);
    storeToVar(expr->name, rhs);
    returnValue = rhs;
}

void Compiler::visitSetExpr(AST::SetExpr* expr) {

}

void Compiler::visitConditionalExpr(AST::ConditionalExpr* expr) {
    auto condtmp = evalASTExpr(expr->condition);
    auto cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*ctx, "condexpr.then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*ctx, "condexpr.else");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "condexpr.merge");

    builder.CreateCondBr(cond, thenBB, elseBB);
    //Emits code to conditionally execute mhs(thenBB) and rhs(elseBB)

    builder.SetInsertPoint(thenBB);
    llvm::Value* thentmp = evalASTExpr(expr->mhs);
    builder.CreateBr(mergeBB);
    // PHI nodes need up-to-date block info to know which block control is coming from
    thenBB = builder.GetInsertBlock();

    func->insert(func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    llvm::Value* elsetmp = evalASTExpr(expr->rhs);
    // PHI nodes need up-to-date block info to know which block control is coming from
    elseBB = builder.GetInsertBlock();

    // Get the results
    builder.CreateBr(mergeBB);
    func->insert(func->end(), mergeBB);
    builder.SetInsertPoint(mergeBB);
    llvm::PHINode *PN = builder.CreatePHI(builder.getInt64Ty(), 2, "condexpr.res");

    PN->addIncoming(thentmp, thenBB);
    PN->addIncoming(elsetmp, elseBB);

    // Value is already a int64
    returnValue = PN;
}

void Compiler::visitRangeExpr(AST::RangeExpr *expr) {

}


void Compiler::visitBinaryExpr(AST::BinaryExpr* expr) {
    updateLine(expr->op);

    // Shorthand for check if both values are numbers, every operation requires this check
    auto bothNum = [&](llvm::Value* lhs, llvm::Value* rhs){
        auto isnum = curModule->getFunction("isNum");
        auto c1 = builder.CreateCall(isnum, lhs);
        auto c2 = builder.CreateCall(isnum, rhs);
        return builder.CreateAnd(c1, c2);
    };
    auto dblCast = [&](llvm::Value* val){
        return builder.CreateBitCast(val, builder.getDoubleTy());
    };
    // TODO: this can break if val > 2^63
    auto dblToInt = [&](llvm::Value* val){
        return builder.CreateFPToSI(val, builder.getInt64Ty());
    };
    auto matchTT = [&](TokenType type, std::initializer_list<TokenType> list){
        for(auto it = list.begin(); it != list.end(); it++){
            if(*it == type) return true;
        }
        return false;
    };

    llvm::Value* lhs = evalASTExpr(expr->left);
    auto op = expr->op.type;
    if(op == TokenType::OR || op == TokenType::AND){
        auto castToBool = curModule->getFunction("isTruthy");
        auto castToVal = curModule->getFunction("encodeBool");

        llvm::Function* func = builder.GetInsertBlock()->getParent();

        // Original block is the one we're coming from, both originalBB and evalRhsBB go to mergeBB
        llvm::BasicBlock* originalBB = builder.GetInsertBlock();
        llvm::BasicBlock* evalRhsBB = llvm::BasicBlock::Create(*ctx, op == TokenType::OR ? "lhsfalse" : "lhstrue", func);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

        // Cast lhs from val to bool and create a check
        // For 'or' operator if lhs is false we eval rhs
        // For 'and' operator if lhs is true we eval rhs
        lhs = builder.CreateCall(castToBool, lhs);
        builder.CreateCondBr(op == TokenType::OR ? builder.CreateNot(lhs) : lhs, evalRhsBB, mergeBB);

        // If lhs is false(or in the case of 'and' operator true) eval rhs and then go into mergeBB
        builder.SetInsertPoint(evalRhsBB);
        llvm::Value* rhs = builder.CreateCall(castToBool, evalASTExpr(expr->right));
        builder.CreateBr(mergeBB);
        // In case we have a nested 'or' or 'and' lhsFalseBB could no longer be the block the builder is emitting to
        evalRhsBB = builder.GetInsertBlock();
        // Emit merge block, code from this point on will be generated into this block
        func->insert(func->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        llvm::PHINode *PN = builder.CreatePHI(builder.getInt1Ty(), 2, op == TokenType::OR ? "lortmp" : "landtmp");

        // If we're coming from the originalBB and the operator is 'or' it means that lhs is true, and thus the entire expression is true
        // For 'and' it's the opposite, if lhs is false, then the entire expression is false
        PN->addIncoming(builder.getInt1(op == TokenType::OR ? true : false), originalBB);
        // For both operators, if control flow is coming from evalRhsBB ths becomes the value of the entire expression
        PN->addIncoming(rhs, evalRhsBB);

        // Cast the bool to a Value
        returnValue = builder.CreateCall(castToVal, PN);
        return;
    }

    llvm::Value* rhs = evalASTExpr(expr->right);
    llvm::Function *F = builder.GetInsertBlock()->getParent();

    if(op == TokenType::PLUS){
        // If both are a number go to addNum, if not try adding as string
        // If both aren't strings, throw error(error is thrown inside addString C++ function)
        llvm::BasicBlock *addNumBB = llvm::BasicBlock::Create(*ctx, "addnum", F);
        llvm::BasicBlock *addStringBB = llvm::BasicBlock::Create(*ctx, "addstring");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx, "merge");

        builder.CreateCondBr(bothNum(lhs, rhs), addNumBB, addStringBB);

        // If both values are numbers, add them and go to mergeBB
        builder.SetInsertPoint(addNumBB);
        auto numAddRes = castToVal(builder.CreateFAdd(dblCast(lhs), dblCast(rhs), "addtmp"));
        builder.CreateBr(mergeBB);

        // Tries to add lhs and rhs as strings, if it fails throw a type error
        F->insert(F->end(), addStringBB);
        builder.SetInsertPoint(addStringBB);
        // Have to pass file and line since strAdd might throw an error, and it needs to know where the error occurred
        llvm::Constant* file = createConstStr(curUnit->file->path);
        llvm::Constant* line = builder.getInt32(current->line);
        // Returns Value
        auto stringAddRes = builder.CreateCall(curModule->getFunction("strAdd"), {lhs, rhs, file, line});
        builder.CreateBr(mergeBB);

        // Final destination for both branches, if both values were numbers or strings(meaning no error was thrown)
        // Use a phi node to determine which one it is and then set it as returnValue
        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        auto phi = builder.CreatePHI(builder.getInt64Ty(), 2);
        phi->addIncoming(numAddRes, addNumBB);
        phi->addIncoming(stringAddRes, addStringBB);
        returnValue = phi;
        return;
    }
    else if(matchTT(op, {TokenType::EQUAL_EQUAL, TokenType::BANG_EQUAL})){
        bool neg = (op == TokenType::BANG_EQUAL);

        llvm::Value* icmptmp;
        llvm::Value* fcmptmp;
        if(neg){
            icmptmp = builder.CreateICmpNE(lhs, rhs, "icmptmp");
            fcmptmp = builder.CreateFCmpONE(dblCast(lhs), dblCast(rhs), "fcmptmp");
        }else {
            icmptmp = builder.CreateICmpEQ(lhs, rhs, "icmptmp");
            fcmptmp = builder.CreateFCmpOEQ(dblCast(lhs), dblCast(rhs), "fcmptmp");
        }
        // To reduce branching on a common operation, select instruction is used
        auto sel = builder.CreateSelect(bothNum(lhs, rhs), fcmptmp, icmptmp);
        returnValue = builder.CreateCall(curModule->getFunction("encodeBool"), sel);
        return;
    }

    string opType;
    switch(op){
        case TokenType::MINUS: opType = "sub";break;
        case TokenType::STAR: opType = "mul"; break;
        case TokenType::SLASH: opType = "fdiv"; break;
        case TokenType::PERCENTAGE: opType = "rem"; break;
        case TokenType::DIV: opType = "idiv"; break;
        case TokenType::BITSHIFT_LEFT: opType = "shl"; break;
        case TokenType::BITSHIFT_RIGHT: opType = "shr"; break;
        case TokenType::BITWISE_AND: opType = "and"; break;
        case TokenType::BITWISE_OR: opType = "or"; break;
        case TokenType::BITWISE_XOR: opType = "xor"; break;
        case TokenType::GREATER: opType = "gt"; break;
        case TokenType::GREATER_EQUAL: opType = "gte"; break;
        case TokenType::LESS: opType = "lt"; break;
        case TokenType::LESS_EQUAL: opType = "lte"; break;
        default: break;
    }
    // If either or both aren't numbers, go to error, otherwise proceed as normal
    llvm::BasicBlock *errorBB = llvm::BasicBlock::Create(*ctx, "error", F);
    llvm::BasicBlock *executeOpBB = llvm::BasicBlock::Create(*ctx, opType + "num");

    builder.CreateCondBr(builder.CreateNot(bothNum(lhs, rhs)), errorBB, executeOpBB);

    // Calls the type error function which throws
    builder.SetInsertPoint(errorBB);
    createTyErr("Operands must be numbers, got '{}' and '{}'.", lhs, rhs);
    // Is never actually hit since tyErr throws, but LLVM requires every block have a terminator
    builder.CreateBr(executeOpBB);
    // Floors both numbers, then divides
    F->insert(F->end(), executeOpBB);
    builder.SetInsertPoint(executeOpBB);


    if(op == TokenType::DIV){
        auto tmp1 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, dblCast(lhs));
        auto tmp2 = builder.CreateUnaryIntrinsic(llvm::Intrinsic::floor, dblCast(rhs));
        retVal(builder.CreateFDiv(tmp1, tmp2, "floordivtmp"));
        return;
    }
    else if(matchTT(op, {TokenType::MINUS, TokenType::STAR, TokenType::SLASH, TokenType::PERCENTAGE})){
        switch(op){
            case TokenType::MINUS:
                retVal(builder.CreateFSub(dblCast(lhs), dblCast(rhs), "subtmp")); break;
            case TokenType::STAR:
                retVal(builder.CreateFMul(dblCast(lhs), dblCast(rhs), "multmp")); break;
            case TokenType::SLASH:
                retVal(builder.CreateFDiv(dblCast(lhs), dblCast(rhs), "divtmp")); break;
            case TokenType::PERCENTAGE:
                retVal(builder.CreateFRem(dblCast(lhs), dblCast(rhs), "remtmp")); break;
            default: break;
        }
        return;
    }
    else if(matchTT(op, {TokenType::BITSHIFT_LEFT, TokenType::BITSHIFT_RIGHT, TokenType::BITWISE_AND, TokenType::BITWISE_OR, TokenType::BITWISE_XOR})){
        llvm::Value* res;
        switch(op){
            case TokenType::BITSHIFT_LEFT:
                res = builder.CreateShl(dblToInt(dblCast(lhs)), dblToInt(dblCast(rhs)), "shltmp"); break;
            case TokenType::BITSHIFT_RIGHT:
                res = builder.CreateAShr(dblToInt(dblCast(lhs)), dblToInt(dblCast(rhs)), "Ashrtmp"); break;
            case TokenType::BITWISE_AND:
                res = builder.CreateAnd(dblToInt(dblCast(lhs)), dblToInt(dblCast(rhs)), "andtmp"); break;
            case TokenType::BITWISE_OR:
                res = builder.CreateOr(dblToInt(dblCast(lhs)), dblToInt(dblCast(rhs)), "ortmp"); break;
            case TokenType::BITWISE_XOR:
                res = builder.CreateXor(dblToInt(dblCast(lhs)), dblToInt(dblCast(rhs)), "xortmp"); break;
            default: break;
        }
        retVal(builder.CreateSIToFP(res, builder.getDoubleTy()));
        return;
    }
    else if(matchTT(op, {TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL})){
        llvm::Value* res;
        switch(op){
            case TokenType::GREATER:
                res = builder.CreateFCmpOGT(dblCast(lhs), dblCast(rhs), "ogttmp"); break;
            case TokenType::GREATER_EQUAL:
                res = builder.CreateFCmpOGE(dblCast(lhs), dblCast(rhs), "ogetmp"); break;
            case TokenType::LESS:
                res = builder.CreateFCmpOLT(dblCast(lhs), dblCast(rhs), "olttmp"); break;
            case TokenType::LESS_EQUAL:
                res = builder.CreateFCmpOLE(dblCast(lhs), dblCast(rhs), "oletmp"); break;
            default: break;
        }
        returnValue = builder.CreateCall(curModule->getFunction("encodeBool"), res);
        return;
    }
    // Should never be hit
    error(expr->op, "Unrecognized token in binary expression.");
}

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
    }*/

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

void Compiler::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    vector<llvm::Value*> vals;
    for(auto mem : expr->members){
        vals.push_back(evalASTExpr(mem));
    }
    auto arrNum = builder.getInt32(vals.size());
    auto arr = builder.CreateCall(curModule->getFunction("createArr"), arrNum, "array");
    auto arrPtr = builder.CreateCall(curModule->getFunction("getArrPtr"), arr, "arrptr");
    for(int i = 0; i < vals.size(); i++){
        auto gep = builder.CreateInBoundsGEP(builder.getInt64Ty(), arrPtr, builder.getInt32(i));
        builder.CreateStore(vals[i], gep);
    }
    returnValue = arr;
}

void Compiler::visitCallExpr(AST::CallExpr* expr) {
    // Invoking is field access + call, when the compiler recognizes this pattern it optimizes
    /*if (invoke(expr)) return;
    //todo: tail recursion optimization
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->args.size());*/
    auto arg = evalASTExpr(expr->args[0]);
    builder.CreateCall(curModule->getFunction("print"), arg);
}

void Compiler::visitNewExpr(AST::NewExpr* expr){
    // Parser guarantees that expr->call->callee is either a literal or a module access
    /*auto klass = getClassFromExpr(expr->call->callee);

    emitConstant(encodeObj(klass));

    for (AST::ASTNodePtr arg : expr->call->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->call->args.size());*/
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

    if(resolveThis(expr)) return;
    expr->callee->accept(this);
    uint16_t name = identifierConstant(probeToken(expr->field));
    if (name <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::GET_PROPERTY, name);
    else emitByteAnd16Bit(+OpCode::GET_PROPERTY_LONG, name);
    return;*/
}

void Compiler::visitStructLiteralExpr(AST::StructLiteral* expr) {
    vector<llvm::Value*> args;
    args.push_back(builder.getInt32(expr->fields.size()));
    //for each field, compile it and get the constant of the field name
    for (AST::StructEntry entry : expr->fields) {
        updateLine(entry.name);
        //this gets rid of quotes, ""Hello world""->"Hello world"
        string temp = entry.name.getLexeme();
        temp.erase(0, 1);
        temp.erase(temp.size() - 1, 1);
        args.push_back(builder.CreateCall(curModule->getFunction("createStr"), createConstStr(temp)));
        args.push_back(evalASTExpr(entry.expr));
    }

    returnValue = builder.CreateCall(curModule->getFunction("createHashMap"), args);
}

static std::pair<bool, bool> classContainsMethod(string& publicField, ankerl::unordered_dense::map<object::ObjString*, Obj*>& map);

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
    else emitByteAnd16Bit(+OpCode::GET_SUPER_LONG, name);*/
}

void Compiler::visitLiteralExpr(AST::LiteralExpr* expr) {
    updateLine(expr->token);

    switch (expr->token.type) {
        case TokenType::NUMBER: {
            string num = expr->token.getLexeme();
            double val = std::stod(num);
            retVal(llvm::ConstantFP::get(*ctx, llvm::APFloat(val)));
            break;
        }
        case TokenType::TRUE:
        case TokenType::FALSE: {
            returnValue = builder.getInt64(encodeBool(expr->token.type == TokenType::TRUE));
            break;
        }
        case TokenType::NIL: returnValue = builder.getInt64(encodeNil()); break;
        case TokenType::STRING: {
            //this gets rid of quotes, ""Hello world""->"Hello world"
            string temp = expr->token.getLexeme();
            temp.erase(0, 1);
            temp.erase(temp.size() - 1, 1);
            auto str = createConstStr(temp);
            // Returns Value, no need to cast
            returnValue = builder.CreateCall(curModule->getFunction("createStr"), str);
            break;
        }
        /*
        case TokenType::THIS: {
            if (currentClass == nullptr) error(expr->token, "Can't use keyword 'this' outside of a class.");
            //'this' get implicitly defined by the compiler
            namedVar(expr->token, false);
            break;
        }*/
        case TokenType::IDENTIFIER: {
            returnValue = readVar(expr->token);
            break;
        }
    }
}

void Compiler::visitFuncLiteral(AST::FuncLiteral* expr) {
    // Creating a new compilerInfo sets us up with a clean slate for writing IR, the enclosing functions info
    // is stored in parserCurrent->enclosing
    auto upvalsToCapture = upvalueMap.at(expr);
    createNewFunc(expr->arity + (upvalsToCapture.size() > 0 ? 1 : 0), "Anonymous function", FuncType::TYPE_FUNC);
    // Essentially pushes all upvalues to the machine stack, the pointer to ObjUpval is stored in the vector 'upvalues'
    for(int i = 0; i < upvalsToCapture.size(); i++){
        auto& upval = upvalsToCapture[i];
        auto tmp = builder.CreateCall(curModule->getFunction("getUpvalue"), {current->func->getArg(0), builder.getInt32(i)});
        current->upvalues.emplace_back(upval.name, tmp);
    }

    // Mo need for a endScope, since returning from the function discards the entire callstack
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
    // If this lambda doesn't use any upvalues bail early and don't convert it to a closure
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
            closureConstructorArgs.push_back(current->upvalues[upval.index].val);
        }
    }
    // Create the closure and stuff the upvalues in it
    returnValue = builder.CreateCall(curModule->getFunction("createClosure"), closureConstructorArgs);
}

void Compiler::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    returnValue = resolveModuleVariable(expr->moduleName, expr->ident);
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
    emitBytes(+OpCode::LAUNCH_ASYNC, expr->args.size());*/
}

void Compiler::visitAwaitExpr(AST::AwaitExpr* expr) {
    /*updateLine(expr->token);
    expr->expr->accept(this);
    emitByte(+OpCode::AWAIT);*/
}

void Compiler::visitVarDecl(AST::VarDecl* decl) {
    string global;
    if(decl->var.type == AST::ASTVarType::GLOBAL){
        global = declareGlobalVar(decl->var.name);
    }else declareLocalVar(decl->var);

    // Compile the right side of the declaration, if there is no right side, the variable is initialized as nil
    llvm::Value* initializer = builder.getInt64(encodeNil());
    if (decl->value != nullptr) {
        initializer = evalASTExpr(decl->value);
    }

    if(decl->var.type == AST::ASTVarType::GLOBAL){
        defineGlobalVar(global, initializer);
    }else defineLocalVar(initializer);
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
    currentClass = nullptr;*/
}

void Compiler::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
}

void Compiler::visitBlockStmt(AST::BlockStmt* stmt) {
    beginScope();
    for (AST::ASTNodePtr node : stmt->statements) {
        try {
            node->accept(this);
        }catch(CompilerException e){

        }
    }
    endScope();
}

void Compiler::visitIfStmt(AST::IfStmt* stmt) {
    auto condtmp = evalASTExpr(stmt->condition);
    auto cond = builder.CreateCall(curModule->getFunction("isTruthy"), condtmp);

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*ctx, "if.then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*ctx, "if.else");
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*ctx, "if.merge");

    builder.CreateCondBr(cond, thenBB, elseBB);


    builder.SetInsertPoint(thenBB);
    stmt->thenBranch->accept(this);
    endControlFlowBB(mergeBB);

    func->insert(func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    if(stmt->elseBranch) stmt->elseBranch->accept(this);
    endControlFlowBB(mergeBB);

    func->insert(func->end(), mergeBB);
    // Sets the builder up to emit code after the if stmt
    builder.SetInsertPoint(mergeBB);
}

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

void Compiler::visitForStmt(AST::ForStmt* stmt) {
    //we wrap this in a scope so if there is a var declaration in the initialization it's scoped to the loop
   /* beginScope();
    if (stmt->init != nullptr) stmt->init->accept(this);
    //if check to see if the condition is true the first time
    int exitJump = -1;
    if (stmt->condition != nullptr) {
        stmt->condition->accept(this);
        exitJump = emitJump(+OpCode::JUMP_IF_FALSE_POP);
    }

    int loopStart = getChunk()->bytecode.size();
    //loop body gets it's own scope because we only patch break and continue jumps which are declared in higher scope depths
    //user might not use {} block when writing a loop, this ensures the body is always in it's own scope
    current->scopeWithLoop.push_back(current->scopeDepth);
    beginScope();
    stmt->body->accept(this);
    endScope();
    current->scopeWithLoop.pop_back();
    //patching continue here to increment if a variable for incrementing has been defined
    patchScopeJumps(ScopeJumpType::CONTINUE);
    //if there is a increment expression, we compile it and emit a POP to get rid of the result
    if (stmt->increment != nullptr) {
        stmt->increment->accept(this);
        emitByte(+OpCode::POP);
    }
    //if there is a condition, compile it again and if true, loop to the start of the body
    if (stmt->condition != nullptr) {
        stmt->condition->accept(this);
        emitLoop(loopStart);
    }
    else {
        //if there isn't a condition, it's implictly defined as 'true'
        emitByte(+OpCode::LOOP);

        int offset = getChunk()->bytecode.size() - loopStart + 2;
        if (offset > UINT16_MAX) error("Loop body too large.");

        emit16Bit(offset);
    }
    if (exitJump != -1) patchJump(exitJump);
    patchScopeJumps(ScopeJumpType::BREAK);
    endScope();*/
}

// Simple no-cond jump to whichever block is at the top of the breakJumpDest stack
void Compiler::visitBreakStmt(AST::BreakStmt* stmt) {
    builder.CreateBr(breakJumpDest.back());
    BBTerminated = true;
}
// Simple no-cond jump to whichever block is at the top of the continueJumpDest stack
void Compiler::visitContinueStmt(AST::ContinueStmt* stmt) {
    builder.CreateBr(continueJumpDest.back());
    BBTerminated = true;
}

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
        //end scope takes care of upvalues
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
    patchScopeJumps(ScopeJumpType::BREAK);*/
}

void Compiler::visitCaseStmt(AST::CaseStmt* stmt) {
    // Compile every statement in the case
    for (AST::ASTNodePtr caseStmt : stmt->stmts) {
        caseStmt->accept(this);
    }
}

// Simple no-cond jump to the next basic block in the current switch
void Compiler::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    builder.CreateBr(advanceJumpDest.back());
    BBTerminated = true;
}

void Compiler::visitReturnStmt(AST::ReturnStmt* stmt) {
    updateLine(stmt->keyword);
    if (current->type == FuncType::TYPE_SCRIPT) {
        error(stmt->keyword, "Can't return from top-level code.");
    }
    else if (current->type == FuncType::TYPE_CONSTRUCTOR) {
        error(stmt->keyword, "Can't return a value from a constructor.");
    }
    current->hasReturnStmt = true;
    //if no expression is given, null is returned
    if (stmt->expr == nullptr) {
        emitReturn();
        return;
    }
    auto rettmp = evalASTExpr(stmt->expr);
    builder.CreateRet(rettmp);
    // Ret is considered a basic block terminator
    BBTerminated = true;
}

#pragma region helpers

void Compiler::emitReturn() {
    llvm::Value* rettmp;
    // In a constructor, the first local variable refers to the new instance of a class('this')
    if (current->type == FuncType::TYPE_CONSTRUCTOR) {
        // First spot in the locals slot is occupied by 'this' instance
        rettmp = builder.CreateCall(curModule->getFunction("encodeObj"), current->locals[0].val);
    }
    else rettmp = builder.getInt64(encodeNil());
    builder.CreateRet(rettmp);
}

void Compiler::endControlFlowBB(llvm::BasicBlock* dest){
    // If the current BB ends with a break/continue/advance/return, don't emit the final break since this basic block is already terminated
    if(BBTerminated) {
        // Reset flag since the break/continue/advance/return statement has been handled
        BBTerminated = false;
    }
    else {
        // Unconditional fallthrough to destination block
        builder.CreateBr(dest);
    }
}

#pragma region Variables

void Compiler::defineGlobalVar(string global, llvm::Value* val) {
    Globalvar& gvar = globals.at(global);
    gvar.isDefined = true;
    builder.CreateStore(val, gvar.val);
}

// Gets/sets a variable, respects the scoping rules(locals->upvalues->class fields(if inside a method)->globals)
llvm::Value* Compiler::readVar(Token name){
    updateLine(name);
    int argIndex = resolveLocal(name);
    if (argIndex != -1) {
        if(current->locals[argIndex].isUpval){
            llvm::Value* upvalPtr = current->locals[argIndex].val;
            //TODO: load from upval
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjUpvalue"], upvalPtr, {builder.getInt32(0), builder.getInt32(0), builder.getInt32(1)}, "upvalAddr");
            return builder.CreateLoad(builder.getInt64Ty(), tmpEle, "loadupval");
        }else{
            return builder.CreateLoad(builder.getInt64Ty(), current->locals[argIndex].val, "loadvar");
        }
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        llvm::Value* upvalPtr = current->upvalues[argIndex].val;
        auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjUpvalue"], upvalPtr, {builder.getInt32(0), builder.getInt32(0), builder.getInt32(1)}, "upvalAddr");
        return builder.CreateLoad(builder.getInt64Ty(), tmpEle, "loadupval");
    }

    llvm::Value* value = nullptr;
    if((value = resolveClassField(name, false))){
        if(current->type == FuncType::TYPE_FUNC){
            error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
        }


    }
    else if((value = resolveGlobal(name, false))){
        return builder.CreateLoad(builder.getInt64Ty(), value, "loadgvar");
    }
    string nativeName = name.getLexeme();
    auto it = nativeFunctions.find(nativeName);
    if(it != nativeFunctions.end()) return it->second;

    error(name, fmt::format("'{}' doesn't match any declared variable name or native function name.", nativeName));
    return nullptr;
}

void Compiler::storeToVar(Token name, llvm::Value* val){
    int argIndex = resolveLocal(name);
    if (argIndex != -1) {
        if(current->locals[argIndex].isUpval){
            llvm::Value* upvalPtr = current->locals[argIndex].val;
            //TODO: store to upval
            auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjUpvalue"], upvalPtr, {builder.getInt32(0), builder.getInt32(0), builder.getInt32(1)}, "upvalAddr");
            builder.CreateStore(val, tmpEle);
            return;
        }else{
            builder.CreateStore(val, current->locals[argIndex].val);
            return;
        }
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        llvm::Value* upvalPtr = current->upvalues[argIndex].val;
        //TODO: store to upval
        auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjUpvalue"], upvalPtr, {builder.getInt32(0), builder.getInt32(0), builder.getInt32(1)}, "upvalAddr");
        builder.CreateStore(val, tmpEle);
        return;
    }
    // Intentionally shadows
    llvm::Value* arg = nullptr;
    if((arg = resolveClassField(name, true))){
        if(current->type == FuncType::TYPE_FUNC){
            error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
        }


    }
    else if((arg = resolveGlobal(name, true))){
        builder.CreateStore(val, arg);
    }
    else{
        // No need to check for natives since they can't be assigned to
        error(name, fmt::format("'{}' doesn't match any declared variable name or native function name.", name.getLexeme()));
    }
}

// Globals are already created at the start of compiling each module, just return the full name
string Compiler::declareGlobalVar(Token name) {
    updateLine(name);
    string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + name.getLexeme();
    return fullSymbol;
}

// Makes sure the compiler is aware that a stack slot is occupied by this local variable
void Compiler::declareLocalVar(AST::ASTVar& var) {
    updateLine(var.name);

    for(int i = current->locals.size() - 1; i >= 0; i--){
        Local& local = current->locals[i];
        if (local.depth != -1 && local.depth < current->scopeDepth) {
            break;
        }
        string str = var.name.getLexeme();
        if (str.compare(local.name) == 0) {
            error(var.name, "Already a variable with this name in this scope.");
        }
    }
    addLocal(var);
}

// If this variable is an upvalue that's in this scope, mark it as such
void Compiler::addLocal(AST::ASTVar var) {
    updateLine(var.name);
    current->locals.emplace_back(var.name.getLexeme(), -1, var.type == AST::ASTVarType::LOCAL_UPVALUE);
    Local& local = current->locals.back();
    // Alloca at the beginning of the function to make use of mem2reg pass
    llvm::IRBuilder<> tempBuilder(&current->func->getEntryBlock(), current->func->getEntryBlock().begin());
    if(!local.isUpval) {
        local.val = tempBuilder.CreateAlloca(builder.getInt64Ty(), nullptr, var.name.getLexeme());
    }else {
        // Allocates an ObjUpval on the heap and returns pointer
        local.val = tempBuilder.CreateCall(curModule->getFunction("createUpvalue"), std::nullopt, var.name.getLexeme());
    }
}

void Compiler::beginScope() {
    current->scopeDepth++;
}

void Compiler::endScope() {
    // Pop every variable that was declared in this scope
    current->scopeDepth--;// First lower the scope, the check for every var that is deeper than the parserCurrent scope
    // Pop from the stack
    while (current->locals.size() > 0 && current->locals.back().depth > current->scopeDepth) {
        current->locals.pop_back();
    }
}

// Returns index of local in current func chunk info
int Compiler::resolveLocal(Token name) {
    //checks to see if there is a local variable with a provided name, if there is return the index of the stack slot of the var
    updateLine(name);
    for (int i  = current->locals.size() - 1; i >= 0; i--) {
        Local& local = current->locals[i];
        string str = name.getLexeme();
        if (str.compare(local.name) == 0) {
            if (local.depth == -1) {
                error(name, "Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

// UpvalueFinder already computed all upvalues this function uses from an enclosing function and stored them in func->upvalues
int Compiler::resolveUpvalue(Token name) {
    string upvalName = name.getLexeme();
    for(int i = 0; i < current->upvalues.size(); i++){
        if(upvalName == current->upvalues[i].name) return i;
    }
    return -1;
}

// Marks the local at the top of the stack as ready to use
void Compiler::defineLocalVar(llvm::Value* val) {
    current->locals.back().depth = current->scopeDepth;

    if(current->locals.back().isUpval) {
        auto tmpEle = builder.CreateInBoundsGEP(namedTypes["ObjUpvalue"], current->locals.back().val, {builder.getInt32(0), builder.getInt32(0), builder.getInt32(1)}, "upvalAddr");
        builder.CreateStore(val, tmpEle);
    }else{
        builder.CreateStore(val, current->locals.back().val);
    }
}

Token Compiler::syntheticToken(string str) {
    return Token(TokenType::IDENTIFIER, str);
}

#pragma endregion

#pragma region Classes and methods
object::ObjClosure* Compiler::method(AST::FuncDecl* _method, Token className) {
    /*updateLine(_method->getName());
    uint16_t name = identifierConstant(_method->getName());
    FuncType type = FuncType::TYPE_METHOD;
    // Constructors are treated separately, but are still methods
    if (_method->getName().equals(className)) type = FuncType::TYPE_CONSTRUCTOR;
    // "this" gets implicitly defined as the first local in methods and the constructor
    current = new CurrentChunkInfo(current, type);
    beginScope();
    // Args defined as local in order they were passed to the function
    for (AST::ASTVar& var : _method->args) {
        declareLocalVar(var);
        defineLocalVar();
    }
    for(auto stmt : _method->body->statements){
        try {
            stmt->accept(this);
        }catch(CompilerException e){

        }
    }
    current->func->arity = _method->arity;

    current->func->name = _method->getName().getLexeme();
    ObjFunc* func = endFuncDecl();
    if (func->upvalueCount != 0) error(_method->getName(), "Upvalues captured in method, aborting...");
    return new ObjClosure(func);*/
    return nullptr;
}

bool Compiler::invoke(AST::CallExpr* expr) {
    /*if (expr->callee->type == AST::ASTType::FIELD_ACCESS) {
        //currently we only optimizes field invoking(struct.field() or array[field]())
        auto call = std::static_pointer_cast<AST::FieldAccessExpr>(expr->callee);
        if(call->accessor.type == TokenType::LEFT_BRACKET) return false;

        call->callee->accept(this);
        uint16_t constant;
        Token name = probeToken(call->field);
        // If invoking a method inside another method, make sure to get the right(public/private) name
        if(isLiteralThis(call->callee)){
            int res = resolveClassField(name, false);
            if(res == -1) error(name, fmt::format("Field {}, doesn't exist in class {}.", name.getLexeme(), currentClass->klass->name->str));
            constant = res;
        }else constant = identifierConstant(name);

        int argCount = 0;
        for (AST::ASTNodePtr arg : expr->args) {
            arg->accept(this);
            argCount++;
        }
        if(constant > SHORT_CONSTANT_LIMIT){
            emitBytes(+OpCode::INVOKE_LONG, argCount);
            emit16Bit(constant);
        }
        else {
            emitBytes(+OpCode::INVOKE, argCount);
            emitByte(constant);
        }
        return true;
    }
    else if (expr->callee->type == AST::ASTType::SUPER) {
        auto superCall = std::static_pointer_cast<AST::SuperExpr>(expr->callee);
        uint16_t name = identifierConstant(superCall->methodName);

        if (currentClass == nullptr) {
            error(superCall->methodName, "Can't use 'super' outside of a class.");
        }
        else if (!currentClass->klass->superclass) {
            error(superCall->methodName, "Can't use 'super' in a class with no superclass.");
        }
        string fieldName = superCall->methodName.getLexeme();
        auto res = classContainsMethod(fieldName, currentClass->klass->superclass->methods);
        if(!res.first){
            error(superCall->methodName, fmt::format("Superclass '{}' doesn't contain method '{}'.", currentClass->klass->superclass->name->str, superCall->methodName.getLexeme()));
        }
        //in methods and constructors, "this" is implicitly defined as the first local
        namedVar(syntheticToken("this"), false);
        int argCount = 0;
        for (AST::ASTNodePtr arg : expr->args) {
            arg->accept(this);
            argCount++;
        }
        // superclass gets popped, leaving only the receiver and args on the stack
        emitConstant(encodeObj(currentClass->klass->superclass));
        if(name > SHORT_CONSTANT_LIMIT){
            emitBytes(+OpCode::SUPER_INVOKE_LONG, argCount);
            emit16Bit(name);
        }
        else {
            emitBytes(+OpCode::SUPER_INVOKE, argCount);
            emitByte(name);
        }
        return true;
    }
    // Class methods can be accessed without 'this' keyword inside of methods and called
    return resolveImplicitObjectField(expr);*/
    return false;
}

// Turns a hash map lookup into an array linear search, but still faster than allocating memory using ObjString::createStr
// First bool in pair is if the search was succesful, second is if the field found was public or private
static std::pair<bool, bool> classContainsField(string& publicField, ankerl::unordered_dense::map<object::ObjString*, Value>& map){
    string privateField = "!" + publicField;
    for(auto it : map){
        if(publicField == it.first->str) return std::pair(true, true);
        else if(privateField == it.first->str) return std::pair(true, false);
    }
    return std::pair(false, false);
}
static std::pair<bool, bool> classContainsMethod(string& publicField, ankerl::unordered_dense::map<object::ObjString*, Obj*>& map){
    string privateField = "!" + publicField;
    for(auto it : map){
        if(publicField == it.first->str) return std::pair(true, true);
        else if(privateField == it.first->str) return std::pair(true, false);
    }
    return std::pair(false, false);
}

llvm::Value* Compiler::resolveClassField(Token name, bool canAssign){
    /*if(!currentClass) return -1;
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass->klass->fieldsInit);
    if(res.first){
        return makeConstant(encodeObj(ObjString::createStr((res.second ? "" : "!") + fieldName)));
    }

    res = classContainsMethod(fieldName, currentClass->klass->methods);
    if(res.first){
        if(canAssign) error(name, "Tried assigning to a method, which is forbidden.");
        return makeConstant(encodeObj(ObjString::createStr((res.second ? "" : "!") + fieldName)));
    }
    return -1;*/
    return nullptr;
}

// Makes sure the correct prefix is used when accessing private fields
// this.private_field -> this.!private_field
bool Compiler::resolveThis(AST::SetExpr *expr) {
    /*if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    int res = resolveClassField(name, true);
    if(res == -1) error(name, fmt::format("Field '{}' doesn't exist in class '{}'.", name.getLexeme(), currentClass->klass->name->str));

    expr->value->accept(this);
    expr->callee->accept(this);
    if (res <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::SET_PROPERTY, res);
    else emitByteAnd16Bit(+OpCode::SET_PROPERTY_LONG, res);*/
    return true;
}
bool Compiler::resolveThis(AST::FieldAccessExpr *expr) {
    /*if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    int res = resolveClassField(name, false);
    if(res == -1) error(name, fmt::format("Field '{}' doesn't exist in class '{}'.", name.getLexeme(), currentClass->klass->name->str));

    expr->callee->accept(this);
    if (res <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::GET_PROPERTY, res);
    else emitByteAnd16Bit(+OpCode::GET_PROPERTY_LONG, res);*/
    return true;
}
// Recognizes objectField() as an invoke operation
// If objectField() is encountered inside a closure which is inside a method, throw an error since only this.objectField() is allowed in closures
bool Compiler::resolveImplicitObjectField(AST::CallExpr *expr) {
    /*if(expr->callee->type != AST::ASTType::LITERAL) return false;
    Token name = probeToken(expr->callee);
    int res = resolveClassField(name, false);
    if(res == -1) return false;
    if(current->type == FuncType::TYPE_FUNC) error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
    namedVar(syntheticToken("this"), false);

    int8_t argCount = 0;
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
        argCount++;
    }
    if(res > SHORT_CONSTANT_LIMIT){
        emitBytes(+OpCode::INVOKE_LONG, argCount);
        emit16Bit(res);
    }
    else {
        emitBytes(+OpCode::INVOKE, argCount);
        emitByte(res);
    }
*/
    return true;
}

// Any call to getClassFromExpr assumes expr is either AST::LiteralExpr and AST::ModuleAccessExpr
ClassChunkInfo* Compiler::getClassFromExpr(AST::ASTNodePtr expr){
    if (expr->type == AST::ASTType::LITERAL) {
        Token symbol = probeToken(expr);
        // First check this module
        string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + symbol.getLexeme();
        auto it = globalClasses.find(fullSymbol);
        if (it != globalClasses.end()) return it->second;
        // Then check imported modules
        for (Dependency& dep : curUnit->deps) {
            fullSymbol = dep.module->file->name + std::to_string(dep.module->id) + symbol.getLexeme();
            if (dep.alias.type == TokenType::NONE && globals.contains(fullSymbol)) {
                return globalClasses.at(fullSymbol);
            }
        }
        error(symbol, "Class doesn't exist.");
    }
    else {
        auto moduleExpr = std::static_pointer_cast<AST::ModuleAccessExpr>(expr);

        // First find the module with the correct alias
        Dependency* depPtr = nullptr;
        for (Dependency dep : curUnit->deps) {
            if (dep.alias.equals(moduleExpr->moduleName)) {
                depPtr = &dep;
                break;
            }
        }
        if (depPtr == nullptr) {
            error(moduleExpr->moduleName, "Module alias doesn't exist.");
        }

        ESLModule* unit = depPtr->module;
        string fullSymbol = unit->file->name + std::to_string(unit->id) + moduleExpr->ident.getLexeme();
        if(globalClasses.contains(fullSymbol)) return globalClasses.at(fullSymbol);
        error(moduleExpr->ident, "Class doesn't exist.");
    }
}
#pragma endregion

void Compiler::error(const string& message) noexcept(false) {
    errorHandler::addSystemError("System compile error [line " + std::to_string(current->line) + "] in '" + curUnit->file->name + "': \n" + message + "\n");
    throw CompilerException();
}

void Compiler::error(Token token, const string& msg) noexcept(false) {
    errorHandler::addCompileError(msg, token);
    throw CompilerException();
}

llvm::Value* Compiler::endFuncDecl(int arity, string name) {
    if (!BBTerminated){
        emitReturn();
        BBTerminated = false;
    }
    // Get the parserCurrent function we've just compiled, delete it's compiler info, and replace it with the enclosing functions compiler info
    auto func = current->func;
    #ifdef COMPILER_DEBUG
    llvm::errs()<<"-------- Function disassembly for " + name + " --------\n";
    func->print(llvm::errs());
    #endif
    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
    // Set insertion point to the end of the enclosing function
    builder.SetInsertPoint(&current->func->back());
    auto typeErasedFn = llvm::ConstantExpr::getBitCast(func, builder.getInt8PtrTy());
    return builder.CreateCall(curModule->getFunction("createFunc"), {typeErasedFn, builder.getInt32(arity), createConstStr(name) });
}

//a little helper for updating the lines emitted by the compiler(used for displaying runtime errors)
void Compiler::updateLine(Token token) {
    current->line = token.str.line;
}

// For every dependency that's imported without an alias, check if any of its exports match 'symbol', return nullptr if not
llvm::Value* Compiler::checkSymbol(Token symbol) {
    for (Dependency& dep : curUnit->deps) {
        string fullSymbol = dep.module->file->name + std::to_string(dep.module->id) + "." + symbol.getLexeme();
        if (dep.alias.type == TokenType::NONE && globals.contains(fullSymbol)) {
            Globalvar& gvar = globals.at(fullSymbol);
            if(!gvar.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            else if(gvar.type == AST::ASTDeclType::CLASS) error(symbol, "Cannot read a class.");
            return gvar.val;
        }
    }
    return nullptr;
}

// Checks if 'symbol' is declared in the current file, if not passes it to checkSymbol
// returns nullptr if variable doesn't exist
llvm::Value* Compiler::resolveGlobal(Token symbol, bool canAssign) {
    string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." +symbol.getLexeme();
    auto it = globals.find(fullSymbol);
    if (canAssign) {
        // Global is in this module
        if (it != globals.end()){
            Globalvar& var = it->second;
            if(var.type == AST::ASTDeclType::FUNCTION) error(symbol, "Cannot assign to a function.");
            else if(var.type == AST::ASTDeclType::CLASS) error(symbol, "Cannot assign to a class.");

            if(!var.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            return var.val;
        }
        error(symbol, "Cannot assign to a variable not declared in this module.");
    }
    else {
        if (it != globals.end()) {
            if(!it->second.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }else if(it->second.type == AST::ASTDeclType::CLASS) error(symbol, "Cannot read a class.");
            return it->second.val;
        }
        else {
            // Global variables defined in an imported file are guaranteed to be already defined
            return checkSymbol(symbol);
        }
    }
    // Never hit, checkSymbol returns -1 upon failure
    return nullptr;
}

// Checks if 'variable' exists in a module which was imported with the alias 'moduleAlias',
// If it exists return llvm::Value* which holds the pointer to the global
llvm::Value* Compiler::resolveModuleVariable(Token moduleAlias, Token symbol) {
    // First find the module with the correct alias
    Dependency* depPtr = nullptr;
    for (Dependency dep : curUnit->deps) {
        if (dep.alias.equals(moduleAlias)) {
            depPtr = &dep;
            break;
        }
    }
    if (depPtr == nullptr) {
        error(moduleAlias, "Module alias doesn't exist.");
    }

    ESLModule* unit = depPtr->module;
    string fullSymbol = unit->file->name + std::to_string(unit->id) + symbol.getLexeme();
    if(globals.contains(fullSymbol)) return globals.at(fullSymbol).val;

    error(symbol, fmt::format("Module '{}' with alias '{}' doesn't export this symbol.", depPtr->pathString.getLexeme(), depPtr->alias.getLexeme()));
    // Never hit
    return nullptr;
}

llvm::Value* Compiler::evalASTExpr(std::shared_ptr<AST::ASTNode> node) {
    node->accept(this);
    return returnValue;
}

void Compiler::retVal(llvm::Value* val){
    returnValue = builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}

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

llvm::Constant* Compiler::createConstStr(string str){
    if(stringConstants.contains(str)) return stringConstants[str];
    auto constant = builder.CreateGlobalStringPtr(str, "internalString", 0, curModule.get());
    stringConstants[str] = constant;
    return constant;
}

void Compiler::createTyErr(string err, llvm::Value* val){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(curUnit->file->path);
    llvm::Constant* line = builder.getInt32(current->line);
    builder.CreateCall(curModule->getFunction("tyErrSingle"), {str, file, line, val});
}
void Compiler::createTyErr(string err, llvm::Value* lhs, llvm::Value* rhs){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(curUnit->file->path);
    llvm::Constant* line = builder.getInt32(current->line);
    builder.CreateCall(curModule->getFunction("tyErrDouble"), {str, file, line, lhs, rhs});
}

llvm::Value* Compiler::castToVal(llvm::Value* val){
    return builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}

void Compiler::createGcSafepoint(){
    auto cmp = builder.CreateCall(curModule->getFunction("gcSafepoint"));
    auto func = builder.CreateSelect(cmp, curModule->getFunction("stopThread"), curModule->getFunction("noop"));
    builder.CreateCall(llvm::FunctionType::get(builder.getVoidTy(), false), func);
}

void Compiler::createNewFunc(int argCount, string name, FuncType type){
    // Create a function type with the appropriate number of arguments
    vector<llvm::Type*> params;
    for(int i = 0; i < argCount; i++) params.push_back(builder.getInt64Ty());
    llvm::FunctionType* fty = llvm::FunctionType::get(builder.getInt64Ty(), params, false);

    auto tmp = llvm::Function::Create(fty, llvm::Function::PrivateLinkage, name, curModule.get());
    current = new CurrentChunkInfo(current, type, tmp);
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*ctx, "entry", current->func);
    builder.SetInsertPoint(BB);
}
#pragma endregion

// Only used when debugging _LONG versions of op codes
#undef SHORT_CONSTANT_LIMIT

#undef CHECK_SCOPE_FOR_LOOP
#undef CHECK_SCOPE_FOR_SWITCH
