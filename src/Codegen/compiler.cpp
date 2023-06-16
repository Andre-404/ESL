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
//using namespace valueHelpers;


#ifdef COMPILER_USE_LONG_INSTRUCTION
#define SHORT_CONSTANT_LIMIT 0
#else
#define SHORT_CONSTANT_LIMIT UINT8_MAX
#endif

//only checks the closest loop/switch, since any break, continue or advance is going to break out of that loop/switch
#define CHECK_SCOPE_FOR_LOOP (current->scopeWithLoop.size() > 0 && local.depth <= current->scopeWithLoop.back())
#define CHECK_SCOPE_FOR_SWITCH (current->scopeWithSwitch.size() > 0 && local.depth <= current->scopeWithSwitch.back())

CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type) : enclosing(_enclosing), type(_type) {
    upvalues = std::array<Upvalue, UPVAL_MAX>();
    hasReturnStmt = false;
    hasCapturedLocals = false;
    localCount = 0;
    scopeDepth = 0;
    line = 0;
    // If a constructor or a method is being compiled, it implicitly declares "this" as the first slot
    if (!(type == FuncType::TYPE_CONSTRUCTOR || type == FuncType::TYPE_METHOD)) {
        // First slot is claimed for function name
        Local* local = &locals[localCount++];
        local->depth = 0;
        local->name = "";
    }
    chunk = Chunk();
    func = new ObjFunc();
}



Compiler::Compiler(vector<ESLModule*>& _units) : ctx(std::make_unique<llvm::LLVMContext>()), builder(llvm::IRBuilder<>(*ctx)) {
    upvalueFinder::UpvalueFinder f(_units);
    current = new CurrentChunkInfo(nullptr, FuncType::TYPE_SCRIPT);
    baseClass = new object::ObjClass("base class", nullptr);
    baseClass->methods.insert_or_assign(ObjString::createStr("to_string"), new object::ObjNativeFunc([](runtime::Thread* thread, int8_t argCount){
        thread->push(encodeObj(object::ObjString::createStr(valueHelpers::toString(thread->pop()))));
    }, 0, "to_string"));
    currentClass = nullptr;
    curUnitIndex = 0;
    curGlobalIndex = 0;
    units = _units;
    nativeFuncs = runtime::createNativeFuncs();
    nativeFuncNames = runtime::createNativeNameTable(nativeFuncs);

    curModule = std::make_unique<llvm::Module>("Module", *ctx);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    auto err = llvm::orc::KaleidoscopeJIT::Create().moveInto(JIT);
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    curModule->setDataLayout(JIT->getDataLayout());
    curModule->setTargetTriple(targetTriple);

    llvmHelpers::addHelperFunctionsToModule(curModule, ctx, builder);

    compile();
}

void Compiler::compile(){
    // Create a new basic block to start insertion into.
    llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx), false);
    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "anon", curModule.get());
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx, "entry", F);
    builder.SetInsertPoint(BB);

    for (ESLModule* unit : units) {
        curUnit = unit;
        sourceFiles.push_back(unit->file);
        for (const auto decl : unit->topDeclarations) {
            globals.emplace_back(decl->getName().getLexeme(), encodeNil());
            definedGlobals.push_back(false);
        }
        for (int i = 0; i < unit->stmts.size(); i++) {
            //doing this here so that even if a error is detected, we go on and possibly catch other(valid) errors
            try {
                unit->stmts[i]->accept(this);
            }
            catch (CompilerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curGlobalIndex = globals.size();
        curUnitIndex++;
    }
    for (ESLModule* unit : units) delete unit;

    //Temporary
    builder.CreateCall(curModule->getFunction("print"), returnValue);
    builder.CreateRetVoid();
    llvm::verifyFunction(*F);
    curModule->print(llvm::errs(), nullptr);
    llvmHelpers::runModule(curModule, JIT, ctx, true);
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

}

void Compiler::visitSetExpr(AST::SetExpr* expr) {

}

void Compiler::visitConditionalExpr(AST::ConditionalExpr* expr) {

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
        return builder.CreateBitCast(val, llvm::Type::getDoubleTy(*ctx));
    };
    // TODO: this can break if val > 2^63
    auto dblToInt = [&](llvm::Value* val){
        return builder.CreateFPToSI(val, llvm::Type::getInt64Ty(*ctx));
    };
    auto matchTT = [&](TokenType type, std::initializer_list<TokenType> list){
        for(auto it = list.begin(); it != list.end(); it++){
            if(*it == type) return true;
        }
        return false;
    };

    llvm::Value* lhs = visitASTNode(expr->left.get());
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
        llvm::Value* rhs = builder.CreateCall(castToBool, visitASTNode(expr->right.get()));
        builder.CreateBr(mergeBB);
        // In case we have a nested 'or' or 'and' lhsFalseBB could no longer be the block the builder is emitting to
        evalRhsBB = builder.GetInsertBlock();
        // Emit merge block, code from this point on will be generated into this block
        func->insert(func->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        llvm::PHINode *PN = builder.CreatePHI(llvm::Type::getInt1Ty(*ctx), 2, op == TokenType::OR ? "lortmp" : "landtmp");

        // If we're coming from the originalBB and the operator is 'or' it means that lhs is true, and thus the entire expression is true
        // For 'and' its the opposite, if lhs is false, then the entire expression is false
        PN->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*ctx), op == TokenType::OR ? true : false), originalBB);
        // For both operators, if control flow is coming from evalRhsBB ths becomes the value of the entire expression
        PN->addIncoming(rhs, evalRhsBB);

        // Cast the bool to a Value
        returnValue = builder.CreateCall(castToVal, PN);
        return;
    }

    llvm::Value* rhs = visitASTNode(expr->right.get());
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
        llvm::Constant* line = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), current->line);
        // Returns Value
        auto stringAddRes = builder.CreateCall(curModule->getFunction("strAdd"), {lhs, rhs, file, line});
        builder.CreateBr(mergeBB);

        // Final destination for both branches, if both values were numbers or strings(meaning no error was thrown)
        // Use a phi node to determine which one it is and then set it as returnValue
        F->insert(F->end(), mergeBB);
        builder.SetInsertPoint(mergeBB);
        auto phi = builder.CreatePHI(llvm::Type::getInt64Ty(*ctx), 2);
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
        retVal(builder.CreateSIToFP(res, llvm::Type::getDoubleTy(*ctx)));
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
    if (expr->op.type == TokenType::INCREMENT || expr->op.type == TokenType::DECREMENT) {
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
        llvm::Value* rhs = visitASTNode(expr->right.get());
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
                    auto tmp = builder.CreateBitCast(rhs, llvm::Type::getDoubleTy(*ctx));
                    auto negated = builder.CreateNot(builder.CreateFPToSI(tmp, llvm::Type::getInt64Ty(*ctx)),"binnegtmp");
                    retVal(builder.CreateSIToFP(negated, llvm::Type::getDoubleTy(*ctx)));
                }else{
                    auto tmp = builder.CreateBitCast(rhs, llvm::Type::getDoubleTy(*ctx));
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
                returnValue = builder.CreateXor(rhs, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), MASK_TYPE_TRUE));
                break;
            }

        }
        return;
    }
    error(expr->op, "Unexpected operator.");
}

void Compiler::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    //we need all of the array member values to be on the stack prior to executing "OP_CREATE_ARRAY"
    //compiling members in reverse order because we add to the array by popping from the stack
    for(auto mem : expr->members){
        mem->accept(this);
    }
    emitBytes(+OpCode::CREATE_ARRAY, expr->members.size());
}

void Compiler::visitCallExpr(AST::CallExpr* expr) {
    // Invoking is field access + call, when the compiler recognizes this pattern it optimizes
    if (invoke(expr)) return;
    //todo: tail recursion optimization
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->args.size());
}

void Compiler::visitNewExpr(AST::NewExpr* expr){
    // Parser guarantees that expr->call->callee is either a literal or a module access
    auto klass = getClassFromExpr(expr->call->callee);

    emitConstant(encodeObj(klass));

    for (AST::ASTNodePtr arg : expr->call->args) {
        arg->accept(this);
    }
    emitBytes(+OpCode::CALL, expr->call->args.size());
}

void Compiler::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    updateLine(expr->accessor);
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
    return;
}

void Compiler::visitStructLiteralExpr(AST::StructLiteral* expr) {
    vector<int> constants;

    bool isLong = false;
    //for each field, compile it and get the constant of the field name
    for (AST::StructEntry entry : expr->fields) {
        entry.expr->accept(this);
        updateLine(entry.name);
        //this gets rid of quotes, ""Hello world""->"Hello world"
        string temp = entry.name.getLexeme();
        temp.erase(0, 1);
        temp.erase(temp.size() - 1, 1);
        uint16_t num = makeConstant(encodeObj(ObjString::createStr(temp)));
        if (num > SHORT_CONSTANT_LIMIT) isLong = true;
        constants.push_back(num);
    }
    //since the amount of fields is variable, we emit the number of fields follwed by constants for each field
    //constants are emitted in reverse order, because we get the values by popping them from stack(reverse order from which they were pushed)
    if (!isLong) {
        emitBytes(+OpCode::CREATE_STRUCT, constants.size());

        for (int i = constants.size() - 1; i >= 0; i--) emitByte(constants[i]);
    }
    else {
        emitBytes(+OpCode::CREATE_STRUCT_LONG, constants.size());

        for (int i = constants.size() - 1; i >= 0; i--) emit16Bit(constants[i]);
    }
}

static std::pair<bool, bool> classContainsMethod(string& publicField, ankerl::unordered_dense::map<object::ObjString*, Method>& map);

void Compiler::visitSuperExpr(AST::SuperExpr* expr) {
    int name = identifierConstant(expr->methodName);
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
            returnValue = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), encodeBool(expr->token.type == TokenType::TRUE));
            break;
        }
        case TokenType::NIL: returnValue = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), encodeNil()); break;
        case TokenType::STRING: {
            //this gets rid of quotes, ""Hello world""->"Hello world"
            string temp = expr->token.getLexeme();
            temp.erase(0, 1);
            temp.erase(temp.size() - 1, 1);
            auto str = builder.CreateGlobalStringPtr(temp, "internalString");
            // Returns Value, no need to cast
            returnValue = builder.CreateCall(curModule->getFunction("createStr"), str);
            break;
        }

        case TokenType::THIS: {
            if (currentClass == nullptr) error(expr->token, "Can't use keyword 'this' outside of a class.");
            //'this' get implicitly defined by the compiler
            namedVar(expr->token, false);
            break;
        }
        case TokenType::IDENTIFIER: {
            namedVar(expr->token, false);
            break;
        }
    }
}

void Compiler::visitFuncLiteral(AST::FuncLiteral* expr) {
    //creating a new compilerInfo sets us up with a clean slate for writing bytecode, the enclosing functions info
    //is stored in parserCurrent->enclosing
    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC);
    //no need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    //we define the args as locals, when the function is called, the args will be sitting on the stack in order
    //we just assign those positions to each arg
    for (AST::ASTVar& var : expr->args) {
        declareLocalVar(var);
        defineLocalVar();
    }
    for(auto stmt : expr->body->statements){
        try {
            stmt->accept(this);
        }catch(CompilerException e){

        }
    }
    current->func->arity = expr->args.size();
    current->func->name = "Anonymous function";
    //have to do this here since endFuncDecl() deletes the compilerInfo
    std::array<Upvalue, UPVAL_MAX> upvals = current->upvalues;
    ObjFunc* func = endFuncDecl();

    //if there are no upvalues captured, compile the function enclosed in a closure and we're done
    if (func->upvalueCount == 0) {
        emitConstant(encodeObj(new ObjClosure(func)));
        return;
    }

    uint16_t constant = makeConstant(encodeObj(func));
    if (constant <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::CLOSURE, constant);
    else emitByteAnd16Bit(+OpCode::CLOSURE_LONG, constant);
    //if this function does capture any upvalues, we emit the code for getting them,
    //when we execute "OP_CLOSURE" we will check to see how many upvalues the function captures by going directly to the func->upvalueCount
    for (int i = 0; i < func->upvalueCount; i++) {
        emitByte(upvals[i].isLocal ? 1 : 0);
        emitByte(upvals[i].index);
    }
}

void Compiler::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    uint16_t arg = resolveModuleVariable(expr->moduleName, expr->ident);

    if (arg > SHORT_CONSTANT_LIMIT) {
        emitByteAnd16Bit(+OpCode::GET_GLOBAL_LONG, arg);
        return;
    }
    emitBytes(+OpCode::GET_GLOBAL, arg);
}

// This shouldn't ever be visited as every macro should be expanded before compilation
void Compiler::visitMacroExpr(AST::MacroExpr* expr) {
    error("Non-expanded macro encountered during compilation.");
}

void Compiler::visitAsyncExpr(AST::AsyncExpr* expr) {
    updateLine(expr->token);
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

void Compiler::visitVarDecl(AST::VarDecl* decl) {
    uint16_t global;
    if(decl->var.type == AST::ASTVarType::GLOBAL){
        global = declareGlobalVar(decl->var.name);
    }else declareLocalVar(decl->var);
    // Compile the right side of the declaration, if there is no right side, the variable is initialized as nil
    AST::ASTNodePtr expr = decl->value;
    if (expr == nullptr) {
        emitByte(+OpCode::NIL);
    }
    else {
        expr->accept(this);
    }
    if(decl->var.type == AST::ASTVarType::GLOBAL){
        defineGlobalVar(global);
        if(global <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::SET_GLOBAL, global);
        else emitByteAnd16Bit(+OpCode::SET_GLOBAL_LONG, global);
        emitByte(+OpCode::POP);
    }else defineLocalVar();
}

void Compiler::visitFuncDecl(AST::FuncDecl* decl) {
    uint16_t index = declareGlobalVar(decl->getName());
    // Defining the function here to allow for recursion
    defineGlobalVar(index);
    //creating a new compilerInfo sets us up with a clean slate for writing bytecode, the enclosing functions info
    //is stored in parserCurrent->enclosing
    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC);
    //no need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    //we define the args as locals, when the function is called, the args will be sitting on the stack in order
    //we just assign those positions to each arg
    for (AST::ASTVar& var : decl->args) {
        declareLocalVar(var);
        defineLocalVar();
    }
    for(auto stmt : decl->body->statements){
        try {
            stmt->accept(this);
        }catch(CompilerException e){

        }
    }
    current->func->arity = decl->args.size();
    current->func->name = decl->getName().getLexeme();
    //have to do this here since endFuncDecl() deletes the compilerInfo
    std::array<Upvalue, UPVAL_MAX> upvals = current->upvalues;

    ObjFunc* func = endFuncDecl();

    //Not possible but just in case
    if(func->upvalueCount != 0){
        error(decl->getName(), "Global function with upvalues detected, aborting...");
    }
    // Assigning at compile time to save on bytecode
    globals[index].val = encodeObj(new ObjClosure(func));
}

void Compiler::visitClassDecl(AST::ClassDecl* decl) {
    Token className = decl->getName();
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
        klass->methods = baseClass->methods;
        klass->superclass = baseClass;
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
}

void Compiler::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
    emitByte(+OpCode::POP);
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
    //compile condition and emit a jump over then branch if the condition is false
    stmt->condition->accept(this);
    int thenJump = emitJump(+OpCode::JUMP_IF_FALSE_POP);
    stmt->thenBranch->accept(this);
    //only compile if there is a else branch
    if (stmt->elseBranch != nullptr) {
        //prevents fallthrough to else branch
        int elseJump = emitJump(+OpCode::JUMP);
        patchJump(thenJump);

        stmt->elseBranch->accept(this);
        patchJump(elseJump);
    }
    else patchJump(thenJump);

}

void Compiler::visitWhileStmt(AST::WhileStmt* stmt) {
    //loop inversion is applied to reduce the number of jumps if the condition is met
    stmt->condition->accept(this);
    int jump = emitJump(+OpCode::JUMP_IF_FALSE_POP);
    //if the condition is true, compile the body and then the condition again and if its true we loop back to the start of the body
    int loopStart = getChunk()->bytecode.size();
    //loop body gets it's own scope because we only patch break and continue jumps which are declared in higher scope depths
    //user might not use {} block when writing a loop, this ensures the body is always in it's own scope
    current->scopeWithLoop.push_back(current->scopeDepth);
    beginScope();
    stmt->body->accept(this);
    endScope();
    current->scopeWithLoop.pop_back();
    //continue skips the rest of the body and evals the condition again
    patchScopeJumps(ScopeJumpType::CONTINUE);
    stmt->condition->accept(this);
    emitLoop(loopStart);
    //break out of the loop
    patchJump(jump);
    patchScopeJumps(ScopeJumpType::BREAK);
}

void Compiler::visitForStmt(AST::ForStmt* stmt) {
    //we wrap this in a scope so if there is a var declaration in the initialization it's scoped to the loop
    beginScope();
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
    endScope();
}

void Compiler::visitBreakStmt(AST::BreakStmt* stmt) {
    //the amount of variables to pop and the amount of code to jump is determined in patchScopeJumps()
    //which is called at the end of loops or a switch
    updateLine(stmt->token);
    int toPop = 0;
    //since the body of the loop is always in its own scope, and the scope before it is declared as having a loop,
    //we pop locals until the first local that is in the same scope as the loop
    //meaning it was declared outside of the loop body and shouldn't be popped
    //break is a special case because it's used in both loops and switches, so we find either in the scope we're check, we bail
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local& local = current->locals[i];
        if (local.depth != -1 && (CHECK_SCOPE_FOR_LOOP || CHECK_SCOPE_FOR_SWITCH)) break;
        toPop++;
    }
    if (toPop > UINT8_MAX) {
        error(stmt->token, "To many variables to pop.");
    }
    emitByte(+ScopeJumpType::BREAK);
    int breakJump = getChunk()->bytecode.size();
    emitBytes((current->scopeDepth >> 8) & 0xff, current->scopeDepth & 0xff);
    emitByte(toPop);
    current->scopeJumps.push_back(breakJump);
}

void Compiler::visitContinueStmt(AST::ContinueStmt* stmt) {
    //the amount of variables to pop and the amount of code to jump is determined in patchScopeJumps()
    //which is called at the end of loops
    updateLine(stmt->token);
    int toPop = 0;
    //since the body of the loop is always in its own scope, and the scope before it is declared as having a loop,
    //we pop locals until the first local that is in the same scope as the loop
    //meaning it was declared outside of the loop body and shouldn't be popped
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local& local = current->locals[i];
        if (local.depth != -1 && CHECK_SCOPE_FOR_LOOP) break;
        toPop++;
    }
    if (toPop > UINT8_MAX) {
        error(stmt->token, "To many variables to pop.");
    }
    emitByte(+ScopeJumpType::CONTINUE);
    int continueJump = getChunk()->bytecode.size();
    emitBytes((current->scopeDepth >> 8) & 0xff, current->scopeDepth & 0xff);
    emitByte(toPop);
    current->scopeJumps.push_back(continueJump);
}

void Compiler::visitSwitchStmt(AST::SwitchStmt* stmt) {
    current->scopeWithSwitch.push_back(current->scopeDepth);
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
    patchScopeJumps(ScopeJumpType::BREAK);
}

void Compiler::visitCaseStmt(AST::CaseStmt* stmt) {
    //compile every statement in the case
    for (AST::ASTNodePtr caseStmt : stmt->stmts) {
        caseStmt->accept(this);
    }
}

void Compiler::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    //the amount of variables to pop and the amount of code to jump is determined in patchScopeJumps()
    //which is called at the start of each case statement(excluding the first)
    updateLine(stmt->token);
    int toPop = 0;
    //advance can only be used inside a case of a switch statement, and when jumping, jumps to the next case
    //case body is compiled in its own scope, so advance is always in a scope higher than it's switch statement
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local& local = current->locals[i];
        if (local.depth != -1 && CHECK_SCOPE_FOR_SWITCH) break;
        toPop++;
    }
    if (toPop > UINT8_MAX) {
        error(stmt->token, "To many variables to pop.");
    }
    emitByte(+ScopeJumpType::ADVANCE);
    int advanceJump = getChunk()->bytecode.size();
    emitBytes((current->scopeDepth >> 8) & 0xff, current->scopeDepth & 0xff);
    emitByte(toPop);
    current->scopeJumps.push_back(advanceJump);
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
    stmt->expr->accept(this);
    emitByte(+OpCode::RETURN);
}

#pragma region helpers

#pragma region Emitting bytes

void Compiler::emitByte(byte byte) {
    //line is incremented whenever we find a statement/expression that contains tokens
    getChunk()->writeData(byte, current->line, sourceFiles.size() - 1);
}

void Compiler::emitBytes(byte byte1, byte byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

void Compiler::emit16Bit(uInt16 number) {
    //Big endian
    emitBytes((number >> 8) & 0xff, number & 0xff);
}

void Compiler::emitByteAnd16Bit(byte byte, uInt16 num) {
    emitByte(byte);
    emit16Bit(num);
}

uint16_t Compiler::makeConstant(Value value) {
    uInt constant = getChunk()->addConstant(value);
    if (constant > UINT16_MAX) {
        error("Too many constants in one chunk.");
    }
    return constant;
}

void Compiler::emitConstant(Value value) {
    // Shorthand for adding a constant to the chunk and emitting it
    uint16_t constant = makeConstant(value);
    if (constant <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::CONSTANT, constant);
    else emitByteAnd16Bit(+OpCode::CONSTANT_LONG, constant);
}

void Compiler::emitReturn() {
    //in a constructor, the first local variable refers to the new instance of a class('this')
    if (current->type == FuncType::TYPE_CONSTRUCTOR) emitBytes(+OpCode::GET_LOCAL, 0);
    else emitByte(+OpCode::NIL);
    emitByte(+OpCode::RETURN);
}

int Compiler::emitJump(byte jumpType) {
    emitByte(jumpType);
    emitBytes(0xff, 0xff);
    return getChunk()->bytecode.size() - 2;
}

void Compiler::patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = getChunk()->bytecode.size() - offset - 2;
    //fix for future: insert 2 more bytes into the array, but make sure to do the same in lines array
    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }
    getChunk()->bytecode[offset] = (jump >> 8) & 0xff;
    getChunk()->bytecode[offset + 1] = jump & 0xff;
}

void Compiler::emitLoop(int start) {
    emitByte(+OpCode::LOOP_IF_TRUE);

    int offset = getChunk()->bytecode.size() - start + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emit16Bit(offset);
}

void Compiler::patchScopeJumps(ScopeJumpType type) {
    int curCode = getChunk()->bytecode.size();
    //most recent jumps are going to be on top
    for (int i = current->scopeJumps.size() - 1; i >= 0; i--) {
        uint32_t jumpPatchPos = current->scopeJumps[i];
        byte jumpType = getChunk()->bytecode[jumpPatchPos - 1];
        uint32_t jumpDepth = (getChunk()->bytecode[jumpPatchPos] << 8) | getChunk()->bytecode[jumpPatchPos + 1];
        uint32_t toPop = getChunk()->bytecode[jumpPatchPos + 2];
        //break and advance statements which are in a strictly deeper scope get patched, on the other hand
        //continue statements which are in parserCurrent or a deeper scope get patched
        if (jumpDepth > current->scopeDepth && +type == jumpType) {
            int jumpLength = curCode - jumpPatchPos - 3;
            if (jumpLength > UINT16_MAX) error("Too much code to jump over.");
            if (toPop > UINT8_MAX) error("Too many variables to pop.");

            getChunk()->bytecode[jumpPatchPos - 1] = +OpCode::JUMP_POPN;
            //variables declared by the time we hit the break whose depth is lower or equal to this break stmt
            getChunk()->bytecode[jumpPatchPos] = toPop;
            //amount to jump
            getChunk()->bytecode[jumpPatchPos + 1] = (jumpLength >> 8) & 0xff;
            getChunk()->bytecode[jumpPatchPos + 2] = jumpLength & 0xff;

            current->scopeJumps.erase(current->scopeJumps.begin() + i);
        }
            //any jump after the one that has a depth lower than the parserCurrent one will also have a lower depth, thus we bail
        else if (jumpDepth < current->scopeDepth) break;
    }
}

#pragma endregion

#pragma region Variables

//creates a string constant from a token
uint16_t Compiler::identifierConstant(Token name) {
    updateLine(name);
    string temp = name.getLexeme();
    return makeConstant(encodeObj(ObjString::createStr(temp)));
}

// If this is a local var, mark it as ready and then bail out, otherwise emit code to add the variable to the global table
void Compiler::defineGlobalVar(uInt16 index) {
    definedGlobals[index] = true;
}

// Gets/sets a variable, respects the scoping rules(locals->upvalues->class fields(if inside a method)->globals)
void Compiler::namedVar(Token token, bool canAssign) {
    updateLine(token);
    byte getOp;
    byte setOp;
    int arg = resolveLocal(token);
    if (arg != -1) {
        getOp = +OpCode::GET_LOCAL;
        setOp = +OpCode::SET_LOCAL;
        if(current->locals[arg].isLocalUpvalue){
            getOp = +OpCode::GET_LOCAL_UPVALUE;
            setOp = +OpCode::SET_LOCAL_UPVALUE;
        }
    }
    else if ((arg = resolveUpvalue(current, token)) != -1) {
        getOp = +OpCode::GET_UPVALUE;
        setOp = +OpCode::SET_UPVALUE;
    }
    else if((arg = resolveClassField(token, canAssign)) != -1){
        if(current->type == FuncType::TYPE_FUNC){
            error(token, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", token.getLexeme()));
        }
        getOp = +OpCode::GET_PROPERTY_EFFICIENT;
        setOp = +OpCode::SET_PROPERTY_EFFICIENT;
        emitByteAnd16Bit((canAssign ? setOp : getOp), arg);
        return;
    }
    else if((arg = resolveGlobal(token, canAssign)) != -1 && arg != -2){
        // All global variables are stored in an array, resolveGlobal gets index into the array
        getOp = +OpCode::GET_GLOBAL;
        setOp = +OpCode::SET_GLOBAL;
        // Classes cannot be accessed with variable get/set, only way they can be accessed is when inheriting,
        // within the 'instanceof' operator and within the 'new' operator
        if(isClass(globals[arg].val)) error(token, "Cannot access or mutate classes.");

        if (arg > SHORT_CONSTANT_LIMIT) {
            getOp = +OpCode::GET_GLOBAL_LONG;
            setOp = +OpCode::SET_GLOBAL_LONG;
            emitByteAnd16Bit((canAssign ? setOp : getOp), arg);
            return;
        }
    }
    else{
        string name = token.getLexeme();
        auto it = nativeFuncNames.find(name);
        if(it == nativeFuncNames.end()){
            if(arg == -1) error(token, fmt::format("'{}' doesn't match any declared variable name or native function name.", name));
            else if(arg == -2) error(token, fmt::format("Trying to access variable '{}' before it's initialized.", name));
        }

        emitByteAnd16Bit(+OpCode::GET_NATIVE, it->second);
        return;
    }
    emitBytes(canAssign ? setOp : getOp, arg);
}

// If 'name' is a global variable it's index in the globals array is returned
// Otherwise, if 'name' is a local variable it's passed to declareVar()
uint16_t Compiler::declareGlobalVar(Token name) {
    updateLine(name);
    // Searches for the index of the global variable in the current file
    // (globals.size() - curGlobalIndex = globals declared in current file)
    int index = curGlobalIndex;
    for (int i = curGlobalIndex; i < globals.size(); i++) {
        if (name.getLexeme() == globals[i].name) return i;
    }
    // Should never be hit, but here just in case
    error(name, "Couldn't find variable.");
    return 0;
}

// Makes sure the compiler is aware that a stack slot is occupied by this local variable
void Compiler::declareLocalVar(AST::ASTVar& var) {
    updateLine(var.name);
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }
        string str = var.name.getLexeme();
        if (str.compare(local->name) == 0) {
            error(var.name, "Already a variable with this name in this scope.");
        }
    }
    addLocal(var);
}

// If a variable is declared as "
void Compiler::addLocal(AST::ASTVar var) {
    updateLine(var.name);
    if (current->localCount == LOCAL_MAX) {
        error(var.name, "Too many local variables in function.");
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name = var.name.getLexeme();
    local->depth = -1;
    local->isLocalUpvalue = var.type == AST::ASTVarType::LOCAL_UPVALUE;
}

void Compiler::beginScope() {
    current->scopeDepth++;
}

void Compiler::endScope() {
    //Pop every variable that was declared in this scope
    current->scopeDepth--;//first lower the scope, the check for every var that is deeper than the parserCurrent scope
    int toPop = 0;
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        toPop++;
        current->localCount--;
    }
    if (toPop > 0) {
        if (toPop == 1) {
            emitByte(+OpCode::POP);
            return;
        }
        emitBytes(+OpCode::POPN, toPop);
    }
}

int Compiler::resolveLocal(CurrentChunkInfo* func, Token name) {
    //checks to see if there is a local variable with a provided name, if there is return the index of the stack slot of the var
    updateLine(name);
    for (int i = func->localCount - 1; i >= 0; i--) {
        Local* local = &func->locals[i];
        string str = name.getLexeme();
        if (str.compare(local->name) == 0) {
            if (local->depth == -1) {
                error(name, "Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

int Compiler::resolveLocal(Token name) {
    return resolveLocal(current, name);
}

int Compiler::resolveUpvalue(CurrentChunkInfo* func, Token name) {
    if (func->enclosing == nullptr) return -1;

    int local = resolveLocal(func->enclosing, name);
    if (local != -1) {
        func->enclosing->hasCapturedLocals = true;
        return addUpvalue(func, (uint8_t)local, true);
    }
    int upvalue = resolveUpvalue(func->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(func, (uint8_t)upvalue, false);
    }

    return -1;
}

int Compiler::addUpvalue(CurrentChunkInfo* func, byte index, bool isLocal) {
    int upvalueCount = func->func->upvalueCount;
    //first check if this upvalue has already been captured
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upval = &func->upvalues[i];
        if (upval->index == index && upval->isLocal == isLocal) {
            return i;
        }
    }
    if (upvalueCount == UPVAL_MAX) {
        error("Too many closure variables in function.");
        return 0;
    }
    func->upvalues[upvalueCount].isLocal = isLocal;
    func->upvalues[upvalueCount].index = index;
    return func->func->upvalueCount++;
}

// Marks the local at the top of the stack as ready to use
void Compiler::defineLocalVar() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
    if(current->locals[current->localCount - 1].isLocalUpvalue) {
        emitBytes(+OpCode::CREATE_UPVALUE, current->localCount - 1);
    }
}

Token Compiler::syntheticToken(string str) {
    return Token(TokenType::IDENTIFIER, str);
}

#pragma endregion

#pragma region Classes and methods
object::ObjClosure* Compiler::method(AST::FuncDecl* _method, Token className) {
    updateLine(_method->getName());
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
    return new ObjClosure(func);
}

bool Compiler::invoke(AST::CallExpr* expr) {
    if (expr->callee->type == AST::ASTType::FIELD_ACCESS) {
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
    return resolveImplicitObjectField(expr);
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
static std::pair<bool, bool> classContainsMethod(string& publicField, ankerl::unordered_dense::map<object::ObjString*, Method>& map){
    string privateField = "!" + publicField;
    for(auto it : map){
        if(publicField == it.first->str) return std::pair(true, true);
        else if(privateField == it.first->str) return std::pair(true, false);
    }
    return std::pair(false, false);
}

int Compiler::resolveClassField(Token name, bool canAssign){
    if(!currentClass) return -1;
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
    return -1;
}

// Makes sure the correct prefix is used when accessing private fields
// this.private_field -> this.!private_field
bool Compiler::resolveThis(AST::SetExpr *expr) {
    if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    int res = resolveClassField(name, true);
    if(res == -1) error(name, fmt::format("Field '{}' doesn't exist in class '{}'.", name.getLexeme(), currentClass->klass->name->str));

    expr->value->accept(this);
    expr->callee->accept(this);
    if (res <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::SET_PROPERTY, res);
    else emitByteAnd16Bit(+OpCode::SET_PROPERTY_LONG, res);
    return true;
}
bool Compiler::resolveThis(AST::FieldAccessExpr *expr) {
    if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    int res = resolveClassField(name, false);
    if(res == -1) error(name, fmt::format("Field '{}' doesn't exist in class '{}'.", name.getLexeme(), currentClass->klass->name->str));

    expr->callee->accept(this);
    if (res <= SHORT_CONSTANT_LIMIT) emitBytes(+OpCode::GET_PROPERTY, res);
    else emitByteAnd16Bit(+OpCode::GET_PROPERTY_LONG, res);
    return true;
}
// Recognizes object_field() as an invoke operation
// If object_field() is encountered inside a closure which is inside a method, throw an error since only this.object_field() is allowed in closures
bool Compiler::resolveImplicitObjectField(AST::CallExpr *expr) {
    if(expr->callee->type != AST::ASTType::LITERAL) return false;
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

    return true;
}

// Any call to getClassFromExpr assumes expr is either AST::LiteralExpr and AST::ModuleAccessExpr
object::ObjClass* Compiler::getClassFromExpr(AST::ASTNodePtr expr){
    uint16_t classIndex = 0;
    Token token;
    if (expr->type == AST::ASTType::LITERAL) {
        Token superclass = probeToken(expr);
        // Gets the index into globals in which the superclass is stored
        int arg = resolveGlobal(superclass, false);
        if(arg == -1) error(superclass, "Class doesn't exist.");
        else if(arg == -2) {
            error(superclass, fmt::format("Trying to access variable '{}' before it's initialized.", superclass.getLexeme()));
        }
        classIndex = arg;
        token = superclass;
    }
    else {
        auto moduleExpr = std::static_pointer_cast<AST::ModuleAccessExpr>(expr);
        classIndex = resolveModuleVariable(moduleExpr->moduleName, moduleExpr->ident);
        token = moduleExpr->ident;
    }
    if(!isClass(globals[classIndex].val)) error(token, "Variable isn't a class.");
    return asClass(globals[classIndex].val);
}
#pragma endregion

Chunk* Compiler::getChunk() {
    return &current->chunk;
}

void Compiler::error(const string& message) noexcept(false) {
    errorHandler::addSystemError("System compile error [line " + std::to_string(current->line) + "] in '" + curUnit->file->name + "': \n" + message + "\n");
    throw CompilerException();
}

void Compiler::error(Token token, const string& msg) noexcept(false) {
    errorHandler::addCompileError(msg, token);
    throw CompilerException();
}

ObjFunc* Compiler::endFuncDecl() {
    if (!current->hasReturnStmt) emitReturn();
    // Get the parserCurrent function we've just compiled, delete it's compiler info, and replace it with the enclosing functions compiler info
    ObjFunc* func = current->func;
    Chunk& chunk = current->chunk;

    //Add the bytecode, lines and constants to the main code block
    uInt64 bytecodeOffset = mainCodeBlock.bytecode.size();
    mainCodeBlock.bytecode.insert(mainCodeBlock.bytecode.end(), chunk.bytecode.begin(), chunk.bytecode.end());
    uInt64 constantsOffset = mainCodeBlock.constants.size();
    mainCodeBlock.constants.insert(mainCodeBlock.constants.end(), chunk.constants.begin(), chunk.constants.end());
    // For the last line of code
    chunk.lines[chunk.lines.size() - 1].end = chunk.bytecode.size();
    // Update lines to reflect the offset in the main code block
    for (codeLine& line : chunk.lines) {
        line.end += bytecodeOffset;
        mainCodeBlock.lines.push_back(line);
    }
#ifdef COMPILER_DEBUG
    mainCodeBlock.disassemble(current->func->name.length() == 0 ? "script" : current->func->name, bytecodeOffset, constantsOffset);
#endif
    // Set the offsets in the function object
    func->bytecodeOffset = bytecodeOffset;
    func->constantsOffset = constantsOffset;

    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
    return func;
}

//a little helper for updating the lines emitted by the compiler(used for displaying runtime errors)
void Compiler::updateLine(Token token) {
    current->line = token.str.line;
}

// For every dependency that's imported without an alias, check if any of its exports match 'symbol', return -1 if not
int Compiler::checkSymbol(Token symbol) {
    string lexeme = symbol.getLexeme();
    for (Dependency dep : curUnit->deps) {
        auto& decls = dep.module->topDeclarations;
        if (dep.alias.type == TokenType::NONE) {
            int globalIndex = 0;
            for (int i = 0; i < units.size(); i++) {
                if (units[i] == dep.module) break;
                globalIndex += units[i]->topDeclarations.size();
            }
            for(int i = 0; i < decls.size(); i++){
                if(!decls[i]->getName().equals(symbol)) continue;
                return globalIndex + i;
            }
        }
    }
    return -1;
}

// Checks if 'symbol' is declared in the current file, if not passes it to checkSymbol
// -1 is returned only if no global variable 'symbol' exists, which is checked by checkSymbol
// -2 if variable exists but isn't defined
int Compiler::resolveGlobal(Token symbol, bool canAssign) {
    bool inThisFile = false;
    int index = curGlobalIndex;
    std::shared_ptr<AST::ASTDecl> ptr = nullptr;
    for (auto decl : curUnit->topDeclarations) {
        if (symbol.equals(decl->getName())) {
            // It's an error to read from a variable during its initialization
            if(!definedGlobals[index]) return -2;
            inThisFile = true;
            ptr = decl;
            break;
        }
        index++;
    }
    if (canAssign) {
        if (inThisFile){
            if(ptr->type == AST::ASTType::FUNC) error(symbol, "Cannot assign to a function.");
            else if(ptr->type == AST::ASTType::CLASS) error(symbol, "Cannot assign to a class.");

            return index;
        }
        error(symbol, "Cannot assign to a variable not declared in this module.");
    }
    else {
        if (inThisFile) return index;
        else {
            // Global variables defined in an imported file are guaranteed to be already defined
            return checkSymbol(symbol);
        }
    }
    // Never hit, checkSymbol returns -1 upon failure
    return -1;
}

// Checks if 'variable' exists in a module which was imported with the alias 'moduleAlias',
// If it exists return the index of the 'variable' in globals array
uint32_t Compiler::resolveModuleVariable(Token moduleAlias, Token variable) {
    //first find the module with the correct alias
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
    int index = 0;
    for (auto& i : units) {
        if (i != unit) {
            index += i->topDeclarations.size();
            continue;
        }
        // Checks every export of said module against 'variable'
        for (auto decl : unit->exports) {
            if (decl->getName().equals(variable)) return index;
            index++;
        }
    }

    error(variable, fmt::format("Module {} doesn't export this symbol.", depPtr->alias.getLexeme()));
    // Never hit
    return -1;
}

llvm::Value* Compiler::visitASTNode(AST::ASTNode *node) {
    node->accept(this);
    return returnValue;
}

void Compiler::retVal(llvm::Value* val){
    returnValue = builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}

void Compiler::createRuntimeErrCall(string fmtErr, std::vector<llvm::Value*> args, int exitCode){
    llvm::Constant* str = createConstStr(fmtErr);
    auto argNum = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), args.size());
    auto arrType = llvm::ArrayType::get(llvm::Type::getInt8PtrTy(*ctx), args.size());
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
    llvm::Constant* line = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), current->line);
    builder.CreateCall(curModule->getFunction("tyErrSingle"), {str, file, line, val});
}
void Compiler::createTyErr(string err, llvm::Value* lhs, llvm::Value* rhs){
    llvm::Constant* str = createConstStr(err);
    llvm::Constant* file = createConstStr(curUnit->file->path);
    llvm::Constant* line = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), current->line);
    builder.CreateCall(curModule->getFunction("tyErrDouble"), {str, file, line, lhs, rhs});
}

llvm::Value* Compiler::castToVal(llvm::Value* val){
    return builder.CreateBitCast(val, llvm::Type::getInt64Ty(*ctx));
}
#pragma endregion

// Only used when debugging _LONG versions of op codes
#undef SHORT_CONSTANT_LIMIT

#undef CHECK_SCOPE_FOR_LOOP
#undef CHECK_SCOPE_FOR_SWITCH
