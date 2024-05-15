#include "ASTToTypedAST.h"
#include "../../AST/ASTProbe.h"
#include "../../Includes/fmt/format.h"
#include "../../ErrorHandling/errorHandler.h"
#include "typeUnification.h"

using namespace passes;
using namespace typedASTParser;

ASTTransformer::ASTTransformer() {
    curUnitIndex = 0;
    hadError = false;
    current = nullptr;
    currentClass = nullptr;
    returnedExpr = nullptr;
    transformedAST = false;
    addBasicTypes();
}

std::pair<std::shared_ptr<typedAST::Function>, vector<File*>>
ASTTransformer::run(vector<ESLModule *> &_units, std::unordered_map<AST::FuncLiteral*, vector<closureConversion::FreeVariable>> _freevarMap){
    units = _units;
    freevarMap = _freevarMap;
    current = new CurrentChunkInfo(nullptr, FuncType::TYPE_SCRIPT, "func.main");
    for (ESLModule* unit : units) {
        curUnit = unit;
        sourceFiles.push_back(unit->file);
        for (int i = 0; i < unit->stmts.size(); i++) {
            // Doing this here so that even if an error is detected, we go on and possibly catch other(valid) errors
            try {
                auto stmts = evalASTStmt(unit->stmts[i]);
                current->func->block.stmts.insert(current->func->block.stmts.end(), stmts.begin(), stmts.end());
            }
            catch (TransformerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curUnitIndex++;
    }
    auto fn = current->func;
    delete current;
    for(auto unit : units) delete unit;

    transformedAST = true;
    return std::make_pair(fn, sourceFiles);
}

vector<types::tyPtr> ASTTransformer::getTypeEnv(){
    if(!transformedAST){
        std::cout<<"Tried to retrieve the type environment before running the transformer, exiting...\n";
        exit(64);
    }
    typeUnification::TypeUnificator unificator;
    return unificator.run(typeEnv);
}

#pragma region Visitor
static Token probeToken(AST::ASTNodePtr ptr){
    AST::ASTProbe p;
    ptr->accept(&p);
    return p.getProbedToken();
}

void ASTTransformer::visitAssignmentExpr(AST::AssignmentExpr* expr){
    auto rhs = evalASTExpr(expr->value);
    returnedExpr = storeToVar(expr->name, expr->op, rhs);
}
void ASTTransformer::visitSetExpr(AST::SetExpr* expr){
    // Needed because transforming a.b += 2 to a.b = a.b + 2 is illegal since eval-ing a could have side effects
    typedAST::SetType operationType;
    switch(expr->op.type){
        case TokenType::EQUAL: operationType = typedAST::SetType::SET; break;
        case TokenType::PLUS_EQUAL: operationType = typedAST::SetType::ADD_SET; break;
        case TokenType::MINUS_EQUAL: operationType = typedAST::SetType::SUB_SET; break;
        case TokenType::STAR_EQUAL: operationType = typedAST::SetType::MUL_SET; break;
        case TokenType::SLASH_EQUAL: operationType = typedAST::SetType::DIV_SET; break;
        case TokenType::PERCENTAGE_EQUAL: operationType = typedAST::SetType::REM_SET; break;
        case TokenType::BITWISE_AND_EQUAL: operationType = typedAST::SetType::AND_SET; break;
        case TokenType::BITWISE_OR_EQUAL: operationType = typedAST::SetType::OR_SET; break;
        case TokenType::BITWISE_XOR_EQUAL: operationType = typedAST::SetType::XOR_SET; break;
    }

    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        auto collection = evalASTExpr(expr->callee);
        auto field = evalASTExpr(expr->field);
        auto toStore = evalASTExpr(expr->value);

        auto dbg = AST::CollectionSetDebugInfo(expr->accessor, expr->op);
        returnedExpr = std::make_shared<typedAST::CollectionSet>(collection, field, toStore, operationType, dbg);
        // TODO: actually do type inference
        switch(operationType){
            case typedAST::SetType::SET: break;
            case typedAST::SetType::ADD_SET:{
                auto ty = createEmptyTy();
                addTypeConstraint(ty, std::make_shared<types::ComputeAddTysConstraint>(getBasicType(types::TypeFlag::ANY), toStore->exprType));
                returnedExpr->exprType = ty;
                break;
            }
            default: returnedExpr->exprType = getBasicType(types::TypeFlag::NUMBER);
        }
        return;
    }
    // Tries to resolve this.field = expr if we're currently inside a method
    auto resolved = tryResolveThis(expr, operationType);
    if(resolved) {
        returnedExpr = resolved;
    }else {
        // Guaranteed to be a literal
        Token field = probeToken(expr->field);
        auto dbg = AST::InstSetDebugInfo(field, expr->accessor, expr->op);
        auto instance = evalASTExpr(expr->callee);
        auto toStore = evalASTExpr(expr->value);

        resolved = std::make_shared<typedAST::InstSet>(instance, field.getLexeme(), toStore, operationType, dbg);
    }

    returnedExpr = resolved;

    switch(operationType){
        case typedAST::SetType::SET: break;
        case typedAST::SetType::ADD_SET:{
            auto fieldty = getInstFieldTy(resolved->instance->exprType, resolved->field);
            auto ty = createEmptyTy();
            addTypeConstraint(ty, std::make_shared<types::ComputeAddTysConstraint>(fieldty, resolved->toStore->exprType));
            returnedExpr->exprType = ty;
            break;
        }
        default: returnedExpr->exprType = getBasicType(types::TypeFlag::NUMBER);
    }
}
void ASTTransformer::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        auto collection = evalASTExpr(expr->callee);
        auto field = evalASTExpr(expr->field);
        auto dbg = AST::CollectionAccessDebugInfo(expr->accessor);
        auto ty = getBasicType(types::TypeFlag::ANY);
        returnedExpr = std::make_shared<typedAST::CollectionGet>(collection, field, ty, dbg);
        return;
    }

    // Tries to resolve this.field if we're currently inside a method
    auto resolved = tryResolveThis(expr);
    if(resolved) {
        returnedExpr = resolved;
        return;
    }
    // Guaranteed to be a literal
    Token field = probeToken(expr->field);
    auto inst = evalASTExpr(expr->callee);
    // If inst->exprType is known to be a single type(determined through type inference) InstGetFieldTyConstraint will return ty of the field
    auto fieldty = getInstFieldTy(inst->exprType, field.getLexeme());
    auto dbg = AST::InstGetDebugInfo(field, expr->accessor);
    returnedExpr = std::make_shared<typedAST::InstGet>(inst, field.getLexeme(), fieldty, dbg);
}

void ASTTransformer::visitConditionalExpr(AST::ConditionalExpr* expr) {
    auto cond = evalASTExpr(expr->condition);
    auto thenBranch = evalASTExpr(expr->mhs);
    auto elseBranch = evalASTExpr(expr->rhs);

    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(thenBranch->exprType));
    addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(elseBranch->exprType));

    auto dbg = AST::ConditionalExprDebugInfo(expr->questionmark, expr->colon);

    returnedExpr = std::make_shared<typedAST::ConditionalExpr>(cond, thenBranch, elseBranch, ty, dbg);
}
void ASTTransformer::visitRangeExpr(AST::RangeExpr* expr) {
    auto lhs = evalASTExpr(expr->start);
    auto rhs = evalASTExpr(expr->end);

    auto dbg = AST::RangeExprDebugInfo(expr->op);

    returnedExpr = std::make_shared<typedAST::RangeExpr>(lhs, rhs, expr->endInclusive, getBasicType(types::TypeFlag::RANGE), dbg);
}
void ASTTransformer::visitBinaryExpr(AST::BinaryExpr* expr) {
    auto lhs = evalASTExpr(expr->left);
    auto rhs = evalASTExpr(expr->right);

    auto dbg = AST::BinaryExprDebugInfo(expr->op);
    switch(expr->op.type){
        case TokenType::MINUS:
        case TokenType::PLUS:
        case TokenType::SLASH:
        case TokenType::STAR:
        case TokenType::PERCENTAGE:
        case TokenType::DIV:
        case TokenType::BITWISE_AND:
        case TokenType::BITWISE_OR:
        case TokenType::BITWISE_XOR:
        case TokenType::BITSHIFT_LEFT:
        case TokenType::BITSHIFT_RIGHT:{
            typedAST::ArithmeticOp op;
            switch(expr->op.type){
                case TokenType::MINUS: op = typedAST::ArithmeticOp::SUB; break;
                case TokenType::PLUS: op = typedAST::ArithmeticOp::ADD; break;
                case TokenType::SLASH: op = typedAST::ArithmeticOp::DIV; break;
                case TokenType::STAR: op = typedAST::ArithmeticOp::MUL; break;
                case TokenType::PERCENTAGE: op = typedAST::ArithmeticOp::MOD; break;
                case TokenType::DIV: op = typedAST::ArithmeticOp::IDIV; break;
                case TokenType::BITWISE_AND: op = typedAST::ArithmeticOp::AND; break;
                case TokenType::BITWISE_OR: op = typedAST::ArithmeticOp::OR; break;
                case TokenType::BITWISE_XOR: op = typedAST::ArithmeticOp::XOR; break;
                case TokenType::BITSHIFT_LEFT: op = typedAST::ArithmeticOp::BITSHIFT_L; break;
                case TokenType::BITSHIFT_RIGHT: op = typedAST::ArithmeticOp::BITSHIFT_R; break;
            }
            types::tyVarIdx exprType = getBasicType(types::TypeFlag::NUMBER);
            if(op == typedAST::ArithmeticOp::ADD){
                auto ty = createEmptyTy();
                addTypeConstraint(ty, std::make_shared<types::ComputeAddTysConstraint>(lhs->exprType, rhs->exprType));
                exprType = ty;
            }
            returnedExpr = std::make_shared<typedAST::ArithmeticExpr>(lhs, rhs, op, exprType, dbg);
            return;
        }
        case TokenType::BANG_EQUAL:
        case TokenType::EQUAL_EQUAL:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUAL:
        case TokenType::LESS:
        case TokenType::LESS_EQUAL:
        case TokenType::AND:
        case TokenType::OR:
        case TokenType::INSTANCEOF:{
            typedAST::ComparisonOp op;
            switch(expr->op.type){
                case TokenType::BANG_EQUAL: op = typedAST::ComparisonOp::NOT_EQUAL; break;
                case TokenType::EQUAL_EQUAL: op = typedAST::ComparisonOp::EQUAL; break;
                case TokenType::GREATER: op = typedAST::ComparisonOp::GREAT; break;
                case TokenType::GREATER_EQUAL: op = typedAST::ComparisonOp::GREATEQ; break;
                case TokenType::LESS: op = typedAST::ComparisonOp::LESS; break;
                case TokenType::LESS_EQUAL: op = typedAST::ComparisonOp::LESSEQ; break;
                case TokenType::AND: op = typedAST::ComparisonOp::AND; break;
                case TokenType::OR: op = typedAST::ComparisonOp::OR; break;
                // TODO: this is dumb, we need to use getclassfromexpr for rhs
                case TokenType::INSTANCEOF: op = typedAST::ComparisonOp::INSTANCEOF; break;
            }
            returnedExpr = std::make_shared<typedAST::ComparisonExpr>(lhs, rhs, op, getBasicType(types::TypeFlag::BOOL), dbg);
            return;
        }
    }
}
void ASTTransformer::visitUnaryExpr(AST::UnaryExpr* expr) {
    auto rhs = evalASTExpr(expr->right);
    typedAST::UnaryOp op;
    types::tyVarIdx ty = getBasicType(types::TypeFlag::NUMBER);

    switch(expr->op.type){
        case TokenType::TILDA: op = typedAST::UnaryOp::BIN_NEG; break;
        case TokenType::BANG: {
            op = typedAST::UnaryOp::NEG;
            ty = getBasicType(types::TypeFlag::BOOL);
            break;
        }
        case TokenType::MINUS: op = typedAST::UnaryOp::FNEG; break;
        case TokenType::INCREMENT: op = expr->isPrefix ? typedAST::UnaryOp::INC_PRE : typedAST::UnaryOp::INC_POST; break;
        case TokenType::DECREMENT: op = expr->isPrefix ? typedAST::UnaryOp::DEC_PRE : typedAST::UnaryOp::DEC_POST; break;
    }
    auto dbg = AST::UnaryExprDebugInfo(expr->op);
    returnedExpr = std::make_shared<typedAST::UnaryExpr>(rhs, op, ty, dbg);
}

void ASTTransformer::visitCallExpr(AST::CallExpr* expr) {
    auto callee = evalASTExpr(expr->callee);
    vector<typedAST::exprPtr> args;
    vector<types::tyVarIdx> argTypes;
    for(auto arg : expr->args){
        args.push_back(evalASTExpr(arg));
        argTypes.push_back(args.back()->exprType);
    }

    auto tryInvoke = tryConvertToInvoke(callee, args, expr->paren1, expr->paren2);
    if(tryInvoke) {
        returnedExpr = tryInvoke;
        return;
    }

    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::CallResTyConstraint>(callee->exprType));

    auto dbg = AST::CallExprDebugInfo(expr->paren1, expr->paren2);

    returnedExpr = std::make_shared<typedAST::CallExpr>(callee, args, ty, dbg);
}
void ASTTransformer::visitNewExpr(AST::NewExpr* expr) {
    auto klass = getClassInfoFromExpr(expr->call->callee);
    vector<typedAST::exprPtr> args;
    for(auto arg : expr->call->args){
        args.push_back(evalASTExpr(arg));
    }
    // getClassFromExpr will throw an error if it doesn't find a class,
    // so callee.valPtr is guaranteed to be a constrained to a types::ClassType
    auto instTy = addType(std::make_shared<types::InstanceType>(klass->classTy));
    Token className = probeToken(expr->call->callee);

    auto dbg = AST::NewExprDebugInfo(expr->keyword, className, expr->call->paren1, expr->call->paren2);
    auto varDbg = AST::VarReadDebugInfo(className);

    returnedExpr = std::make_shared<typedAST::NewExpr>(klass->mangledName, args, instTy, dbg);
}

void ASTTransformer::visitAsyncExpr(AST::AsyncExpr* expr) {
    auto callee = evalASTExpr(expr->callee);
    vector<typedAST::exprPtr> args;
    vector<types::tyVarIdx> argTypes;
    for(auto arg : expr->args){
        args.push_back(evalASTExpr(arg));
        argTypes.push_back(args.back()->exprType);
    }
    auto futTy = std::make_shared<types::FutureType>(callee->exprType);

    auto dbg = AST::AsyncExprDebugInfo(expr->keyword, expr->paren1, expr->paren2);

    returnedExpr = std::make_shared<typedAST::AsyncExpr>(callee, args, addType(futTy), dbg);
}
void ASTTransformer::visitAwaitExpr(AST::AwaitExpr* expr) {
    auto val = evalASTExpr(expr->expr);
    auto furAwaitTy = createEmptyTy();
    addTypeConstraint(furAwaitTy, std::make_shared<types::AwaitTyConstraint>(val->exprType));

    auto dbg = AST::AwaitExprDebugInfo(expr->keyword);

    returnedExpr = std::make_shared<typedAST::AwaitExpr>(val, furAwaitTy, dbg);
}

void ASTTransformer::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    vector<typedAST::exprPtr> fields;
    for(auto field : expr->members){
        fields.push_back(evalASTExpr(field));
    }
    auto arrTy = std::make_shared<types::ArrayType>(getBasicType(types::TypeFlag::ANY));

    auto dbg = AST::ArrayLiteralDebugInfo(expr->bracket1, expr->bracket2);

    returnedExpr = std::make_shared<typedAST::ArrayExpr>(fields, addType(arrTy), dbg);
}
void ASTTransformer::visitStructLiteralExpr(AST::StructLiteral* expr) {
    vector<std::pair<string, typedAST::exprPtr>> fields;
    vector<AST::StructDbgInfoField> fieldsDbg;

    for(auto field : expr->fields){
        //this gets rid of quotes, ""Hello world""->"Hello world"
        string temp = field.name.getLexeme();
        temp.erase(0, 1);
        temp.erase(temp.size() - 1, 1);
        fieldsDbg.emplace_back(field.name, field.colon);
        fields.emplace_back(temp, evalASTExpr(field.expr));
    }
    auto hashmapTy = std::make_shared<types::HashMapType>(getBasicType(types::TypeFlag::ANY));

    auto dbg = AST::StructLiteralDebugInfo(expr->brace1, fieldsDbg, expr->brace2);

    returnedExpr = std::make_shared<typedAST::HashmapExpr>(fields, addType(hashmapTy), dbg);
}
void ASTTransformer::visitLiteralExpr(AST::LiteralExpr* expr) {
    types::tyVarIdx ty;
    std::variant<double, bool, void*, string> variant;
    switch(expr->token.type){
        case TokenType::STRING:{
            string temp = expr->token.getLexeme();
            temp.erase(0, 1);
            temp.erase(temp.size() - 1, 1);
            variant = temp;
            ty = getBasicType(types::TypeFlag::STRING);
            break;
        }
        case TokenType::NUMBER:{
            variant = std::stod(expr->token.getLexeme());
            ty = getBasicType(types::TypeFlag::NUMBER);
            break;
        }
        case TokenType::TRUE:
        case TokenType::FALSE:{
            variant = expr->token.type == TokenType::TRUE;
            ty = getBasicType(types::TypeFlag::BOOL);
            break;
        }
        case TokenType::NIL:{
            variant = nullptr;
            ty = getBasicType(types::TypeFlag::NIL);
            break;
        }
        case TokenType::IDENTIFIER:{
            returnedExpr = readVar(expr->token);
            return;
        }
        case TokenType::THIS:{
            if (currentClass == nullptr) error(expr->token, "Can't use keyword 'this' outside of a class.");
            returnedExpr = readVar(expr->token);
            return;
        }
        default: break; // Unreachable
    }
    auto dbg = AST::LiteralDebugInfo(expr->token);
    returnedExpr = std::make_shared<typedAST::LiteralExpr>(variant, ty, dbg);
}

static string classContainsMethod(string publicField, std::shared_ptr<ClassChunkInfo> klass);

static int anonFuncCounter = 0;
void ASTTransformer::visitFuncLiteral(AST::FuncLiteral* expr) {
    auto freevars = freevarMap.at(expr);

    auto ty = createNewFunc("anonfunc." + std::to_string(anonFuncCounter++), expr->arity, FuncType::TYPE_FUNC, freevars.size() > 0);

    // Upvalues are gathered from the enclosing function
    for(int i = 0; i < freevars.size(); i++){
        auto& freevar = freevars[i];

        std::shared_ptr<typedAST::VarDecl> freevarPtr = nullptr;
        std::shared_ptr<typedAST::VarDecl> varToCapturePtr = nullptr;

        if(freevar.isLocal) varToCapturePtr = current->enclosing->locals[freevar.index].ptr;
        else varToCapturePtr = current->enclosing->freevars[freevar.index].ptr;

        // Kinda hacky, but it makes sure to propagate possible types of freevars when type inferring
        freevarPtr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::FREEVAR, varToCapturePtr->possibleTypes);
        // Sets upvalue in CurrentChunkInfo, used when resolving freevars in readVar and storeToVar
        current->freevars.emplace_back(freevar.name, freevarPtr);
        // Sets up the (enclosing var -> upvalue) pairs
        current->freevarPtrs.emplace_back(varToCapturePtr, freevarPtr);
    }

    // endScope is in endFuncDecl
    auto scopeEdge = beginScope();

    declareFuncArgs(expr->args);
    current->func->block = parseStmtsToBlock(expr->body->statements);
    current->func->block.stmts.insert(current->func->block.stmts.begin(), scopeEdge);
    auto fvars = current->freevarPtrs;
    auto func = endFuncDecl();
    vector<Token> params;
    for(auto arg : expr->args) params.push_back(arg.name);

    auto dbg = AST::FuncLiteralDebugInfo(expr->keyword, params);

    returnedExpr = std::make_shared<typedAST::CreateClosureExpr>(func, fvars, ty, dbg);
}
// Checks if 'symbol' exists in a module which was imported with the alias 'moduleAlias',
// If it exists return varPtr which holds the pointer to the global
void ASTTransformer::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    Token moduleAlias = expr->moduleName;
    Token symbol = expr->ident;
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
    if(globals.contains(fullSymbol)){
        auto ptr = globals.at(fullSymbol).valPtr;
        auto dbg = AST::VarReadDebugInfo(expr->ident);
        returnedExpr = std::make_shared<typedAST::VarRead>(ptr, dbg);
        return;
    }else if(globalClasses.contains(fullSymbol)){
        error(symbol, fmt::format("Classes aren't first class values.", depPtr->pathString.getLexeme(), depPtr->alias.getLexeme()));
    }

    error(symbol, fmt::format("Module '{}' with alias '{}' doesn't export this symbol.", depPtr->pathString.getLexeme(), depPtr->alias.getLexeme()));
}
// This shouldn't ever be visited as every macro should be expanded after AST is generatefd
void ASTTransformer::visitMacroExpr(AST::MacroExpr* expr) {
    error("Non-expanded macro encountered during compilation.");
}

void ASTTransformer::visitVarDecl(AST::VarDecl* decl) {
    updateLine(decl->var.name);
    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + decl->var.name.getLexeme();

    varPtr var;
    if(decl->var.type == AST::ASTVarType::GLOBAL){
        var = declareGlobalVar(fullGlobalSymbol, AST::ASTDeclType::VAR, -1);
    }else var = declareLocalVar(decl->var, -1);

    // Compile the right side of the declaration, if there is no right side, the variable is initialized as nil

    auto dbg = AST::LiteralDebugInfo(Token()); // Never used

    typedAST::exprPtr initializer = std::make_shared<typedAST::LiteralExpr>(nullptr, getBasicType(types::TypeFlag::NIL), dbg);
    if (decl->value != nullptr) {
        initializer = evalASTExpr(decl->value);
    }

    if(decl->var.type == AST::ASTVarType::GLOBAL){
        defineGlobalVar(fullGlobalSymbol, AST::VarDeclDebugInfo(decl->keyword, decl->getName()));
    }else defineLocalVar(AST::VarDeclDebugInfo(decl->keyword, decl->getName()));

    auto toStore = storeToVar(decl->var.name, decl->op, initializer);

    nodesToReturn = {var, toStore};
}

void ASTTransformer::visitFuncDecl(AST::FuncDecl* decl) {
    updateLine(decl->getName());

    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + decl->getName().getLexeme();
    auto ty = createNewFunc("func." + fullGlobalSymbol, decl->args.size(), FuncType::TYPE_FUNC, false);

    varPtr name = declareGlobalVar(fullGlobalSymbol, AST::ASTDeclType::FUNCTION, ty);
    // Defining the function here to allow for recursion
    defineGlobalVar(fullGlobalSymbol, AST::VarDeclDebugInfo(decl->keyword, decl->getName()));

    // No need for a endScope, since returning from the function discards the entire callstack
    auto scopeEdge = beginScope();
    declareFuncArgs(decl->args);
    current->func->block = parseStmtsToBlock(decl->body->statements);
    // Insert scope edge to the start of the function
    current->func->block.stmts.insert(current->func->block.stmts.begin(), scopeEdge);
    auto func = endFuncDecl();

    vector<Token> params;
    for(auto arg : decl->args) params.push_back(arg.name);
    auto dbg = AST::FuncDeclDebugInfo(decl->keyword, decl->name, params);


    nodesToReturn = {name, std::make_shared<typedAST::FuncDecl>(func, dbg, name->uuid)};
}
void ASTTransformer::visitClassDecl(AST::ClassDecl* decl) {
    updateLine(decl->getName());

    std::shared_ptr<types::ClassType> classTy = std::make_shared<types::ClassType>();
    auto classTyIdx = addType(classTy);

    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + decl->getName().getLexeme();

    currentClass = std::make_shared<ClassChunkInfo>(fullGlobalSymbol, classTy, classTyIdx);
    globalClasses.insert_or_assign(fullGlobalSymbol, currentClass);

    if (decl->inheritedClass) {
        //decl->inheritedClass is always either a LiteralExpr with an identifier token or a ModuleAccessExpr

        // If a class wants to inherit from a class in another file of the same name, the import has to use an alias, otherwise we get
        // undefined behavior (eg. class a : a)
        try { // try block because getClassInfoFromExpr can throw
            auto superclass = getClassInfoFromExpr(decl->inheritedClass);
            // Make the class type aware of the inheritance(copies methods and fields)
            currentClass->inherit(superclass);
            currentClass->classTy->inherit(superclass->classTy);
        }catch(TransformerException& e){

        }
    }
    // Before compiling any code make the class type aware of its own fields and methods
    // Think of this as a sort of forward declaration
    for(auto field : decl->fields){
        // First check if this is a duplicate symbol
        detectDuplicateSymbol(field.field, false ,false);

        string str = field.field.getLexeme();
        str = (field.isPublic ? str : ("priv." + str));
        // Can't override symbols
        int idx = currentClass->fields.size();

        // New field is inserted at the end of the fields array
        currentClass->fields.insert_or_assign(str, idx);
        classTy->fields.insert_or_assign(str, std::make_pair(getBasicType(types::TypeFlag::ANY), idx));
    }

    vector<std::shared_ptr<types::FunctionType>> methodTys;
    for(auto method : decl->methods){
        // First check if this is a duplicate symbol
        detectDuplicateSymbol(method.method->getName(), true ,method.overrides);

        string str = method.method->getName().getLexeme();
        str = (method.isPublic ? str : ("priv." + str));

        // Creates new type for each method(if it's an override then just override the field in classTy)
        auto methodTy = std::make_shared<types::FunctionType>(method.method->args.size(),
                                                              createEmptyTy(), false);
        auto tyIdx = addType(methodTy);
        methodTys.push_back(methodTy);


        int fieldIndex = currentClass->classTy->methods.size();
        if(currentClass->methods.contains(str)){
            fieldIndex = currentClass->methods[str].second;
        }

        classTy->methods.insert_or_assign(str, std::make_pair(tyIdx, fieldIndex));
    }

    // Process the methods after forward declaring the fields
    processMethods(decl->getName().getLexeme(), decl->methods, methodTys);

    // Only pass pointer to where the class is stored if this class inherits
    string paren = "";
    if(currentClass->parent){
        paren = currentClass->parent->mangledName;
    }

    // Debug stuff
    auto dbg = AST::ClassDeclDebugInfo(decl->keyword, decl->name);
    std::unordered_map<string, AST::MethodDebugInfo> methodsDbg;
    if(decl->inheritedClass) dbg.colon = decl->colon;
    for(auto m : decl->methods) {
        string str = m.method->getName().getLexeme();
        str = (m.isPublic ? str : ("priv." + str));
        vector<Token> params;
        for(auto arg : m.method->args) params.push_back(arg.name);
        methodsDbg.insert_or_assign(str, AST::MethodDebugInfo(m.override, m.method->keyword, m.method->name, params));
    }

    auto klass = std::make_shared<typedAST::ClassDecl>(currentClass->classTy,
                                                       dbg, fullGlobalSymbol, paren, currentClass->fields, currentClass->methods);
    nodesToReturn = {klass};
    currentClass = nullptr;
}

void ASTTransformer::visitExprStmt(AST::ExprStmt* stmt) {
    nodesToReturn= {evalASTExpr(stmt->expr)};
}
void ASTTransformer::visitBlockStmt(AST::BlockStmt* stmt) {
    vector<typedAST::nodePtr> nodes = {beginScope()};
    for(auto temp : stmt->statements){
        vector<typedAST::nodePtr> nodesToInsert;
        try{
            nodesToInsert = evalASTStmt(temp);
        }catch(TransformerException e){
            // To stop unwinding
        }
        nodes.insert(nodes.end(), nodesToInsert.begin(), nodesToInsert.end());
    }
    nodes.push_back(endScope());
    nodesToReturn.insert(nodesToReturn.end(), nodes.begin(), nodes.end());
}

void ASTTransformer::visitIfStmt(AST::IfStmt* stmt) {
    auto cond = evalASTExpr(stmt->condition);
    typedAST::Block thenBlock = parseStmtToBlock(stmt->thenBranch);
    typedAST::Block elseBlock;
    if(stmt->elseBranch){
        elseBlock = parseStmtToBlock(stmt->elseBranch);
    }

    auto dbg = AST::IfStmtDebugInfo(stmt->keyword);

    nodesToReturn = {std::make_shared<typedAST::IfStmt>(cond, thenBlock, elseBlock, dbg)};
}
void ASTTransformer::visitWhileStmt(AST::WhileStmt* stmt) {
    auto cond = evalASTExpr(stmt->condition);
    typedAST::Block loopBody = parseStmtToBlock(stmt->body);

    auto dbg = AST::WhileStmtDebugInfo(stmt->keyword);

    nodesToReturn = {std::make_shared<typedAST::WhileStmt>(cond, loopBody, dbg)};
}
void ASTTransformer::visitForStmt(AST::ForStmt* stmt) {
    // Convert for to a while loop with "init" directly above it in a block
    auto scopeEdge1 = beginScope();
    vector<typedAST::nodePtr> init;
    typedAST::exprPtr cond = nullptr;
    typedAST::exprPtr inc = nullptr;
    // Order of eval is important for types
    // Init can be a single statement(expr statement) or a var decl + var store
    if(stmt->init) init = evalASTStmt(stmt->init);
    if(stmt->condition) cond = evalASTExpr(stmt->condition);

    typedAST::Block loopBody = parseStmtToBlock(stmt->body);
    if(stmt->increment) inc = evalASTExpr(stmt->increment);
    auto scopeEdge2 = endScope();
    // Init isn't treated as part of the while loop, but as statement(s) by itself(exprStmt or var decl + var store)
    nodesToReturn.insert(nodesToReturn.end(), scopeEdge1);
    nodesToReturn.insert(nodesToReturn.end(), init.begin(), init.end());

    auto dbg = AST::WhileStmtDebugInfo(stmt->keyword);

    // AfterLoopExpr is special field that won't be part of the loop body, but in a basic block by itself
    nodesToReturn.push_back(std::make_shared<typedAST::WhileStmt>(cond, loopBody, dbg, inc));
    nodesToReturn.push_back(scopeEdge2);
}
void ASTTransformer::visitBreakStmt(AST::BreakStmt* stmt) {
    auto dbg = AST::UncondJmpDebugInfo(stmt->keyword);
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::BREAK, dbg)};
}
void ASTTransformer::visitContinueStmt(AST::ContinueStmt* stmt) {
    auto dbg = AST::UncondJmpDebugInfo(stmt->keyword);
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::CONTINUE, dbg)};
}

vector<std::variant<double, bool, void*, string>> ASTTransformer::getCaseConstants(vector<Token> constants){
    vector<std::variant<double, bool, void*, string>> converted;
    for (auto literal: constants) {
        if(literal.type == TokenType::STRING){
            string temp = literal.getLexeme();
            temp.erase(0, 1);
            temp.erase(temp.size() - 1, 1);
            converted.emplace_back(temp);
        }else if(literal.type == TokenType::NUMBER) {
            double num = std::stod(literal.getLexeme());
            if(trunc(num) != num) {
                error(literal, "Case literal can't be a floating point.");
            }else if(num > (1ull<<32ull)){
                error(literal, "Integer constants in switch must be contained in 32bit integers.");
            }
            converted.emplace_back(num);
        }
        else if(literal.type == TokenType::NIL) {
            converted.emplace_back(nullptr);
        }
        else {
            converted.emplace_back(literal.type == TokenType::TRUE);
        }
    }
    return converted;
}

void ASTTransformer::visitSwitchStmt(AST::SwitchStmt* stmt) {
    auto cond = evalASTExpr(stmt->expr);
    vector<std::pair<std::variant<double, bool, void*, string>, int>> constants;
    // Used to check if switch stmt contains duplicate constants
    vector<typedAST::Block> caseBlocks;
    int defaultBlockIdx = -1;
    int i = 0;
    vector<Token> containedTokens;
    auto contains = [&](Token t){
        for(Token& tok : containedTokens){
            if(tok.equals(t)) return true;
        }
        return false;
    };
    bool containsString = false;
    for(auto _case : stmt->cases){
        if(_case->caseType.type == TokenType::DEFAULT){
            defaultBlockIdx = i;
            caseBlocks.push_back(parseStmtsToBlock(_case->stmts));
            i++;
            continue;
        }
        // For error reporting
        for(Token& tok : _case->constants){
            if(contains(tok)){
                error(tok, "Switch cannot contains 2 same constants");
            }
            containedTokens.emplace_back(tok);
            if(tok.type == TokenType::STRING) containsString = true;
        }
        auto convertedLiterals = getCaseConstants(_case->constants);
        for(auto literal : convertedLiterals){
            constants.emplace_back(literal, i);
        }
        caseBlocks.push_back(parseStmtsToBlock(_case->stmts));
        i++;
    }
    vector<Token> cases;
    for(auto _case : stmt->cases){
        cases.push_back(_case->caseType);
    }
    auto dbg = AST::SwitchStmtDebugInfo(stmt->keyword, cases);

    nodesToReturn = {std::make_shared<typedAST::SwitchStmt>(cond, constants, caseBlocks, defaultBlockIdx, containsString, dbg)};
}
void ASTTransformer::visitCaseStmt(AST::CaseStmt* _case) {
    //Nothing, everything is handled in visitSwitchStmt
}
void ASTTransformer::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    auto dbg = AST::UncondJmpDebugInfo(stmt->keyword);
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::ADVANCE, dbg)};
}

void ASTTransformer::visitReturnStmt(AST::ReturnStmt* stmt) {
    if (current->type == FuncType::TYPE_SCRIPT) {
        error(stmt->keyword, "Can't return from top-level code.");
    }
    else if (current->type == FuncType::TYPE_CONSTRUCTOR) {
        error(stmt->keyword, "Can't return a value from a constructor.");
    }
    typedAST::exprPtr expr = nullptr;
    if(stmt->expr) expr = evalASTExpr(stmt->expr);
    else expr = std::make_shared<typedAST::LiteralExpr>(nullptr, getBasicType(types::TypeFlag::NIL), AST::LiteralDebugInfo(Token()));
    addTypeConstraint(current->func->fnTy->retType, std::make_shared<types::AddTyConstraint>(expr->exprType));

    auto dbg = AST::ReturnStmtDebugInfo(stmt->keyword);

    nodesToReturn = {std::make_shared<typedAST::ReturnStmt>(expr, dbg)};
}
#pragma endregion

#pragma region Helpers
// Variables
// Checks all imports to see if the symbol 'token' is imported
varPtr ASTTransformer::checkSymbol(const Token symbol){
    for (Dependency& dep : curUnit->deps) {
        string fullSymbol = dep.module->file->name + std::to_string(dep.module->id) + "." + symbol.getLexeme();
        if (dep.alias.type == TokenType::NONE && globals.contains(fullSymbol)) {
            Globalvar& gvar = globals.at(fullSymbol);
            if(!gvar.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            return gvar.valPtr;
        }else if(dep.alias.type == TokenType::NONE && globalClasses.contains(fullSymbol)){
            error(symbol, "Classes aren't first class values.");
        }
    }
    return nullptr;
}
// Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
varPtr ASTTransformer::resolveGlobal(const Token symbol, const bool canAssign){
    string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." +symbol.getLexeme();
    auto it = globals.find(fullSymbol);
    if (canAssign) {
        // Global is in this module
        if (it != globals.end()){
            Globalvar& var = it->second;
            if(var.valPtr->varType == typedAST::VarType::GLOBAL_FUNC) error(symbol, "Cannot assign to a function.");

            if(!var.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            return var.valPtr;
        }
        if(globalClasses.contains(fullSymbol)){
            error(symbol, "Classes aren't first class values.");
        }
        error(symbol, "Cannot assign to a variable not declared in this module.");
    }
    else {
        if (it != globals.end()) {
            if(!it->second.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            return it->second.valPtr;
        }
        else if(globalClasses.contains(fullSymbol)){
            error(symbol, "Classes aren't first class values.");
        }
        else {
            // Global variables defined in an imported file are guaranteed to be already defined
            return checkSymbol(symbol);
        }
    }
    // Never hit, checkSymbol returns -1 upon failure
    return nullptr;
}

varPtr ASTTransformer::declareGlobalVar(const string& name, const AST::ASTDeclType type, const types::tyVarIdx defaultTy){
    typedAST::VarType varty;
    switch(type){
        case AST::ASTDeclType::VAR: varty = typedAST::VarType::GLOBAL; break;
        case AST::ASTDeclType::FUNCTION: varty = typedAST::VarType::GLOBAL_FUNC; break;
        case AST::ASTDeclType::CLASS: break; // Unreachable
    }
    varPtr var;
    if(defaultTy != -1) {
        var = std::make_shared<typedAST::VarDecl>(varty, defaultTy);
    }else{
        var = std::make_shared<typedAST::VarDecl>(varty, createEmptyTy());
    }
    globals.insert_or_assign(name, Globalvar(var));
    return var;
}
void ASTTransformer::defineGlobalVar(const string& name, AST::VarDeclDebugInfo dbgInfo){
    Globalvar& gvar = globals.at(name);
    gvar.isDefined = true;
    gvar.valPtr->dbgInfo = dbgInfo;
}

// Makes sure the compiler is aware that a stack slot is occupied by this local variable
varPtr ASTTransformer::declareLocalVar(const AST::ASTVar& var, const types::tyVarIdx defaultTy) {
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
    return addLocal(var, defaultTy);
}
void ASTTransformer::defineLocalVar(AST::VarDeclDebugInfo dbgInfo){
    current->locals.back().depth = current->scopeDepth;
    current->locals.back().ptr->dbgInfo = dbgInfo;
}
varPtr ASTTransformer::addLocal(const AST::ASTVar& var, const types::tyVarIdx defaultTy){
    updateLine(var.name);
    current->locals.emplace_back(var.name.getLexeme(), -1, var.type == AST::ASTVarType::FREEVAR);
    Local& local = current->locals.back();
    auto varTy = defaultTy == -1 ? createEmptyTy() : defaultTy;
    if(!local.isUpval) {
        local.ptr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::LOCAL, varTy);
    }else{
        local.ptr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::FREEVAR, varTy);
    }
    return local.ptr;
}

int ASTTransformer::resolveLocal(const Token name){
    // Checks to see if there is a local variable with a provided name, if there is return the index of the stack slot of the var
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
int ASTTransformer::resolveUpvalue(const Token name){
    string upvalName = name.getLexeme();
    for(int i = 0; i < current->freevars.size(); i++){
        if(upvalName == current->freevars[i].name) return i;
    }
    return -1;
}

// Order of checking:
// locals->freevars->implicit object fields->globals->natives
typedAST::exprPtr ASTTransformer::readVar(const Token name){
    updateLine(name);
    auto dbg = AST::VarReadDebugInfo(name);
    int argIndex = resolveLocal(name);
    if (argIndex != -1) {
        varPtr valPtr = current->locals[argIndex].ptr;
        return std::make_shared<typedAST::VarRead>(valPtr, dbg);
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        varPtr upvalPtr = current->freevars[argIndex].ptr;
        return std::make_shared<typedAST::VarRead>(upvalPtr, dbg);
    }
    std::shared_ptr<typedAST::InstGet> implicitClassField = resolveClassFieldRead(name);
    if(implicitClassField){
        if(current->type == FuncType::TYPE_FUNC){
            error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
        }
        return implicitClassField;
    }
    varPtr globalPtr = resolveGlobal(name, false);
    if(globalPtr){
        return std::make_shared<typedAST::VarRead>(globalPtr, dbg);
    }
    string nativeName = name.getLexeme();
    auto it = nativesTypes.find(nativeName);
    if(it != nativesTypes.end()) return std::make_shared<typedAST::VarReadNative>(nativeName, it->second, dbg);

    error(name, fmt::format("'{}' doesn't match any declared variable name or native function name.", nativeName));
    return nullptr;
}

typedAST::exprPtr ASTTransformer::storeToVar(const Token name, const Token op, typedAST::exprPtr toStore){
    updateLine(name);
    int argIndex = resolveLocal(name);
    auto dbg = AST::VarStoreDebugInfo(name, op);

    if (argIndex != -1) {
        varPtr valPtr = current->locals[argIndex].ptr;
        addTypeConstraint(valPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(valPtr, toStore, dbg);
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        varPtr upvalPtr = current->freevars[argIndex].ptr;
        addTypeConstraint(upvalPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(upvalPtr, toStore, dbg);
    }

    std::shared_ptr<typedAST::InstSet> implicitClassField = resolveClassFieldStore(name, toStore, Token());
    if(implicitClassField){
        if(current->type == FuncType::TYPE_FUNC){
            error(name, fmt::format("Cannot access object fields within a closure without 'this', use this.{}", name.getLexeme()));
        }
        return implicitClassField;
    }
    varPtr globalPtr = resolveGlobal(name, true);
    if(globalPtr){
        addTypeConstraint(globalPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(globalPtr, toStore, dbg);
    }
    auto it = nativesTypes.find(name.getLexeme());
    if(it != nativesTypes.end()) {
        error(name, fmt::format("'{}' is a native function, cannot assign to native functions.", name.getLexeme()));
        return nullptr;
    }

    error(name, fmt::format("'{}' undefined symbol.", name.getLexeme()));
    return nullptr;
}

std::shared_ptr<typedAST::ScopeEdge> ASTTransformer::beginScope(){
    current->scopeDepth++;
    return std::make_shared<typedAST::ScopeEdge>(typedAST::ScopeEdgeType::START,std::unordered_set<uInt64>());
}
// Pop every variable that was declared in this scope
std::shared_ptr<typedAST::ScopeEdge> ASTTransformer::endScope(){
    // First lower the scope, the check for every var that is deeper than the parserCurrent scope
    current->scopeDepth--;
    // Store which variables to pop(store the VarDecl ptr)
    std::unordered_set<uInt64> toPop;
    // Pop from the stack
    while (current->locals.size() > 0 && current->locals.back().depth > current->scopeDepth) {
        toPop.insert(current->locals.back().ptr->uuid);
        current->locals.pop_back();
    }
    return std::make_shared<typedAST::ScopeEdge>(typedAST::ScopeEdgeType::END, toPop);
}

// Functions
std::shared_ptr<typedAST::Function> ASTTransformer::endFuncDecl(){
    // Get the function we've just transformed, delete its compiler info, and replace it with the enclosing functions compiler info
    // If function doesn't contain an explicit return stmt, add it to the end of the function
    if(!current->func->block.terminates){
        std::shared_ptr<typedAST::ReturnStmt> ret = nullptr;
        // Constructors return must return the instance
        if(current->type == FuncType::TYPE_CONSTRUCTOR){
            auto _this = readVar(syntheticToken("this"));
            ret = std::make_shared<typedAST::ReturnStmt>(_this, AST::ReturnStmtDebugInfo(Token()));
        }
        else {
            ret = std::make_shared<typedAST::ReturnStmt>(
                    std::make_shared<typedAST::LiteralExpr>(
                            nullptr, getBasicType(types::TypeFlag::NIL),AST::LiteralDebugInfo(Token())),
                    AST::ReturnStmtDebugInfo(Token()));
        }
        addTypeConstraint(current->func->fnTy->retType, std::make_shared<types::AddTyConstraint>(ret->expr->exprType));
        current->func->block.stmts.push_back(ret);
        current->func->block.terminates = true;
    }
    // Need to end scope AFTER emitting return because "this" is a local var(arg)
    auto scopeEdge = endScope();
    current->func->block.stmts.push_back(scopeEdge);

    auto func = current->func;
    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;

    // Dead code elimination
    for(int i = func->block.stmts.size() - 1; i >= 0; i--){
        auto stmt = func->block.stmts[i];
        if(stmt->type == typedAST::NodeType::RETURN || stmt->type == typedAST::NodeType::UNCOND_JMP){
            func->block.stmts.resize(i + 1);
            break;
        }
    }
    return func;
}
void ASTTransformer::declareFuncArgs(vector<AST::ASTVar>& args){
    for (AST::ASTVar& var : args) {
        types::tyVarIdx ty;
        // 'this' is of a known type so annotate it to enable optimizations,
        // currentClass is not null because 'this' can only appear as an argument in methods
        if(var.name.getLexeme() == "this"){
            ty = addType(std::make_shared<types::InstanceType>(currentClass->classTy));
        }else{
            // Set the type of the other arguments
            ty = getBasicType(types::TypeFlag::ANY);
        }
        current->func->args.push_back(declareLocalVar(var, ty));
        defineLocalVar(AST::VarDeclDebugInfo(Token(), var.name));
    }
}
types::tyVarIdx ASTTransformer::createNewFunc(const string name, const int arity, const FuncType fnKind, const bool isClosure){
    std::shared_ptr<types::FunctionType> fnTy = std::make_shared<types::FunctionType>(arity, createEmptyTy(), isClosure);
    auto idx = addType(fnTy);
    current = new CurrentChunkInfo(current, fnKind, name);
    current->func->fnTy = fnTy;
    return idx;
}

// Classes and methods
// Class name is for recognizing constructor
typedAST::ClassMethod ASTTransformer::createMethod(AST::FuncDecl* _method, const Token overrides, const string className,
                                                   std::shared_ptr<types::FunctionType> fnTy){
    updateLine(_method->getName());

    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + className;
    FuncType type = FuncType::TYPE_METHOD;
    // Constructors are treated separately, but are still methods
    if (_method->getName().getLexeme() == className) type = FuncType::TYPE_CONSTRUCTOR;
    current = new CurrentChunkInfo(current, type,  fullGlobalSymbol + ".method." + _method->getName().getLexeme());
    current->func->fnTy = fnTy;
    // No need for a endScope, since returning from the function discards the entire callstack
    auto scopeEdge = beginScope();

    declareFuncArgs(_method->args);
    current->func->block = parseStmtsToBlock(_method->body->statements);
    // Insert the scope edge to the start of the functions
    current->func->block.stmts.insert(current->func->block.stmts.begin(), scopeEdge);
    auto func = endFuncDecl();
    vector<Token> params;
    for(auto arg : _method->args) params.push_back(arg.name);
    return typedAST::ClassMethod(func, AST::MethodDebugInfo(overrides, _method->keyword, _method->name, params));
}
std::shared_ptr<typedAST::InvokeExpr> ASTTransformer::tryConvertToInvoke(typedAST::exprPtr callee, vector<typedAST::exprPtr>& args,
                                                                         const Token paren1, const Token paren2){
    vector<types::tyVarIdx> argTys;
    for(auto arg : args){
        argTys.push_back(arg->exprType);
    }
    if(callee->type == typedAST::NodeType::INST_GET){
        std::shared_ptr<typedAST::InstGet> casted = std::reinterpret_pointer_cast<typedAST::InstGet>(callee);
        auto invokeTy = createEmptyTy();
        addTypeConstraint(invokeTy, std::make_shared<types::CallResTyConstraint>(casted->exprType));
        auto dbg = AST::InvokeExprDebugInfo(casted->dbgInfo.accessor, casted->dbgInfo.field, paren1, paren2);
        return std::make_shared<typedAST::InvokeExpr>(casted->instance, casted->field, args, invokeTy, dbg);

    }
    return nullptr;
}
void ASTTransformer::processMethods(const string className, vector<AST::ClassMethod>& methods,
                                    vector<std::shared_ptr<types::FunctionType>>& methodTys){
    int i = 0;
    for(auto method : methods){
        string str = method.method->getName().getLexeme();
        str = (method.isPublic ? str : ("priv." + str));
        // Get the index of the linearized field
        int fieldIndex = currentClass->classTy->methods.size();
        if(currentClass->methods.contains(str)){
            fieldIndex = currentClass->methods[str].second;
        }
        auto transformedMethod = createMethod(method.method.get(), method.override, className, methodTys[i]);

        // If this is an override of a method, it gets the index that the overriden function had
        currentClass->methods.insert_or_assign(str, std::make_pair(transformedMethod, fieldIndex));
        i++;
    }
}

void ASTTransformer::detectDuplicateSymbol(const Token publicName, const bool isMethod, const bool methodOverrides){
    string pubName = publicName.getLexeme();
    string privName = "priv." + pubName;
    // Immediately catch the thrown error since this isn't an error that would require unwinding
    try{
        if(isMethod && !methodOverrides && (currentClass->methods.contains(pubName) || currentClass->methods.contains(privName))){
            error(publicName, "Duplicate symbol found. Methods that override need to be explicitly marked with 'override'.");
        }

        if(isMethod && methodOverrides && !(currentClass->methods.contains(pubName) || currentClass->methods.contains(privName))){
            // If this method has been marked as override but there isn't a parent method to override, throw an error
            error(publicName, "Method marked as 'override' but there is no parent method with matching name.");
        }

        if(currentClass->fields.contains(privName) || currentClass->fields.contains(privName)){
            error(publicName, "Duplicate symbol found. Fields are inherited from parents.");
        }
    }catch(TransformerException& e){

    }
}
// Resolve implicit object field access
// Turns a hash map lookup into an array linear search, but still faster than allocating memory using ObjString::createStr
// First bool in pair is if the search was succesful, second is if the field found was public or private
static string classContainsField(string publicField, std::shared_ptr<ClassChunkInfo> klass){
    string privateField = "priv." + publicField;
    for(auto it : klass->fields){
        if(publicField == it.first) return publicField;
        else if(privateField == it.first) return privateField;
    }
    return "";
}
static string classContainsMethod(string publicField, std::shared_ptr<ClassChunkInfo> klass){
    string privateField = "priv." + publicField;
    for(auto it : klass->methods){
        if(publicField == it.first) return publicField;
        else if(privateField == it.first) return privateField;
    }
    return "";
}

std::shared_ptr<typedAST::InstGet> ASTTransformer::resolveClassFieldRead(Token name){
    if(!currentClass) return nullptr;
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass);
    // Accessor doesn't exist in the source code so it doesn't have any debug info
    auto dbg = AST::InstGetDebugInfo(name, Token());
    // If this class containes fieldName, transform fieldname -> this.fieldName
    if(!res.empty()){
        auto readThis = readVar(syntheticToken("this"));
        auto fieldty = getInstFieldTy(readThis->exprType, res);

        return std::make_shared<typedAST::InstGet>(readThis, res, fieldty, dbg);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(!res.empty()){
        auto readThis = readVar(syntheticToken("this"));
        auto fieldty = getInstFieldTy(readThis->exprType, res);

        return std::make_shared<typedAST::InstGet>(readThis, res, fieldty, dbg);
    }
    return nullptr;
}
std::shared_ptr<typedAST::InstSet> ASTTransformer::resolveClassFieldStore(const Token name, typedAST::exprPtr toStore, const Token op) {
    if(!currentClass) return nullptr;
    string fieldName = name.getLexeme();
    // Accessor doesn't exist in the source code so it doesn't have any debug info
    auto dbg = AST::InstSetDebugInfo(name, Token(), op);
    // If this class containes fieldName, transform fieldname -> this.fieldName
    auto res = classContainsField(fieldName, currentClass);
    if(!res.empty()){
        // Can safely use normal SET since evaling "this" twice has no side effects
        //TODO does this even do what it needs to?
        auto operationType = typedAST::SetType::SET;
        return std::make_shared<typedAST::InstSet>(readVar(syntheticToken("this")), res, toStore, operationType, dbg);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(!res.empty()){
        error(name, "Tried assigning to a method, which is forbidden.");
    }
    return nullptr;
}

Globalvar& ASTTransformer::getClassFromExpr(AST::ASTNodePtr expr){
    return globals.at(getClassInfoFromExpr(expr)->mangledName);
}
std::shared_ptr<ClassChunkInfo> ASTTransformer::getClassInfoFromExpr(AST::ASTNodePtr expr){
    if (expr->type == AST::ASTType::LITERAL) {
        Token symbol = probeToken(expr);
        // First check this module
        string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + symbol.getLexeme();
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
        error(moduleExpr->ident, fmt::format("Symbol doesn't exist in module '{}'.", moduleExpr->moduleName.getLexeme()));
    }
    // Never hit
    return nullptr;
}
static bool isLiteralThis(AST::ASTNodePtr ptr){
    if(ptr->type != AST::ASTType::LITERAL) return false;
    return probeToken(ptr).type == TokenType::THIS;
}

// Resolve public/private fields when this.field in encountered in code
static string demangleName(string mangled){
    return mangled.substr(mangled.rfind("."), mangled.size() - mangled.rfind("."));
}
std::shared_ptr<typedAST::InstGet> ASTTransformer::tryResolveThis(AST::FieldAccessExpr* expr){
    if(!isLiteralThis(expr->callee)) return nullptr;
    Token _this = probeToken(expr->callee);
    Token name = probeToken(expr->field);
    string fieldName = name.getLexeme();
    auto dbg = AST::InstGetDebugInfo(expr->accessor, name);
    auto res = classContainsField(fieldName, currentClass);
    if(!res.empty()){
        auto readThis = readVar(_this);
        auto fieldty = getInstFieldTy(readThis->exprType, res);

        return std::make_shared<typedAST::InstGet>(readThis, res, fieldty, dbg);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(!res.empty()){
        auto readThis = readVar(_this);
        auto fieldty = getInstFieldTy(readThis->exprType, res);

        return std::make_shared<typedAST::InstGet>(readThis, res, fieldty, dbg);
    }
    error(name, fmt::format("Class '{}' doesn't contain this symbol", demangleName(currentClass->mangledName)));
    // Never hit
    return nullptr;
}
std::shared_ptr<typedAST::InstSet> ASTTransformer::tryResolveThis(AST::SetExpr* expr, typedAST::SetType operationTy){
    if(!isLiteralThis(expr->callee)) return nullptr;

    Token _this = probeToken(expr->callee);
    Token name = probeToken(expr->field);
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass);
    if(!res.empty()){
        auto toStore = evalASTExpr(expr->value);
        auto dbg = AST::InstSetDebugInfo(name, expr->accessor, expr->op);
        return std::make_shared<typedAST::InstSet>(readVar(_this), res, toStore, operationTy, dbg);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(!res.empty()){
        error(name, "Tried assigning to a method, which is forbidden.");
    }
    error(name, fmt::format("Class '{}' doesn't contain this symbol", demangleName(currentClass->mangledName)));
    // Never hit
    return nullptr;
}
// Misc
Token ASTTransformer::syntheticToken(const string& str){
    return Token(TokenType::IDENTIFIER, str);
}
void ASTTransformer::updateLine(const Token token){
    current->line = token.str.line;
}
void ASTTransformer::error(const Token token, const string& msg) noexcept(false){
    errorHandler::addCompileError(msg, token);
    hadError = true;
    throw TransformerException();
}
void ASTTransformer::error(const string& message) noexcept(false){
    errorHandler::addSystemError("System compile error [line " + std::to_string(current->line) + "] in '" + curUnit->file->name + "': \n" + message + "\n");
    hadError = true;
    throw TransformerException();
}

typedAST::Block ASTTransformer::parseStmtsToBlock(vector<AST::ASTNodePtr>& stmts){
    typedAST::Block block;
    for(auto stmt : stmts){
        vector<typedAST::nodePtr> stmtVec;
        try {
            stmtVec = evalASTStmt(stmt);
        }catch(TransformerException e){
            // Nothing
        }
        // Dead code elimination
        // If a terminator instruction is detected in this block, don't eval anything below it
        for (int i = stmtVec.size() - 1; i >= 0; i--) {
            if (stmtVec[i]->type == typedAST::NodeType::UNCOND_JMP || stmtVec[i]->type == typedAST::NodeType::RETURN) {
                stmtVec.resize(i + 1);
                block.terminates = true;
                block.stmts.insert(block.stmts.end(), stmtVec.begin(), stmtVec.end());
                return block;
            }
        }
        block.stmts.insert(block.stmts.end(), stmtVec.begin(), stmtVec.end());

    }
    return block;
}
typedAST::Block ASTTransformer::parseStmtToBlock(AST::ASTNodePtr stmt){
    typedAST::Block block;
    vector<typedAST::nodePtr> stmtVec;
    try {
        stmtVec = evalASTStmt(stmt);
    }catch(TransformerException e){
        //
    }
    // Dead code elimination
    // If a terminator instruction is detected in this block, don't eval anything below it
    for(int i = stmtVec.size()-1; i >= 0; i--){
        if(stmtVec[i]->type == typedAST::NodeType::UNCOND_JMP || stmtVec[i]->type == typedAST::NodeType::RETURN){
            stmtVec.resize(i + 1);
            block.terminates = true;
            block.stmts.insert(block.stmts.end(), stmtVec.begin(), stmtVec.end());
            return block;
        }
    }
    block.stmts.insert(block.stmts.end(), stmtVec.begin(), stmtVec.end());
    return block;
}
typedAST::exprPtr ASTTransformer::evalASTExpr(std::shared_ptr<AST::ASTNode> node){
    node->accept(this);
    auto tmp = returnedExpr;
    // Sanity check
    returnedExpr = nullptr;
    return tmp;
}
vector<typedAST::nodePtr> ASTTransformer::evalASTStmt(std::shared_ptr<AST::ASTNode> node){
    node->accept(this);
    auto tmp = nodesToReturn;
    // Sanity check
    nodesToReturn.clear();
    return tmp;
}

types::tyVarIdx ASTTransformer::addType(types::tyPtr ty){
    typeEnv.emplace_back(ty, vector<std::shared_ptr<types::TypeConstraint>>());
    return typeEnv.size() - 1;
}
types::tyVarIdx ASTTransformer::createEmptyTy(){
    return addType(nullptr);
}
void ASTTransformer::addTypeConstraint(const types::tyVarIdx ty, std::shared_ptr<types::TypeConstraint> constraint){
    typeEnv[ty].second.push_back(constraint);
}
types::tyVarIdx ASTTransformer::getBasicType(const types::TypeFlag ty){
    return static_cast<types::tyVarIdx>(ty);
}
void ASTTransformer::addBasicTypes(){
    addType(types::getBasicType(types::TypeFlag::NIL));
    addType(types::getBasicType(types::TypeFlag::BOOL));
    addType(types::getBasicType(types::TypeFlag::NUMBER));
    addType(types::getBasicType(types::TypeFlag::STRING));
    addType(types::getBasicType(types::TypeFlag::MUTEX));
    addType(types::getBasicType(types::TypeFlag::RANGE));
    addType(types::getBasicType(types::TypeFlag::FILE));
    addType(types::getBasicType(types::TypeFlag::ANY));
}

types::tyVarIdx ASTTransformer::getInstFieldTy(const types::tyVarIdx possibleInstTy, string field){
    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::InstGetFieldTyConstraint>(possibleInstTy, field));
    return ty;
}

CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, string funcName) {
    enclosing = _enclosing;
    type = _type;
    line = 0;
    func = std::make_shared<typedAST::Function>();
    func->name = funcName;
}
#pragma endregion