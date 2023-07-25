#include "ASTToTypedAST.h"
#include "../../Parsing/ASTProbe.h"
#include "../../Includes/fmt/format.h"
#include "../../ErrorHandling/errorHandler.h"

using namespace passes;
using namespace typedASTParser;

CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, string funcName) {
    enclosing = _enclosing;
    type = _type;
    line = 0;
    func.name = funcName;
}

ASTTransformer::ASTTransformer() {
    curUnitIndex = 0;
    hadError = false;
    current = nullptr;
    currentClass = nullptr;
    returnedExpr = nullptr;
    addBasicTypes();
}

std::pair<typedAST::Function, vector<File*>> ASTTransformer::run(vector<ESLModule *> &_units, std::unordered_map<AST::FuncLiteral*, vector<variableFinder::Upvalue>> _upvalMap){
    units = _units;
    upvalueMap = _upvalMap;
    current = new CurrentChunkInfo(nullptr, FuncType::TYPE_SCRIPT, "func.main");
    for (ESLModule* unit : units) {
        curUnit = unit;
        sourceFiles.push_back(unit->file);
        for (int i = 0; i < unit->stmts.size(); i++) {
            // Doing this here so that even if an error is detected, we go on and possibly catch other(valid) errors
            try {
                auto stmts = evalASTStmt(unit->stmts[i]);
                current->func.block.stmts.insert(current->func.block.stmts.end(), stmts.begin(), stmts.end());
            }
            catch (TransformerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curUnitIndex++;
    }
    auto fn = current->func;
    delete current;
    return std::make_pair(fn, sourceFiles);
}

static Token probeToken(AST::ASTNodePtr ptr){
    AST::ASTProbe p;
    ptr->accept(&p);
    return p.getProbedToken();
}

#pragma region Visitor
void ASTTransformer::visitAssignmentExpr(AST::AssignmentExpr* expr){
    auto rhs = evalASTExpr(expr->value);
    returnedExpr = storeToVar(expr->name, rhs);
}
void ASTTransformer::visitSetExpr(AST::SetExpr* expr){
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        auto collection = evalASTExpr(expr->callee);
        auto toStore = evalASTExpr(expr->value);
        returnedExpr = std::make_shared<typedAST::CollectionSet>(collection, evalASTExpr(expr->field), toStore);
        return;
    }
    auto resolved = tryResolveThis(expr);
    if(resolved) {
        returnedExpr = resolved;
        return;
    }
    // Guaranteed to be a literal
    string field = probeToken(expr->field).getLexeme();
    returnedExpr = std::make_shared<typedAST::InstSet>(evalASTExpr(expr->callee), field, evalASTExpr(expr->value));
}
void ASTTransformer::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        auto collection = evalASTExpr(expr->callee);
        auto field = evalASTExpr(expr->field);
        // TODO:: actually do type inference for collections
        auto ty = getBasicType(types::TypeFlag::ANY);
        returnedExpr = std::make_shared<typedAST::CollectionGet>(collection, field, ty);
        return;
    }
    auto resolved = tryResolveThis(expr);
    if(resolved) {
        returnedExpr = resolved;
        return;
    }
    // Guaranteed to be a literal
    string field = probeToken(expr->field).getLexeme();
    auto inst = evalASTExpr(expr->callee);
    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::InstGetFieldTyConstraint>(inst->exprType, field));
    returnedExpr = std::make_shared<typedAST::InstGet>(inst, field, ty);
}

void ASTTransformer::visitConditionalExpr(AST::ConditionalExpr* expr) {
    auto cond = evalASTExpr(expr->condition);
    auto thenBranch = evalASTExpr(expr->mhs);
    auto elseBranch = evalASTExpr(expr->rhs);
    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(thenBranch->exprType));
    addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(elseBranch->exprType));
    returnedExpr = std::make_shared<typedAST::ConditionalExpr>(cond, thenBranch, elseBranch, ty);
}
void ASTTransformer::visitRangeExpr(AST::RangeExpr* expr) {
    auto lhs = evalASTExpr(expr->start);
    auto rhs = evalASTExpr(expr->end);
    returnedExpr = std::make_shared<typedAST::RangeExpr>(lhs, rhs, expr->endInclusive, getBasicType(types::TypeFlag::RANGE));
}
void ASTTransformer::visitBinaryExpr(AST::BinaryExpr* expr) {
    auto lhs = evalASTExpr(expr->left);
    auto rhs = evalASTExpr(expr->right);
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
                addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(lhs->exprType));
                addTypeConstraint(ty, std::make_shared<types::AddTyConstraint>(rhs->exprType));
                exprType = ty;
            }
            returnedExpr = std::make_shared<typedAST::ArithmeticExpr>(lhs, rhs, op, exprType);
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
                case TokenType::INSTANCEOF: op = typedAST::ComparisonOp::INSTANCEOF; break;
            }
            returnedExpr = std::make_shared<typedAST::ComparisonExpr>(lhs, rhs, op, getBasicType(types::TypeFlag::BOOL));
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
    returnedExpr = std::make_shared<typedAST::UnaryExpr>(rhs, op, ty);
}

void ASTTransformer::visitCallExpr(AST::CallExpr* expr) {
    auto callee = evalASTExpr(expr->callee);
    vector<typedAST::exprPtr> args;
    vector<types::tyVarIdx> argTypes;
    for(auto arg : expr->args){
        args.push_back(evalASTExpr(arg));
        argTypes.push_back(args.back()->exprType);
    }
    auto tryInvoke = tryConvertToInvoke(callee, args);
    if(tryInvoke) {
        returnedExpr = tryInvoke;
        return;
    }
    auto ty = createEmptyTy();
    addTypeConstraint(ty, std::make_shared<types::CallResTyConstraint>(callee->exprType));
    returnedExpr = std::make_shared<typedAST::CallExpr>(callee, args, ty);
}
void ASTTransformer::visitNewExpr(AST::NewExpr* expr) {
    auto klass = getClassInfoFromExpr(expr->call->callee);
    auto callee = globals.at(klass->mangledName);
    vector<typedAST::exprPtr> args;
    for(auto arg : expr->call->args){
        args.push_back(evalASTExpr(arg));
    }
    // getClassFromExpr will throw an error if it doesn't find a class,
    // so callee.valPtr is guaranteed to be a constrained to a types::ClassType
    auto instTy = addType(std::make_shared<types::InstanceType>(klass->classTy));
    returnedExpr = std::make_shared<typedAST::NewExpr>(std::make_shared<typedAST::VarRead>(callee.valPtr), args, instTy);
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
    returnedExpr = std::make_shared<typedAST::AsyncExpr>(callee, args, addType(futTy));
}
void ASTTransformer::visitAwaitExpr(AST::AwaitExpr* expr) {
    auto val = evalASTExpr(expr->expr);
    auto furAwaitTy = createEmptyTy();
    addTypeConstraint(furAwaitTy, std::make_shared<types::AwaitTyConstraint>(val->exprType));
    returnedExpr = std::make_shared<typedAST::AwaitExpr>(val, furAwaitTy);
}

void ASTTransformer::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    vector<typedAST::exprPtr> fields;
    for(auto field : expr->members){
        fields.push_back(evalASTExpr(field));
    }
    auto arrTy = std::make_shared<types::ArrayType>(getBasicType(types::TypeFlag::ANY));
    returnedExpr = std::make_shared<typedAST::ArrayExpr>(fields, addType(arrTy));
}
void ASTTransformer::visitStructLiteralExpr(AST::StructLiteral* expr) {
    vector<std::pair<string, typedAST::exprPtr>> fields;
    for(auto field : expr->fields){
        //this gets rid of quotes, ""Hello world""->"Hello world"
        string temp = field.name.getLexeme();
        temp.erase(0, 1);
        temp.erase(temp.size() - 1, 1);
        fields.push_back(std::make_pair(temp, evalASTExpr(field.expr)));
    }
    auto hashmapTy = std::make_shared<types::HashMapType>(getBasicType(types::TypeFlag::ANY));
    returnedExpr = std::make_shared<typedAST::HashmapExpr>(fields, addType(hashmapTy));
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
    }
    returnedExpr = std::make_shared<typedAST::LiteralExpr>(variant, ty);
}

static std::pair<bool, bool> classContainsMethod(string publicField, std::shared_ptr<ClassChunkInfo> klass);

void ASTTransformer::visitSuperExpr(AST::SuperExpr* expr) {
    if (!currentClass) {
        error(expr->methodName, "Can't use 'super' outside of a class.");
    }
    else if (!currentClass->parent) {
        error(expr->methodName, "Can't use 'super' in a class with no superclass.");
    }
    string methodName = expr->methodName.getLexeme();
    auto res = classContainsMethod(methodName, currentClass->parent);
    if(!res.first){
        error(expr->methodName, "Method doesn't exist.");
    }
    // Private fields are prefixed with a "priv."
    if(res.second) methodName = "priv." + methodName;
    auto inst = readVar(syntheticToken("this"));
    varPtr ptrToClass = globals.at(currentClass->parent->mangledName).valPtr;
    returnedExpr = std::make_shared<typedAST::InstSuperGet>(inst, methodName, ptrToClass, currentClass->parent->classTypeIdx);
}

static int anonFuncCounter = 0;
void ASTTransformer::visitFuncLiteral(AST::FuncLiteral* expr) {
    auto upvalMap = upvalueMap.at(expr);

    auto retTy = createEmptyTy();
    std::shared_ptr<types::FunctionType> fnTy = std::make_shared<types::FunctionType>(expr->arity, retTy, false);

    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC, "anonfunc." + std::to_string(anonFuncCounter++));
    current->func.fnTy = addType(fnTy);
    current->retTy = retTy;

    // Upvalues are gathered from the enclosing function
    for(int i = 0; i < upvalMap.size(); i++){
        auto& upval = upvalMap[i];

        std::shared_ptr<typedAST::VarDecl> upvalPtr = nullptr;
        std::shared_ptr<typedAST::VarDecl> varToCapturePtr = nullptr;

        if(upval.isLocal) varToCapturePtr = current->enclosing->locals[upval.index].ptr;
        else varToCapturePtr = current->enclosing->upvalues[upval.index].ptr;

        // Kinda hacky, but it makes sure to propagate possible types of upvalues when type inferring
        upvalPtr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::UPVALUE, varToCapturePtr->possibleTypes, varToCapturePtr->typeConstrained);
        // Sets upvalue in CurrentChunkInfo, used when resolving upvalues in readVar and storeToVar
        current->upvalues.emplace_back(upval.name, upvalPtr);
        // Sets up the (enclosing var -> upvalue) pairs
        current->upvalPtrs.emplace_back(varToCapturePtr, upvalPtr);
    }

    // No need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    for (AST::ASTVar& var : expr->args) {
        current->func.args.push_back(declareLocalVar(var));
        defineLocalVar();
        auto ptr = current->func.args.back();
        // Set the type of the argument
        ptr->possibleTypes = getBasicType(types::TypeFlag::ANY);
        ptr->typeConstrained = true;
    }
    current->func.block = parseStmtsToBlock(expr->body->statements);
    auto upvals = current->upvalPtrs;
    auto func = endFuncDecl();
    returnedExpr = std::make_shared<typedAST::CreateClosureExpr>(func, upvals);
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
        if(ptr->varType == typedAST::VarType::GLOBAL_CLASS){
            error(symbol, fmt::format("Classes aren't first class values.", depPtr->pathString.getLexeme(), depPtr->alias.getLexeme()));
        }
        returnedExpr = std::make_shared<typedAST::VarRead>(ptr);
        return;
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
        var = declareGlobalVar(fullGlobalSymbol, AST::ASTDeclType::VAR);
    }else var = declareLocalVar(decl->var);

    // Compile the right side of the declaration, if there is no right side, the variable is initialized as nil
    typedAST::exprPtr initializer = std::make_shared<typedAST::LiteralExpr>(nullptr, getBasicType(types::TypeFlag::NIL));
    if (decl->value != nullptr) {
        initializer = evalASTExpr(decl->value);
    }

    if(decl->var.type == AST::ASTVarType::GLOBAL){
        defineGlobalVar(fullGlobalSymbol);
    }else defineLocalVar();

    auto toStore = storeToVar(decl->var.name, initializer);

    nodesToReturn = {var, toStore};
}
void ASTTransformer::visitFuncDecl(AST::FuncDecl* decl) {
    updateLine(decl->getName());
    auto retTy = createEmptyTy();
    std::shared_ptr<types::FunctionType> fnTy = std::make_shared<types::FunctionType>(decl->arity, retTy, false);
    auto fnTyIdx = addType(fnTy);

    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + decl->getName().getLexeme();
    varPtr name = declareGlobalVar(fullGlobalSymbol, AST::ASTDeclType::FUNCTION, fnTyIdx);
    // Defining the function here to allow for recursion
    defineGlobalVar(fullGlobalSymbol);

    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC, "func." + fullGlobalSymbol);
    current->func.fnTy = fnTyIdx;
    current->retTy = retTy;

    // No need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    for (AST::ASTVar& var : decl->args) {
        current->func.args.push_back(declareLocalVar(var));
        defineLocalVar();
        auto ptr = current->func.args.back();
        // Set the type of the argument
        ptr->possibleTypes = getBasicType(types::TypeFlag::ANY);
        ptr->typeConstrained = true;
    }
    current->func.block = parseStmtsToBlock(decl->body->statements);
    auto func = endFuncDecl();
    nodesToReturn = {std::make_shared<typedAST::FuncDecl>(func)};
}
void ASTTransformer::visitClassDecl(AST::ClassDecl* decl) {
    std::shared_ptr<types::ClassType> classTy = std::make_shared<types::ClassType>();
    auto classTyIdx = addType(classTy);
    updateLine(decl->getName());

    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + decl->getName().getLexeme();
    varPtr var = declareGlobalVar(fullGlobalSymbol, AST::ASTDeclType::CLASS, classTyIdx);
    // Defining the function here to allows for creating a new instance of a class inside its own methods
    defineGlobalVar(fullGlobalSymbol);

    currentClass = std::make_shared<ClassChunkInfo>(fullGlobalSymbol, classTy, classTyIdx);
    globalClasses.insert_or_assign(fullGlobalSymbol, currentClass);

    if (decl->inheritedClass) {
        //decl->inheritedClass is always either a LiteralExpr with an identifier token or a ModuleAccessExpr

        //if a class wants to inherit from a class in another file of the same name, the import has to use an alias, otherwise we get
        //undefined behavior (eg. class a : a)
        try { // try block because getClassInfoFromExpr can throw
            auto superclass = getClassInfoFromExpr(decl->inheritedClass);
            currentClass->parent = superclass;
            // Make the class type aware of the inheritance(copies methods and fields)
            currentClass->inherit(superclass);
            currentClass->classTy->inherit(superclass->classTy);
        }catch(TransformerException& e){

        }
    }
    // Before compiling any code make the class type aware of its own fields and methods
    // Think of this as a sort of forward declaration

    // Since every method(not marked as override) and field must be unique, error if a duplicate name is found(regardless of visibility)
    auto duplicateSymbolLambda = [&](Token publicName, bool isMethod, bool methodOverrides = false){
        string pubNameStr = publicName.getLexeme();
        try{
            if(isMethod && !methodOverrides && (classTy->methods.contains(pubNameStr) || classTy->methods.contains("priv." + pubNameStr))){
                error(publicName, "Duplicate symbol found. Methods that override need to be explicitly marked with 'override'.");
            }
            else if(isMethod && methodOverrides && !(classTy->methods.contains(pubNameStr) || classTy->methods.contains("priv." + pubNameStr))){
                // If this method has been marked as override but there isn't a parent method to override, throw an error
                error(publicName, "Method marked as 'override' but there is no parent method with matching name.");
            }
            if(classTy->fields.contains(pubNameStr) || classTy->fields.contains("priv." + pubNameStr)){
                error(publicName, "Duplicate symbol found. Fields are inherited from parents.");
            }
        }catch(TransformerException& e){

        }
    };
    for(auto field : decl->fields){
        // First check if this is a duplicate symbol
        duplicateSymbolLambda(field.field, false);

        string str = field.field.getLexeme();
        str = (field.isPublic ? str : ("priv." + str));
        // Can't override symbols
        int idx = currentClass->fields.size();

        currentClass->fields.insert_or_assign(str, idx);
        classTy->fields.insert_or_assign(str, getBasicType(types::TypeFlag::ANY));
    }
    vector<types::tyVarIdx> returnTys;
    for(auto method : decl->methods){
        // First check if this is a duplicate symbol
        duplicateSymbolLambda(method.method->getName(), true, method.overrides);

        string str = method.method->getName().getLexeme();
        str = (method.isPublic ? str : ("priv." + str));

        // Creates new type for each function(even if it's an override)
        auto retTy = createEmptyTy();
        auto methodTy = std::make_shared<types::FunctionType>(method.method->arity, retTy, false);
        auto tyIdx = addType(methodTy);
        returnTys.push_back(retTy);

        // TODO: check if method overlaps some prev defined field
        classTy->methods.insert_or_assign(str, tyIdx);
    }
    // After type forward decl, compile methods
    int i = 0;
    for(auto method : decl->methods){
        string str = method.method->getName().getLexeme();
        str = (method.isPublic ? str : ("priv." + str));
        // Get the index of the linearized field
        int idx = classTy->methods.size();
        if(currentClass->methods.contains(str)){
            idx = currentClass->methods[str].second;
        }

        // Transform the method
        auto retTy = returnTys[i];
        auto methodTy = currentClass->classTy->methods.at(str);
        auto transformedMethod = createMethod(method.method.get(), decl->getName().getLexeme(), methodTy, retTy);

        currentClass->methods.insert_or_assign(str, std::make_pair(transformedMethod, idx));
        i++;
    }
    // Only pass pointer to where the class is stored if this class inherits
    varPtr paren = nullptr;
    if(currentClass->parent){
        paren = globals.at(currentClass->parent->mangledName).valPtr;
    }
    auto klass = std::make_shared<typedAST::ClassDecl>(currentClass->classTypeIdx, paren);
    nodesToReturn = {};
    currentClass = nullptr;
}

void ASTTransformer::visitExprStmt(AST::ExprStmt* stmt) {
    nodesToReturn= {evalASTExpr(stmt->expr)};
}
void ASTTransformer::visitBlockStmt(AST::BlockStmt* stmt) {
    beginScope();
    vector<typedAST::nodePtr> nodes;
    for(auto temp : stmt->statements){
        vector<typedAST::nodePtr> nodesToInsert;
        try{
            nodesToInsert = evalASTStmt(temp);
        }catch(TransformerException e){
            //
        }
        nodes.insert(nodes.end(), nodesToInsert.begin(), nodesToInsert.end());
    }
    endScope();
    nodesToReturn.insert(nodesToReturn.end(), nodes.begin(), nodes.end());
}

void ASTTransformer::visitIfStmt(AST::IfStmt* stmt) {
    auto cond = evalASTExpr(stmt->condition);
    typedAST::Block thenBlock = parseStmtToBlock(stmt->thenBranch);
    typedAST::Block elseBlock = parseStmtToBlock(stmt->elseBranch);

    nodesToReturn = {std::make_shared<typedAST::IfStmt>(cond, thenBlock, elseBlock)};
}
void ASTTransformer::visitWhileStmt(AST::WhileStmt* stmt) {
    auto cond = evalASTExpr(stmt->condition);
    typedAST::Block loopBody = parseStmtToBlock(stmt->body);
    nodesToReturn = {std::make_shared<typedAST::WhileStmt>(cond, loopBody)};
}
void ASTTransformer::visitForStmt(AST::ForStmt* stmt) {
    // Convert for to a while loop with "init" directly above it in a block
    beginScope();
    vector<typedAST::nodePtr> init;
    typedAST::exprPtr cond = nullptr;
    // Order of eval is important for types
    // Init can be a single statement(expr statement) or a var decl + var store
    if(stmt->init) init = evalASTStmt(stmt->init);
    if(stmt->condition) cond = evalASTExpr(stmt->condition);

    typedAST::Block loopBody = parseStmtToBlock(stmt->body);
    typedAST::exprPtr inc = evalASTExpr(stmt->increment);
    // Init isn't treated as part of the while loop, but as statement(s) by itself(exprStmt or var decl + var store)
    nodesToReturn.insert(nodesToReturn.end(), init.begin(), init.end());
    // AfterLoopExpr is special field that won't be part of the loop body, but in a basic block by itself
    nodesToReturn.push_back(std::make_shared<typedAST::WhileStmt>(cond, loopBody, inc));
}
void ASTTransformer::visitBreakStmt(AST::BreakStmt* stmt) {
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::BREAK)};
}
void ASTTransformer::visitContinueStmt(AST::ContinueStmt* stmt) {
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::CONTINUE)};
}

void ASTTransformer::visitSwitchStmt(AST::SwitchStmt* stmt) {
    auto cond = evalASTExpr(stmt->expr);
    vector<std::pair<std::variant<double, void*, bool, string>, int>> constants;
    vector<typedAST::Block> caseBlocks;
    caseBlocks.resize(stmt->cases.size());
    int defaultBlockIdx = -1;
    int i = 0;
    // Bitflag for case constant types
    // 0: all ints, 1: all nums, 2: all strings, 3: mixed
    uint8_t flag;
    for(auto _case : stmt->cases){
        if(_case->caseType.type == TokenType::DEFAULT){
            defaultBlockIdx = i;
            caseBlocks.push_back(parseStmtsToBlock(_case->stmts));
            i++;
            continue;
        }
        for (auto literal: _case->constants) {
            if(literal.type == TokenType::STRING){
                string temp = literal.getLexeme();
                temp.erase(0, 1);
                temp.erase(temp.size() - 1, 1);
                constants.emplace_back(temp, i);
                flag |= 2;
            }else if(literal.type == TokenType::NUMBER) {
                double num = std::stod(literal.getLexeme());
                if(trunc(num) != num) flag |= 1;
                constants.emplace_back(num, i);
            }
            else if(literal.type == TokenType::NIL) {
                constants.emplace_back(nullptr, i);
                flag |= 3;
            }
            else {
                constants.emplace_back(literal.type == TokenType::TRUE, i);
                flag |= 3;
            }
        }
        caseBlocks.push_back(parseStmtsToBlock(_case->stmts));
        i++;
    }
    typedAST::SwitchConstantsType constantsType;
    switch(flag){
        case 0: constantsType = typedAST::SwitchConstantsType::ALL_INT; break;
        case 1: constantsType = typedAST::SwitchConstantsType::ALL_NUM; break;
        case 2: constantsType = typedAST::SwitchConstantsType::ALL_STRING; break;
        case 3: constantsType = typedAST::SwitchConstantsType::MIXED; break;
    }
    nodesToReturn = {std::make_shared<typedAST::SwitchStmt>(cond, constants, caseBlocks, constantsType, defaultBlockIdx)};
}
void ASTTransformer::visitCaseStmt(AST::CaseStmt* _case) {
    //Nothing, everything is handled in visitSwitchStmt
}
void ASTTransformer::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    nodesToReturn = {std::make_shared<typedAST::UncondJump>(typedAST::JumpType::ADVANCE)};
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
    else expr = std::make_shared<typedAST::LiteralExpr>(nullptr, getBasicType(types::TypeFlag::NIL));
    addTypeConstraint(current->retTy, std::make_shared<types::AddTyConstraint>(expr->exprType));
    nodesToReturn = {std::make_shared<typedAST::ReturnStmt>(expr)};
}
#pragma endregion

#pragma region Helpers
// Variables
// Checks all imports to see if the symbol 'token' is imported
varPtr ASTTransformer::checkSymbol(Token symbol){
    for (Dependency& dep : curUnit->deps) {
        string fullSymbol = dep.module->file->name + std::to_string(dep.module->id) + "." + symbol.getLexeme();
        if (dep.alias.type == TokenType::NONE && globals.contains(fullSymbol)) {
            Globalvar& gvar = globals.at(fullSymbol);
            if(!gvar.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            else if(gvar.valPtr->varType == typedAST::VarType::GLOBAL_CLASS) error(symbol, "Cannot read a class.");
            return gvar.valPtr;
        }
    }
    return nullptr;
}
// Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
varPtr ASTTransformer::resolveGlobal(Token symbol, bool canAssign){
    string fullSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." +symbol.getLexeme();
    auto it = globals.find(fullSymbol);
    if (canAssign) {
        // Global is in this module
        if (it != globals.end()){
            Globalvar& var = it->second;
            if(var.valPtr->varType == typedAST::VarType::GLOBAL_FUNC) error(symbol, "Cannot assign to a function.");
            else if(var.valPtr->varType == typedAST::VarType::GLOBAL_CLASS) error(symbol, "Cannot assign to a class.");

            if(!var.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }
            return var.valPtr;
        }
        error(symbol, "Cannot assign to a variable not declared in this module.");
    }
    else {
        if (it != globals.end()) {
            if(!it->second.isDefined){
                error(symbol, fmt::format("Trying to access variable '{}' before it's initialized.", symbol.getLexeme()));
            }else if(it->second.valPtr->varType == typedAST::VarType::GLOBAL_CLASS) error(symbol, "Classes aren't first class values.");
            return it->second.valPtr;
        }
        else {
            // Global variables defined in an imported file are guaranteed to be already defined
            return checkSymbol(symbol);
        }
    }
    // Never hit, checkSymbol returns -1 upon failure
    return nullptr;
}

varPtr ASTTransformer::declareGlobalVar(string name, AST::ASTDeclType type, types::tyVarIdx contraintType){
    typedAST::VarType varty;
    switch(type){
        case AST::ASTDeclType::VAR: varty = typedAST::VarType::GLOBAL;
        case AST::ASTDeclType::FUNCTION: varty = typedAST::VarType::GLOBAL_FUNC;
        case AST::ASTDeclType::CLASS: varty = typedAST::VarType::GLOBAL_CLASS;
    }
    varPtr var;
    if(contraintType) {
        var = std::make_shared<typedAST::VarDecl>(varty, contraintType, true);
    }else{
        var = std::make_shared<typedAST::VarDecl>(varty, createEmptyTy());
    }
    globals.insert_or_assign(name, Globalvar(var));
    return var;
}
void ASTTransformer::defineGlobalVar(string name){
    Globalvar& gvar = globals.at(name);
    gvar.isDefined = true;
}

// Makes sure the compiler is aware that a stack slot is occupied by this local variable
varPtr ASTTransformer::declareLocalVar(AST::ASTVar& var) {
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
    return addLocal(var);
}
void ASTTransformer::defineLocalVar(){
    current->locals.back().depth = current->scopeDepth;
}
varPtr ASTTransformer::addLocal(AST::ASTVar var){
    updateLine(var.name);
    current->locals.emplace_back(var.name.getLexeme(), -1, var.type == AST::ASTVarType::UPVALUE);
    Local& local = current->locals.back();
    auto varTy = createEmptyTy();
    if(!local.isUpval) {
        local.ptr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::LOCAL, varTy);
    }else{
        local.ptr = std::make_shared<typedAST::VarDecl>(typedAST::VarType::UPVALUE, varTy);
    }
    return local.ptr;
}

int ASTTransformer::resolveLocal(Token name){
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
int ASTTransformer::resolveUpvalue(Token name){
    string upvalName = name.getLexeme();
    for(int i = 0; i < current->upvalues.size(); i++){
        if(upvalName == current->upvalues[i].name) return i;
    }
    return -1;
}

// Order of checking:
// locals->upvalues->implicit object fields->globals->natives
typedAST::exprPtr ASTTransformer::readVar(Token name){
    updateLine(name);
    int argIndex = resolveLocal(name);
    if (argIndex != -1) {
        varPtr valPtr = current->locals[argIndex].ptr;
        return std::make_shared<typedAST::VarRead>(valPtr);
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        varPtr upvalPtr = current->upvalues[argIndex].ptr;
        return std::make_shared<typedAST::VarRead>(upvalPtr);
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
        return std::make_shared<typedAST::VarRead>(globalPtr);
    }
    string nativeName = name.getLexeme();
    auto it = nativesTypes.find(nativeName);
    if(it != nativesTypes.end()) return std::make_shared<typedAST::VarReadNative>(nativeName, it->second);

    error(name, fmt::format("'{}' doesn't match any declared variable name or native function name.", nativeName));
    return nullptr;
}
// Can't store to natives so that check is skipped
typedAST::exprPtr ASTTransformer::storeToVar(Token name, typedAST::exprPtr toStore){
    // If valPtr isn't type constrained possibleTypes is a TypeUnion
    // TODO: something with constrained tys
    updateLine(name);
    int argIndex = resolveLocal(name);
    if (argIndex != -1) {
        varPtr valPtr = current->locals[argIndex].ptr;
        addTypeConstraint(valPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(valPtr, toStore);
    }
    else if ((argIndex = resolveUpvalue(name)) != -1) {
        varPtr upvalPtr = current->upvalues[argIndex].ptr;
        addTypeConstraint(upvalPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(upvalPtr, toStore);
    }
    std::shared_ptr<typedAST::InstSet> implicitClassField = resolveClassFieldStore(name, toStore);
    if(implicitClassField){
        if(current->type == FuncType::TYPE_FUNC){
            error(name, fmt::format("Cannot access object fields within a closure without 'this', use this.{}", name.getLexeme()));
        }
        return implicitClassField;
    }
    varPtr globalPtr = resolveGlobal(name, true);
    if(globalPtr){
        addTypeConstraint(globalPtr->possibleTypes, std::make_shared<types::AddTyConstraint>(toStore->exprType));
        return std::make_shared<typedAST::VarStore>(globalPtr, toStore);
    }
    auto it = nativesTypes.find(name.getLexeme());
    if(it != nativesTypes.end()) {
        error(name, fmt::format("'{}' is a native function, cannot assign to native functions.", name.getLexeme()));
        return nullptr;
    }

    error(name, fmt::format("'{}' undefined symbol.", name.getLexeme()));
    return nullptr;
}

void ASTTransformer::beginScope(){
    current->scopeDepth++;
}
// Pop every variable that was declared in this scope
void ASTTransformer::endScope(){
    // First lower the scope, the check for every var that is deeper than the parserCurrent scope
    current->scopeDepth--;
    // Pop from the stack
    while (current->locals.size() > 0 && current->locals.back().depth > current->scopeDepth) {
        current->locals.pop_back();
    }
}

// Functions
typedAST::Function ASTTransformer::endFuncDecl(){
    // Get the function we've just transformed, delete its compiler info, and replace it with the enclosing functions compiler info
    // If function doesn't contain an explicit return stmt, add it to the end of the function
    if(!current->func.block.terminates){
        std::shared_ptr<typedAST::ReturnStmt> ret = nullptr;
        // Constructors return must return the instance
        if(current->type == FuncType::TYPE_CONSTRUCTOR){
            auto _this = readVar(syntheticToken("this"));
            ret = std::make_shared<typedAST::ReturnStmt>(_this);
        }
        else ret = std::make_shared<typedAST::ReturnStmt>(std::make_shared<typedAST::LiteralExpr>(nullptr, getBasicType(types::TypeFlag::NIL)));
        addTypeConstraint(current->retTy, std::make_shared<types::AddTyConstraint>(ret->expr->exprType));
        current->func.block.stmts.push_back(ret);
        current->func.block.terminates = true;
    }
    auto func = current->func;
    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
    // Dead code elimination
    for(int i = func.block.stmts.size() - 1; i >= 0; i--){
        auto stmt = func.block.stmts[i];
        if(stmt->type == typedAST::NodeType::RETURN || stmt->type == typedAST::NodeType::UNCOND_JMP){
            func.block.stmts.resize(i + 1);
            break;
        }
    }
    return func;
}

// Classes and methods
// Class name is for recognizing constructor
typedAST::Function ASTTransformer::createMethod(AST::FuncDecl* _method, string className, types::tyVarIdx fnTy, types::tyVarIdx retTy){
    updateLine(_method->getName());
    string fullGlobalSymbol = curUnit->file->name + std::to_string(curUnit->id) + "." + _method->getName().getLexeme();
    FuncType type = FuncType::TYPE_METHOD;
    // Constructors are treated separately, but are still methods
    if (_method->getName().getLexeme() == className) type = FuncType::TYPE_CONSTRUCTOR;
    current = new CurrentChunkInfo(current, type, "method." + className + "." + fullGlobalSymbol);
    current->func.fnTy = fnTy;
    // No need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    for (AST::ASTVar& var : _method->args) {
        current->func.args.push_back(declareLocalVar(var));
        defineLocalVar();
        auto ptr = current->func.args.back();
        // 'this' is of a known type so annotate it to enable optimizations
        if(var.name.getLexeme() == "this"){
            ptr->possibleTypes = addType(std::make_shared<types::InstanceType>(currentClass->classTy));
        }else{
            // Set the type of the other arguments
            ptr->possibleTypes = getBasicType(types::TypeFlag::ANY);
        }
        ptr->typeConstrained = true;
    }
    current->func.block = parseStmtsToBlock(_method->body->statements);
    auto func = endFuncDecl();
    return func;
}
std::shared_ptr<typedAST::InvokeExpr> ASTTransformer::tryConvertToInvoke(typedAST::exprPtr callee, vector<typedAST::exprPtr> args){
    vector<types::tyVarIdx> argTys;
    for(auto arg : args){
        argTys.push_back(arg->exprType);
    }
    if(callee->type == typedAST::NodeType::INST_GET){
        std::shared_ptr<typedAST::InstGet> casted = std::reinterpret_pointer_cast<typedAST::InstGet>(callee);
        auto invokeTy = createEmptyTy();
        addTypeConstraint(invokeTy, std::make_shared<types::CallResTyConstraint>(casted->exprType));
        return std::make_shared<typedAST::InvokeExpr>(casted->instance, casted->field, args, invokeTy);
    }else if(callee->type == typedAST::NodeType::INST_SUPER_GET){
        std::shared_ptr<typedAST::InstSuperGet> casted = std::reinterpret_pointer_cast<typedAST::InstSuperGet>(callee);
        auto invokeTy = createEmptyTy();
        addTypeConstraint(invokeTy, std::make_shared<types::CallResTyConstraint>(casted->exprType));
        return std::make_shared<typedAST::InvokeExpr>(casted->instance, casted->method, args, invokeTy, casted->klass);
    }
    return nullptr;
}
// Resolve implicit object field access
// Turns a hash map lookup into an array linear search, but still faster than allocating memory using ObjString::createStr
// First bool in pair is if the search was succesful, second is if the field found was public or private
static std::pair<bool, bool> classContainsField(string publicField, std::shared_ptr<ClassChunkInfo> klass){
    string privateField = "priv." + publicField;
    for(auto it : klass->fields){
        if(publicField == it.first) return std::pair(true, true);
        else if(privateField == it.first) return std::pair(true, false);
    }
    return std::pair(false, false);
}
static std::pair<bool, bool> classContainsMethod(string publicField, std::shared_ptr<ClassChunkInfo> klass){
    string privateField = "priv." + publicField;
    for(auto it : klass->methods){
        if(publicField == it.first) return std::pair(true, true);
        else if(privateField == it.first) return std::pair(true, false);
    }
    return std::pair(false, false);
}

std::shared_ptr<typedAST::InstGet> ASTTransformer::resolveClassFieldRead(Token name){
    if(!currentClass) return nullptr;
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass);
    if(res.first){
        auto readThis = readVar(syntheticToken("this"));
        fieldName = (res.second ? "" : "priv.") + fieldName;

        auto instGetTy = createEmptyTy();
        addTypeConstraint(instGetTy, std::make_shared<types::InstGetFieldTyConstraint>(readThis->exprType, fieldName));
        return std::make_shared<typedAST::InstGet>(readThis, fieldName, instGetTy);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(res.first){
        auto readThis = readVar(syntheticToken("this"));
        fieldName = (res.second ? "" : "priv.") + fieldName;

        auto instGetTy = createEmptyTy();
        addTypeConstraint(instGetTy, std::make_shared<types::InstGetFieldTyConstraint>(readThis->exprType, fieldName));
        return std::make_shared<typedAST::InstGet>(readThis, fieldName, instGetTy);
    }
    return nullptr;
}
std::shared_ptr<typedAST::InstSet> ASTTransformer::resolveClassFieldStore(Token name, typedAST::exprPtr toStore){
    if(!currentClass) return nullptr;
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass);
    if(res.first){
        return std::make_shared<typedAST::InstSet>(readVar(syntheticToken("this")), (res.second ? "" : "priv.") + fieldName, toStore);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(res.first){
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
    auto res = classContainsField(fieldName, currentClass);
    if(res.first){
        auto readThis = readVar(syntheticToken("this"));
        fieldName = (res.second ? "" : "priv.") + fieldName;

        auto instGetTy = createEmptyTy();
        addTypeConstraint(instGetTy, std::make_shared<types::InstGetFieldTyConstraint>(readThis->exprType, fieldName));
        return std::make_shared<typedAST::InstGet>(readThis, fieldName, instGetTy);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(res.first){
        auto readThis = readVar(syntheticToken("this"));
        fieldName = (res.second ? "" : "priv.") + fieldName;

        auto instGetTy = createEmptyTy();
        addTypeConstraint(instGetTy, std::make_shared<types::InstGetFieldTyConstraint>(readThis->exprType, fieldName));
        return std::make_shared<typedAST::InstGet>(readThis, fieldName, instGetTy);
    }
    error(name, fmt::format("Class '{}' doesn't contain this symbol", demangleName(currentClass->mangledName)));
    // Never hit
    return nullptr;
}
std::shared_ptr<typedAST::InstSet> ASTTransformer::tryResolveThis(AST::SetExpr* expr){
    if(!isLiteralThis(expr->callee)) return nullptr;
    Token _this = probeToken(expr->callee);
    Token name = probeToken(expr->field);
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass);
    if(res.first){
        auto toStore = evalASTExpr(expr->value);
        return std::make_shared<typedAST::InstSet>(readVar(syntheticToken("this")), (res.second ? "" : "priv.") + fieldName, toStore);
    }

    res = classContainsMethod(fieldName, currentClass);
    if(res.first){
        error(name, "Tried assigning to a method, which is forbidden.");
    }
    error(name, fmt::format("Class '{}' doesn't contain this symbol", demangleName(currentClass->mangledName)));
    // Never hit
    return nullptr;
}
// Misc
Token ASTTransformer::syntheticToken(string str){
    return Token(TokenType::IDENTIFIER, str);
}
void ASTTransformer::updateLine(Token token){
    current->line = token.str.line;
}
void ASTTransformer::error(Token token, const string& msg) noexcept(false){
    errorHandler::addCompileError(msg, token);
    hadError = true;
    throw TransformerException();
}
void ASTTransformer::error(const string& message) noexcept(false){
    errorHandler::addSystemError("System compile error [line " + std::to_string(current->line) + "] in '" + curUnit->file->name + "': \n" + message + "\n");
    hadError = true;
    throw TransformerException();
}

typedAST::Block ASTTransformer::parseStmtsToBlock(vector<AST::ASTNodePtr> stmts){
    typedAST::Block block;
    for(auto stmt : stmts){
        vector<typedAST::nodePtr> stmtVec;
        try {
            stmtVec = evalASTStmt(stmt);
        }catch(TransformerException e){
            //
        }
        // Dead code elimination
        // If a terminator instruction is detected in this block, don't eval anything below it
        for (int i = stmtVec.size() - 1; i >= 0; i--) {
            if (stmtVec[i]->type == typedAST::NodeType::UNCOND_JMP ||
                stmtVec[i]->type == typedAST::NodeType::RETURN) {
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
    typeEnv.emplace_back(ty, false);
    return typeEnv.size() - 1;
}

types::tyVarIdx ASTTransformer::createEmptyTy(){
    return addType(nullptr);
}
void ASTTransformer::addTypeConstraint(types::tyVarIdx ty, std::shared_ptr<types::TypeConstraint> constraint){
    typeEnv[ty].second.push_back(constraint);
}
types::tyVarIdx ASTTransformer::getBasicType(types::TypeFlag ty){
    return static_cast<int>(ty);
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
#pragma endregion