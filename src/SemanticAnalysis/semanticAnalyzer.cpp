#include "semanticAnalyzer.h"
#include <iostream>
#include "../Includes/fmt/format.h"
#include "../Codegen/upvalueFinder.h"
#include "../ErrorHandling/errorHandler.h"

using namespace SemanticAnalysis;


CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type) : enclosing(_enclosing), type(_type) {
    upvalues = std::array<Upvalue, UPVAL_MAX>();
    hasCapturedLocals = false;
    localCount = 0;
    scopeDepth = 0;
    // If a constructor or a method is being compiled, it implicitly declares "this" as the first slot
    if (!(type == FuncType::TYPE_CONSTRUCTOR || type == FuncType::TYPE_METHOD)) {
        // First slot is claimed for function name
        Local* local = &locals[localCount++];
        local->depth = 0;
        local->name = "";
    }
}

SemanticAnalyzer::SemanticAnalyzer() {
    current = new CurrentChunkInfo(nullptr, FuncType::TYPE_SCRIPT);
    currentClass = nullptr;
    curUnit = nullptr;
    curUnitIndex = 0;
    curGlobalIndex = 0;
    generateSemanticTokens = false;
}

string SemanticAnalyzer::highlight(vector<CSLModule *> &_units, CSLModule* unitToHighlight, unordered_map<string, std::unique_ptr<AST::Macro>>& macros){
    units = _units;
    for (CSLModule* unit : units) {
        if(unit == unitToHighlight) generateSemanticTokens = true;
        curUnit = unit;

        for (const auto decl : unit->topDeclarations) {
            globals.emplace_back(decl->getName(), decl->getType());
        }
        for (int i = 0; i < unit->stmts.size(); i++) {
            //doing this here so that even if a error is detected, we go on and possibly catch other(valid) errors
            try {
                unit->stmts[i]->accept(this);
            }
            catch (SemanticAnalyzerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curGlobalIndex = globals.size();
        curUnitIndex++;
    }
    for(auto& it : macros){
        createSemanticToken(it.second->name, "macro", {"declaration"});
        for(int i = 0; i < it.second->matchers.size(); i++){
            auto& matcher = it.second->matchers[i];
            auto& transcriber = it.second->transcribers[i];
            auto pattern = matcher.getPattern();
            std::unordered_set<string> params;

            for(int j = 0; j < pattern.size(); j++){
                if(pattern[j].type == TokenType::DOLLAR && j+1 < pattern.size() && pattern[++j].type == TokenType::IDENTIFIER){
                    params.insert(pattern[j].getLexeme());
                    createSemanticToken(pattern[j], "parameter", {"declaration"});
                }
            }
            for(int j = 0; j < transcriber.size(); j++){
                if(transcriber[j].type == TokenType::DOLLAR && j+1 < transcriber.size() && transcriber[++j].type == TokenType::IDENTIFIER && params.contains(transcriber[j].getLexeme())){
                    createSemanticToken(transcriber[j], "parameter", {"declaration"});
                }
            }
        }
    }
    for (CSLModule* unit : units) delete unit;
    string final = "[";
    for(auto token : semanticTokens){
        final += token.toJSON() + ",";
    }
    if(semanticTokens.size() > 0) final.pop_back();
    final += "]";
    return final;
}

string SemanticAnalyzer::generateDiagnostics(vector<CSLModule *> &_units){
    units = _units;
    vector<string> previousErrors = errorHandler::convertCompilerErrorsToJson();
    for (CSLModule *unit: units) {
        curUnit = unit;
        for (const auto decl: unit->topDeclarations) {
            globals.emplace_back(decl->getName(), decl->getType());
        }
        for (int i = 0; i < unit->stmts.size(); i++) {
            //doing this here so that even if a error is detected, we go on and possibly catch other(valid) errors
            try {
                unit->stmts[i]->accept(this);
            }
            catch (SemanticAnalyzerException e) {
                // Do nothing, only used for unwinding the stack
            }
        }
        curGlobalIndex = globals.size();
        curUnitIndex++;
    }
    for (CSLModule* unit : units) delete unit;
    string final = "[";
    for(auto& str : previousErrors) final += str + ",";
    if(diagnostics.empty() && !previousErrors.empty()) final.pop_back();
    for(auto& diagnostic : diagnostics){
        final += diagnostic.toJSON() + ",";
    }
    if(!diagnostics.empty()) final.pop_back();
    final += "]";
    return final;
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

void SemanticAnalyzer::visitAssignmentExpr(AST::AssignmentExpr* expr) {
    //rhs of the expression is on the top of the stack and stays there, since assignment is an expression
    expr->value->accept(this);
    namedVar(expr->name, true);
}

void SemanticAnalyzer::visitSetExpr(AST::SetExpr* expr) {
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        // Allows for things like object["field" + "name"]
        expr->value->accept(this);
        expr->callee->accept(this);
        expr->field->accept(this);
        return;
    }
    if(resolveThis(expr)) return;
    // The "." is always followed by a field name as a string, emitting a constant speeds things up and avoids unnecessary stack manipulation
    expr->value->accept(this);
    expr->callee->accept(this);
    createSemanticToken(probeToken(expr->field), "property");
}

void SemanticAnalyzer::visitConditionalExpr(AST::ConditionalExpr* expr) {
    //compile condition and emit a jump over then branch if the condition is false
    expr->condition->accept(this);
    expr->mhs->accept(this);
    if (expr->rhs) expr->rhs->accept(this);
}

void SemanticAnalyzer::visitRangeExpr(AST::RangeExpr *expr) {
    if(expr->start) expr->start->accept(this);
    if(expr->end) expr->end->accept(this);
}

void SemanticAnalyzer::visitBinaryExpr(AST::BinaryExpr* expr) {
    expr->left->accept(this);
    uint8_t op = 0;
    switch (expr->op.type) {
        case TokenType::INSTANCEOF:{
            getClassFromExpr(expr->right);
            return;
        }
        case TokenType::OR:
        case TokenType::AND:
        //take in double or string(in case of add)
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::SLASH:
        case TokenType::STAR:
        //for these operators, a cbers are integers, not decimals
        case TokenType::PERCENTAGE:
        case TokenType::BITSHIFT_LEFT:
        case TokenType::BITSHIFT_RIGHT:
        case TokenType::BITWISE_AND:
        case TokenType::BITWISE_OR:
        case TokenType::BITWISE_XOR:
        //these return bools and use an epsilon value when comparing
        case TokenType::EQUAL_EQUAL:
        case TokenType::BANG_EQUAL:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUAL:
        case TokenType::LESS:
        case TokenType::LESS_EQUAL:
        case TokenType::IN: break;
        default: error(expr->op, "Unrecognized token in binary expression.");
    }
    expr->right->accept(this);
}

void SemanticAnalyzer::visitUnaryExpr(AST::UnaryExpr* expr) {
    if (expr->op.type == TokenType::INCREMENT || expr->op.type == TokenType::DECREMENT) {
        int arg = -1;
        if (expr->right->type == AST::ASTType::LITERAL) {
            Token token = probeToken(expr->right);
            arg = resolveLocal(token);
            if (arg != -1) {
                createSemanticToken(token, "variable", {"local"});
            }
            else if ((arg = resolveUpvalue(current, token)) != -1)  createSemanticToken(token, "variable", {"local"});
            else if(resolveClassField(token, true) != ""){
                if(current->type == FuncType::TYPE_FUNC) error(token, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", token.getLexeme()));
                createSemanticToken(token, "property", {});
            }
            else if((arg = resolveGlobal(token, true)) != -1){
                if(arg == -2) error(token, fmt::format("Trying to access variable '{}' before it's initialized.", token.getLexeme()));
                else{
                    auto var = globals[arg];
                    string type;
                    switch(var.type){
                        case GlobalvarType::FUNCTION: type = "function"; break;
                        case GlobalvarType::CLASS: type = "class"; break;
                        case GlobalvarType::VARIABLE: type = "variable"; break;
                        default: type = "";
                    }
                    createSemanticToken(token, type, {});
                }
            }
            else error(token, fmt::format("Variable '{}' isn't declared.", token.getLexeme()));
        }
        else if (expr->right->type == AST::ASTType::FIELD_ACCESS) {
            // If a field is being incremented, compile the object, and then if it's not a dot access also compile the field
            auto left = std::static_pointer_cast<AST::FieldAccessExpr>(expr->right);
            left->callee->accept(this);

            if (left->accessor.type == TokenType::DOT) {
                // Little check to see if field access is to 'this', in which case the correct(public/private) name must be chosen
                if(isLiteralThis(left->callee)){
                    Token name = probeToken(left->field);
                    string res = resolveClassField(name, true);
                    if(res != "") {
                        createSemanticToken(name, currentClass->fields[res] ? "method" : "property", {});
                    }
                }
                else createSemanticToken(probeToken(left->field), "property", {});
            }
            else {
                left->field->accept(this);
            }
        }
        else error(expr->op, "Left side is not incrementable.");
        return;
    }
    expr->right->accept(this);
}

void SemanticAnalyzer::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    for(auto mem : expr->members){
        mem->accept(this);
    }
}

void SemanticAnalyzer::visitCallExpr(AST::CallExpr* expr) {
    // Invoking is field access + call, when the compiler recognizes this pattern it optimizes
    if (invoke(expr)) return;
    if(expr->callee->type == AST::ASTType::LITERAL){
        Token token = probeToken(expr->callee);
        createSemanticToken(token, "function");
    }else expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
}

void SemanticAnalyzer::visitNewExpr(AST::NewExpr* expr){
    getClassFromExpr(expr->call->callee);

    for (AST::ASTNodePtr arg : expr->call->args) {
        arg->accept(this);
    }
}

void SemanticAnalyzer::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    //array[index] or object["propertyAsString"]
    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        expr->callee->accept(this);
        expr->field->accept(this);
        return;
    }

    if(resolveThis(expr)) return;
    expr->callee->accept(this);
    createSemanticToken(probeToken(expr->field), "property");
}

void SemanticAnalyzer::visitStructLiteralExpr(AST::StructLiteral* expr) {
    //for each field, compile it and get the constant of the field name
    for (AST::StructEntry entry : expr->fields) {
        entry.expr->accept(this);
    }
}

void SemanticAnalyzer::visitSuperExpr(AST::SuperExpr* expr) {
    if (currentClass == nullptr) {
        error(expr->methodName, "Can't use 'super' outside of a class.");
        createSemanticToken(expr->methodName, "method");
    }
    else if (!currentClass->superclass) {
        error(expr->methodName, "Can't use 'super' in a class with no superclass.");
    }
    resolveSuperClassField(expr->methodName);
    createSemanticToken(expr->methodName, "method");
}

void SemanticAnalyzer::visitLiteralExpr(AST::LiteralExpr* expr) {
    switch (expr->token.type) {
        case TokenType::IDENTIFIER: {
            namedVar(expr->token, false);
            break;
        }
        case TokenType::THIS:{
            if (currentClass == nullptr) error(expr->token, "Can't use keyword 'this' outside of a class.");
            return;
        }
    }
}

void SemanticAnalyzer::visitFuncLiteral(AST::FuncLiteral* expr) {
    //creating a new compilerInfo sets us up with a clean slate for writing bytecode, the enclosing functions info
    //is stored in parserCurrent->enclosing
    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC);
    //no need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    //we define the args as locals, when the function is called, the args will be sitting on the stack in order
    //we just assign those positions to each arg
    for (AST::ASTVar& var : expr->args) {
        declareLocalVar(var, true);
        defineLocalVar();
    }
    for(auto stmt : expr->body->statements){
        try {
            stmt->accept(this);
        }catch(SemanticAnalyzerException e){

        }
    }
    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
}

void SemanticAnalyzer::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    resolveModuleVariable(expr->moduleName, expr->ident);
}

void SemanticAnalyzer::visitMacroExpr(AST::MacroExpr* expr) {
    createSemanticToken(expr->macroName, "macro");
}

void SemanticAnalyzer::visitAsyncExpr(AST::AsyncExpr* expr) {
    if (invoke(expr)) return;
    if(expr->callee->type == AST::ASTType::LITERAL){
        Token token = probeToken(expr->callee);
        createSemanticToken(token, "function");
    }else expr->callee->accept(this);
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
}

void SemanticAnalyzer::visitAwaitExpr(AST::AwaitExpr* expr) {
    expr->expr->accept(this);
}


void SemanticAnalyzer::visitVarDecl(AST::VarDecl* decl) {
    uint16_t global;
    if(current->scopeDepth == 0){
        global = declareGlobalVar(decl->var.name);
    }else declareLocalVar(decl->var, false);
    // Compile the right side of the declaration, if there is no right side, the variable is initialized as nil
    if (decl->value != nullptr) decl->value->accept(this);
    if(current->scopeDepth == 0){
        defineGlobalVar(global, decl->var.name);
    }else defineLocalVar();
}

void SemanticAnalyzer::visitFuncDecl(AST::FuncDecl* decl) {
    uint16_t index = declareGlobalVar(decl->getName());
    // Defining the function here to allow for recursion
    defineGlobalVar(index, decl->getName());
    //creating a new compilerInfo sets us up with a clean slate for writing bytecode, the enclosing functions info
    //is stored in parserCurrent->enclosing
    current = new CurrentChunkInfo(current, FuncType::TYPE_FUNC);
    //no need for a endScope, since returning from the function discards the entire callstack
    beginScope();
    //we define the args as locals, when the function is called, the args will be sitting on the stack in order
    //we just assign those positions to each arg
    for (AST::ASTVar& var : decl->args) {
        declareLocalVar(var, true);
        defineLocalVar();
    }
    for(auto stmt : decl->body->statements){
        try {
            stmt->accept(this);
        }catch(SemanticAnalyzerException e){

        }
    }
    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
}

void SemanticAnalyzer::visitClassDecl(AST::ClassDecl* decl) {
    Token className = decl->getName();
    uInt16 index = declareGlobalVar(className);

    currentClass = std::make_shared<ClassChunkInfo>(className);

    if (decl->inheritedClass) {
        // getClassFromExpr returns the class chunk for this
        // Allows semantic analyzer to determine which variables are class properties
        // Use try block since this shouldn't throw out of the whole class definition
        try {
            auto superclass = getClassFromExpr(decl->inheritedClass);
            if (superclass) currentClass->fields = superclass->fields;
            currentClass->superclass = superclass;
        }catch(SemanticAnalyzerException& e){

        }
    }

    // Put field and method names into the class
    for(auto& field : decl->fields){
        currentClass->fields.insert_or_assign((field.isPublic ? "" : "!") + field.field.getLexeme(), false);
        createSemanticToken(field.field, "property");
    }
    // First put the method names into the class, then compiled the methods later to be able to correctly detect the methods when
    // resolveClassField is called
    for(auto& method : decl->methods){
        currentClass->fields.insert_or_assign((method.isPublic ? "" : "!") + method.method->getName().getLexeme(), true);
        createSemanticToken(method.method->getName(), "method");
    }

    // Define the class here, so that it can be called inside its own methods
    // Defining after inheriting so that a class can't be used as its own parent
    defineGlobalVar(index, className);
    // Assigning at compile time to save on bytecode, also to get access to the class in getClassFromExpr
    globals[index].ptr = currentClass;

    for (auto& _method : decl->methods) {
        //At this point the name is guaranteed to exist as a string, so createStr just returns the already created string
        method(_method.method.get(), className);
    }
    currentClass = nullptr;
}


void SemanticAnalyzer::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
}

void SemanticAnalyzer::visitBlockStmt(AST::BlockStmt* stmt) {
    beginScope();
    for (AST::ASTNodePtr node : stmt->statements) {
        try {
            node->accept(this);
        }catch(SemanticAnalyzerException e){

        }
    }
    endScope();
}

void SemanticAnalyzer::visitIfStmt(AST::IfStmt* stmt) {
    stmt->condition->accept(this);
    stmt->thenBranch->accept(this);
    if (stmt->elseBranch != nullptr) stmt->elseBranch->accept(this);

}

void SemanticAnalyzer::visitWhileStmt(AST::WhileStmt* stmt) {
    stmt->condition->accept(this);
    beginScope();
    stmt->body->accept(this);
    endScope();
}

void SemanticAnalyzer::visitForStmt(AST::ForStmt* stmt) {
    // Wrap this in a scope so if there is a var declaration in the initialization it's scoped to the loop
    beginScope();
    if (stmt->init != nullptr) stmt->init->accept(this);
    if (stmt->condition != nullptr) stmt->condition->accept(this);
    beginScope();
    stmt->body->accept(this);
    endScope();
    if (stmt->increment != nullptr) stmt->increment->accept(this);
    endScope();
}

void SemanticAnalyzer::visitBreakStmt(AST::BreakStmt* stmt) {}

void SemanticAnalyzer::visitContinueStmt(AST::ContinueStmt* stmt) {}

void SemanticAnalyzer::visitSwitchStmt(AST::SwitchStmt* stmt) {
    //compile the expression in parentheses
    stmt->expr->accept(this);
    for (const std::shared_ptr<AST::CaseStmt>& _case : stmt->cases) {
        for (const Token& constant : _case->constants) {
            switch (constant.type) {
                case TokenType::NUMBER:
                case TokenType::TRUE:
                case TokenType::FALSE:
                case TokenType::NIL:
                case TokenType::STRING: break;
                default: {
                    error(constant, "Case expression can only be a constant.");
                }
            }
        }
        beginScope();
        _case->accept(this);
        endScope();
    }
}

void SemanticAnalyzer::visitCaseStmt(AST::CaseStmt* stmt) {
    //compile every statement in the case
    for (AST::ASTNodePtr caseStmt : stmt->stmts) {
        try {
            caseStmt->accept(this);
        }catch(SemanticAnalyzerException e){

        }
    }
}

void SemanticAnalyzer::visitAdvanceStmt(AST::AdvanceStmt* stmt) {}

void SemanticAnalyzer::visitReturnStmt(AST::ReturnStmt* stmt) {
    if (current->type == FuncType::TYPE_SCRIPT) {
        error(stmt->keyword, "Can't return from top-level code.");
    }
    else if (current->type == FuncType::TYPE_CONSTRUCTOR) {
        error(stmt->keyword, "Can't return a value from a constructor.");
    }
    stmt->expr->accept(this);
}

#pragma region helpers

#pragma region Variables

void SemanticAnalyzer::defineGlobalVar(uInt16 index, Token token) {
    string _type;
    switch(globals[index].type){
        case GlobalvarType::VARIABLE: _type = "variable"; break;
        case GlobalvarType::CLASS: _type = "class"; break;
        case GlobalvarType::FUNCTION: _type = "function"; break;
        default: _type = "";
    }
    createSemanticToken(token, _type, {"declaration"});
}

void SemanticAnalyzer::namedVar(Token token, bool canAssign) {
    string type;
    vector<string> modifiers;
    int arg = resolveLocal(token);
    if (arg != -1) {
        type = current->locals[arg].isFunctionParameter ? "parameter" : "variable";
        if(!current->locals[arg].isFunctionParameter) modifiers.push_back("local");
    }
    else if ((arg = resolveUpvalue(current, token)) != -1) {
        type = "variable";
        modifiers.push_back("local");
    }
    else if(resolveClassField(token, canAssign) != ""){
        if(current->type == FuncType::TYPE_FUNC){
            error(token, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", token.getLexeme()));
        }
        type = currentClass->fields[resolveClassField(token, canAssign)] ? "method" : "property";
    }
    else{
        type = "variable";
    }
    createSemanticToken(token, type, modifiers);
}

uint16_t SemanticAnalyzer::declareGlobalVar(Token name) {
    // Searches for the index of the global variable in the current file
    // (globals.size() - curGlobalIndex = globals declared in current file)
    int index = curGlobalIndex;
    for (int i = curGlobalIndex; i < globals.size(); i++) {
        if (name.getLexeme() == globals[i].name.getLexeme()) return i;
    }
    // Should never be hit, but here just in case
    error(name, "Couldn't find variable.");
    return 0;
}

void SemanticAnalyzer::declareLocalVar(AST::ASTVar& var, bool isParam) {
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
    addLocal(var, isParam);
}

void SemanticAnalyzer::addLocal(AST::ASTVar var, bool isParam) {
    if (current->localCount == LOCAL_MAX) {
        error(var.name, "Too many local variables in function.");
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name = var.name.getLexeme();
    local->depth = -1;
    local->isLocalUpvalue = false;
    local->isFunctionParameter = isParam;
    createSemanticToken(var.name, isParam ? "parameter" : "variable", {"declaration", "local"});
}

void SemanticAnalyzer::beginScope() {
    current->scopeDepth++;
}

void SemanticAnalyzer::endScope() {
    //Pop every variable that was declared in this scope
    current->scopeDepth--;//first lower the scope, the check for every var that is deeper than the parserCurrent scope
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        current->localCount--;
    }
}

int SemanticAnalyzer::resolveLocal(CurrentChunkInfo* func, Token name) {
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

int SemanticAnalyzer::resolveLocal(Token name) {
    return resolveLocal(current, name);
}

// returns dummy value since it's never used, only important thing is to return -1 on failure
int SemanticAnalyzer::resolveUpvalue(CurrentChunkInfo* func, Token name) {
    if (func->enclosing == nullptr) return -1;

    int local = resolveLocal(func->enclosing, name);
    if (local != -1) {
        func->enclosing->hasCapturedLocals = true;
        return 0;
    }
    int upvalue = resolveUpvalue(func->enclosing, name);
    if (upvalue != -1) {
        return 0;
    }

    return -1;
}

// Marks the local at the top of the stack as ready to use
void SemanticAnalyzer::defineLocalVar() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

#pragma endregion

#pragma region Classes and methods
void SemanticAnalyzer::method(AST::FuncDecl* _method, Token className) {
    FuncType type = FuncType::TYPE_METHOD;
    // Constructors are treated separately, but are still methods
    if (_method->getName().equals(className)) type = FuncType::TYPE_CONSTRUCTOR;
    // "this" gets implicitly defined as the first local in methods and the constructor
    current = new CurrentChunkInfo(current, type);
    beginScope();
    // Args defined as local in order they were passed to the function
    for (AST::ASTVar& var : _method->args) {
        declareLocalVar(var, true);
        defineLocalVar();
    }
    for(auto stmt : _method->body->statements){
        try {
            stmt->accept(this);
        }catch(SemanticAnalyzerException e){

        }
    }

    CurrentChunkInfo* temp = current->enclosing;
    delete current;
    current = temp;
}

bool SemanticAnalyzer::invoke(AST::CallExpr* expr) {
    if (expr->callee->type == AST::ASTType::FIELD_ACCESS) {
        //currently we only optimizes field invoking(struct.field() or array[field]())
        auto call = std::static_pointer_cast<AST::FieldAccessExpr>(expr->callee);
        if (call->accessor.type == TokenType::LEFT_BRACKET) return false;

        call->callee->accept(this);
        uint16_t constant;
        Token name = probeToken(call->field);
        // If invoking a method inside another method, make sure to get the right(public/private) name
        if (isLiteralThis(call->callee)) {
            string res = resolveClassField(name, false);
            if (res == "") error(name, fmt::format("Field {}, doesn't exist in class {}.", name.getLexeme(),
                                                   currentClass->name.getLexeme()));
            createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
        } else createSemanticToken(name, "method");

        for (AST::ASTNodePtr arg: expr->args) {
            arg->accept(this);
        }
        return true;
    }
    else if (expr->callee->type == AST::ASTType::SUPER) {
        auto superCall = std::static_pointer_cast<AST::SuperExpr>(expr->callee);
        if (currentClass == nullptr) {
            error(superCall->methodName, "Can't use 'super' outside of a class.");
            createSemanticToken(superCall->methodName, "method");
        }
        else if (!currentClass->superclass) {
            error(superCall->methodName, "Can't use 'super' in a class with no superclass.");
        }
        resolveSuperClassField(superCall->methodName);
        createSemanticToken(superCall->methodName, "method");

        if (currentClass == nullptr) {
            error(superCall->methodName, "Can't use 'super' outside of a class.");
        }
        else if (!currentClass->superclass) {
            error(superCall->methodName, "Can't use 'super' in a class with no superclass.");
        }
        for (AST::ASTNodePtr arg : expr->args) {
            arg->accept(this);
        }
        return true;
    }
    // Class methods can be accessed without 'this' keyword inside of methods and called
    return resolveImplicitObjectField(expr);
}

bool SemanticAnalyzer::invoke(AST::AsyncExpr* expr) {
    if (expr->callee->type == AST::ASTType::FIELD_ACCESS) {
        //currently we only optimizes field invoking(struct.field() or array[field]())
        auto call = std::static_pointer_cast<AST::FieldAccessExpr>(expr->callee);
        if (call->accessor.type == TokenType::LEFT_BRACKET) return false;

        call->callee->accept(this);
        uint16_t constant;
        Token name = probeToken(call->field);
        // If invoking a method inside another method, make sure to get the right(public/private) name
        if (isLiteralThis(call->callee)) {
            string res = resolveClassField(name, false);
            if (res == "") error(name, fmt::format("Field {}, doesn't exist in class {}.", name.getLexeme(),
                                                   currentClass->name.getLexeme()));
            createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
        } else createSemanticToken(name, "method");

        for (AST::ASTNodePtr arg: expr->args) {
            arg->accept(this);
        }
        return true;
    }
    else if (expr->callee->type == AST::ASTType::SUPER) {
        auto superCall = std::static_pointer_cast<AST::SuperExpr>(expr->callee);
        if (currentClass == nullptr) {
            error(superCall->methodName, "Can't use 'super' outside of a class.");
            createSemanticToken(superCall->methodName, "method");
        }
        else if (!currentClass->superclass) {
            error(superCall->methodName, "Can't use 'super' in a class with no superclass.");
        }
        resolveSuperClassField(superCall->methodName);
        createSemanticToken(superCall->methodName, "method");

        if (currentClass == nullptr) {
            error(superCall->methodName, "Can't use 'super' outside of a class.");
        }
        else if (!currentClass->superclass) {
            error(superCall->methodName, "Can't use 'super' in a class with no superclass.");
        }
        for (AST::ASTNodePtr arg : expr->args) {
            arg->accept(this);
        }
        return true;
    }
    // Class methods can be accessed without 'this' keyword inside of methods and called
    return resolveImplicitObjectField(expr);
}

// Turns a hash map lookup into an array linear search, but still faster than allocating memory using ObjString::createStr
// First bool in pair is if the search was successful, second is if the field found was public or private
static std::pair<bool, bool> classContainsField(string& publicField, std::unordered_map<string, bool>& map){
    string privateField = "!" + publicField;
    for(auto it : map){
        if(publicField == it.first && !it.second) return std::pair(true, true);
        else if(privateField == it.first && !it.second) return std::pair(true, false);
    }
    return std::pair(false, false);
}
static std::pair<bool, bool> classContainsMethod(string& publicField, std::unordered_map<string, bool>& map){
    string privateField = "!" + publicField;
    if(map.contains(publicField) && map[publicField]) return std::pair(true, true);
    else if(map.contains(privateField) && map[privateField]) return std::pair(true, false);
    return std::pair(false, false);
}

string SemanticAnalyzer::resolveClassField(Token name, bool canAssign){
    if(!currentClass) return "";
    string fieldName = name.getLexeme();
    auto res = classContainsField(fieldName, currentClass->fields);
    if(res.first){
        return (res.second ? "" : "!") + fieldName;
    }

    res = classContainsMethod(fieldName, currentClass->fields);
    if(res.first){
        if(canAssign) error(name, "Tried assigning to a method, which is forbidden.");
        return (res.second ? "" : "!") + fieldName;
    }
    return "";
}

// Only called if a superclass exists
void SemanticAnalyzer::resolveSuperClassField(Token name){
    string fieldName = name.getLexeme();
    auto res = classContainsMethod(fieldName, currentClass->superclass->fields);
    if(!res.first){
        error(name, fmt::format("Superclass '{}' doesn't contain method '{}'.", currentClass->superclass->name.getLexeme(), name.getLexeme()));
    }
}

// Makes sure the correct prefix is used when accessing private fields
// this.private_field -> this.!private_field
bool SemanticAnalyzer::resolveThis(AST::SetExpr *expr) {
    if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    string res = resolveClassField(name, true);
    if(res == "") {
        error(name, fmt::format("Field '{}' doesn't exist in class '{}'.", name.getLexeme(), currentClass->name.getLexeme()));
        return true;
    }
    createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
    expr->value->accept(this);
    return true;
}
bool SemanticAnalyzer::resolveThis(AST::FieldAccessExpr *expr) {
    if(!isLiteralThis(expr->callee)) return false;
    Token name = probeToken(expr->field);
    string res = resolveClassField(name, false);
    if(res == "") {
        error(name, fmt::format("Field {} doesn't exist in class '{}'.", name.getLexeme(), currentClass->name.getLexeme()));
        return true;
    }
    createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
    return true;
}
// Recognizes object_field() as an invoke operation
// If object_field() is encountered inside a closure which is inside a method, throw an error since only this.object_field() is allowed in closures
bool SemanticAnalyzer::resolveImplicitObjectField(AST::CallExpr *expr) {
    if(expr->callee->type != AST::ASTType::LITERAL) return false;
    Token name = probeToken(expr->callee);
    string res = resolveClassField(name, false);
    if(res == "") return false;
    if(current->type == FuncType::TYPE_FUNC) error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
    createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }

    return true;
}

bool SemanticAnalyzer::resolveImplicitObjectField(AST::AsyncExpr *expr) {
    if(expr->callee->type != AST::ASTType::LITERAL) return false;
    Token name = probeToken(expr->callee);
    string res = resolveClassField(name, false);
    if(res == "") return false;
    if(current->type == FuncType::TYPE_FUNC) error(name, fmt::format("Cannot access fields without 'this' within a closure, use this.{}", name.getLexeme()));
    createSemanticToken(name, currentClass->fields[res] ? "method" : "property");
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }

    return true;
}

std::shared_ptr<ClassChunkInfo> SemanticAnalyzer::getClassFromExpr(AST::ASTNodePtr expr){
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
        createSemanticToken(token, "class");
    }
    else {
        auto moduleExpr = std::static_pointer_cast<AST::ModuleAccessExpr>(expr);
        classIndex = resolveModuleVariable(moduleExpr->moduleName, moduleExpr->ident);
        token = moduleExpr->ident;
    }
    if(globals[classIndex].type != GlobalvarType::CLASS) error(token, "Variable isn't a class.");
    return globals[classIndex].ptr;
}
#pragma endregion

SemanticAnalyzerException SemanticAnalyzer::error(Token token, const string& msg) noexcept(false) {
    diagnostics.emplace_back(token, msg, 0);
    throw SemanticAnalyzerException();
}

// For every dependency that's imported without an alias, check if any of its exports match 'symbol', return -1 if not
int SemanticAnalyzer::checkSymbol(Token symbol) {
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
                string _type;
                switch(decls[i]->getType()){
                    case AST::ASTDeclType::VAR: _type = "variable"; break;
                    case AST::ASTDeclType::FUNCTION: _type = "function"; break;
                    case AST::ASTDeclType::CLASS: _type = "class"; break;
                }
                createSemanticToken(symbol, _type, {"readonly"});
                return globalIndex + i;
            }
        }
    }
    return -1;
}

int SemanticAnalyzer::resolveGlobal(Token symbol, bool canAssign) {
    bool inThisFile = false;
    int index = curGlobalIndex;
    std::shared_ptr<AST::ASTDecl> ptr = nullptr;
    for (auto decl : curUnit->topDeclarations) {
        if (symbol.equals(decl->getName())) {
            // It's an error to read from a variable during its initialization
            if(globals[index].isDefined) return -2;
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

            createSemanticToken(symbol, "variable", {"readonly"});
            return index;
        }
        throw error(symbol, "Cannot assign to a variable not declared in this module.");
    }
    else {
        if (inThisFile) {
            string _type;
            switch(ptr->getType()){
                case AST::ASTDeclType::VAR: _type = "variable"; break;
                case AST::ASTDeclType::FUNCTION: _type = "function"; break;
                case AST::ASTDeclType::CLASS: _type = "class"; break;
            }
            createSemanticToken(symbol, _type, {"readonly"});
            return index;
        }
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
uint32_t SemanticAnalyzer::resolveModuleVariable(Token moduleAlias, Token variable) {
    //first find the module with the correct alias
    Dependency* depPtr = nullptr;
    for (Dependency dep : curUnit->deps) {
        if (dep.alias.equals(moduleAlias)) {
            depPtr = &dep;
            break;
        }
    }
    if (depPtr == nullptr) {
        throw error(moduleAlias, "Module alias doesn't exist.");
    }

    createSemanticToken(moduleAlias, "namespace");

    CSLModule* unit = depPtr->module;
    int index = 0;
    for (auto& i : units) {
        if (i != unit) {
            index += i->topDeclarations.size();
            continue;
        }
        // Checks every export of said module against 'variable'
        for (auto decl : unit->exports) {
            if (decl->getName().equals(variable)) {
                string _type;
                switch(decl->getType()){
                    case AST::ASTDeclType::VAR: _type = "variable"; break;
                    case AST::ASTDeclType::FUNCTION: _type = "function"; break;
                    case AST::ASTDeclType::CLASS: _type = "class"; break;
                }
                createSemanticToken(variable, _type, {"readonly"});
                return index;
            }
            index++;
        }
    }

    throw error(variable, fmt::format("Module {} doesn't export this symbol.", depPtr->alias.getLexeme()));
    // Never hit
    return -1;
}

void SemanticAnalyzer::createSemanticToken(Token token, string type, std::vector<string> modifiers){
    if(!generateSemanticTokens || token.isSynthetic) return;
    semanticTokens.emplace_back(token, type, modifiers);
}
#pragma endregion

// Only used when debugging _LONG versions of op codes
#undef SHORT_CONSTANT_LIMIT

#undef CHECK_SCOPE_FOR_LOOP
#undef CHECK_SCOPE_FOR_SWITCH