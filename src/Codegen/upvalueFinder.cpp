#include "upvalueFinder.h"
#include <unordered_set>
#include <iostream>
#include "../Includes/fmt/format.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Codegen/valueHelpersInline.cpp"

using namespace upvalueFinder;
using namespace object;
using namespace valueHelpers;


CurrentChunkInfo::CurrentChunkInfo(CurrentChunkInfo* _enclosing){
    upvalues = std::array<Upvalue, 256>();
    localCount = 0;
    upvalCount = 0;
    scopeDepth = 0;
    enclosing = _enclosing;
}

UpvalueFinder::UpvalueFinder(vector<ESLModule*>& _units) {
    current = new CurrentChunkInfo(nullptr);

    for (ESLModule* unit : _units) {
        for (int i = 0; i < unit->stmts.size(); i++) {
            unit->stmts[i]->accept(this);
        }
    }
    delete current;
}

void UpvalueFinder::visitAssignmentExpr(AST::AssignmentExpr* expr) {
    // First check rhs, then the variable that is being assigned to
    expr->value->accept(this);
    namedVar(expr->name, true);
}

void UpvalueFinder::visitSetExpr(AST::SetExpr* expr) {
    switch (expr->accessor.type) {
        case TokenType::LEFT_BRACKET: {
            expr->value->accept(this);
            expr->callee->accept(this);
            expr->field->accept(this);
            break;
        }
        case TokenType::DOT: {
            //the "." is always followed by a field name as a string, no need to look through it
            expr->value->accept(this);
            expr->callee->accept(this);
            break;
        }
    }
}

void UpvalueFinder::visitConditionalExpr(AST::ConditionalExpr* expr) {
    expr->condition->accept(this);
    expr->mhs->accept(this);
    expr->rhs->accept(this);
}

void UpvalueFinder::visitRangeExpr(AST::RangeExpr *expr) {
    if(expr->start) expr->start->accept(this);
    if(expr->end) expr->end->accept(this);
}

void UpvalueFinder::visitBinaryExpr(AST::BinaryExpr* expr) {
    expr->left->accept(this);
    expr->right->accept(this);
}

void UpvalueFinder::visitUnaryExpr(AST::UnaryExpr* expr) {
    expr->right->accept(this);
}

void UpvalueFinder::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    for(auto mem : expr->members){
        mem->accept(this);
    }
}

void UpvalueFinder::visitCallExpr(AST::CallExpr* expr) {
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
}

void UpvalueFinder::visitNewExpr(AST::NewExpr* expr){
    // Expr->call will always be a call
    for (AST::ASTNodePtr arg : reinterpret_cast<AST::CallExpr*>(expr->call.get())->args) {
        arg->accept(this);
    }
}

void UpvalueFinder::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    expr->callee->accept(this);

    if(expr->accessor.type == TokenType::LEFT_BRACKET){
        expr->field->accept(this);
    }
}

void UpvalueFinder::visitStructLiteralExpr(AST::StructLiteral* expr) {
    for (AST::StructEntry entry : expr->fields) {
        entry.expr->accept(this);
    }
}

void UpvalueFinder::visitSuperExpr(AST::SuperExpr* expr) {
    namedVar(Token(TokenType::IDENTIFIER, "this"), false);
}

void UpvalueFinder::visitLiteralExpr(AST::LiteralExpr* expr) {
    switch (expr->token.type) {
        case TokenType::THIS: {
            //'this' gets implicitly defined by the compiler
            namedVar(expr->token, false);
            break;
        }
        case TokenType::IDENTIFIER: {
            namedVar(expr->token, false);
            break;
        }
    }
}

void UpvalueFinder::visitFuncLiteral(AST::FuncLiteral* expr) {
    current = new CurrentChunkInfo(current);
    //no need for a endScope, since returning from the function discards the entire CurrentChunkInfo
    beginScope();
    // Args defined as locals, if some closure accesses them, they are turned into an upvalue
    for (AST::ASTVar& var : expr->args) {
        declareLocalVar(var);
    }
    for(auto stmt : expr->body->statements){
        stmt->accept(this);
    }
    auto temp = current->enclosing;
    delete current;
    current = temp;
}

void UpvalueFinder::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {}

// This shouldn't ever be visited as every macro should be expanded before compilation
void UpvalueFinder::visitMacroExpr(AST::MacroExpr* expr) {}

void UpvalueFinder::visitAsyncExpr(AST::AsyncExpr* expr) {
    expr->callee->accept(this);
    for (AST::ASTNodePtr arg : expr->args) {
        arg->accept(this);
    }
}

void UpvalueFinder::visitAwaitExpr(AST::AwaitExpr* expr) {
    expr->expr->accept(this);
}

void UpvalueFinder::visitVarDecl(AST::VarDecl* decl) {
    if(current->scopeDepth == 0){
        declareGlobalVar(decl->var);
    }else declareLocalVar(decl->var);
    if (decl->value != nullptr) {
        decl->value->accept(this);
    }
}

void UpvalueFinder::visitFuncDecl(AST::FuncDecl* decl) {
    //Function name is always going to be global, no need to define it
    current = new CurrentChunkInfo(current);
    //no need for a endScope, since returning from the function discards CurrentChunkInfo
    beginScope();
    // Args defined as locals, if some closure accesses them, they are turned into an upvalue
    for (AST::ASTVar& var : decl->args) {
        declareLocalVar(var);
    }
    for(auto stmt : decl->body->statements){
        stmt->accept(this);
    }
    auto temp = current->enclosing;
    delete current;
    current = temp;
}

void UpvalueFinder::visitClassDecl(AST::ClassDecl* decl) {
    // Class name is always going to be a global
    for (auto& _method : decl->methods) {
        _method.method->accept(this);
    }
}

void UpvalueFinder::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
}

void UpvalueFinder::visitBlockStmt(AST::BlockStmt* stmt) {
    beginScope();
    for (AST::ASTNodePtr node : stmt->statements) {
        node->accept(this);
    }
    endScope();
}

void UpvalueFinder::visitIfStmt(AST::IfStmt* stmt) {
    stmt->condition->accept(this);
    stmt->thenBranch->accept(this);
    // Else branch is optional
    if (stmt->elseBranch) stmt->elseBranch->accept(this);

}

void UpvalueFinder::visitWhileStmt(AST::WhileStmt* stmt) {
    stmt->condition->accept(this);
    beginScope();
    stmt->body->accept(this);
    endScope();
}

void UpvalueFinder::visitForStmt(AST::ForStmt* stmt) {
    // Wrap this in a scope so if there is a var declaration in the initialization it's scoped to the loop
    beginScope();
    if (stmt->init != nullptr) stmt->init->accept(this);
    if (stmt->condition != nullptr) {
        stmt->condition->accept(this);
    }
    stmt->body->accept(this);
    if(stmt->increment != nullptr) stmt->increment->accept(this);
    endScope();
}

void UpvalueFinder::visitBreakStmt(AST::BreakStmt* stmt) {}

void UpvalueFinder::visitContinueStmt(AST::ContinueStmt* stmt) {}

void UpvalueFinder::visitSwitchStmt(AST::SwitchStmt* stmt) {
    stmt->expr->accept(this);
    for (const std::shared_ptr<AST::CaseStmt>& _case : stmt->cases) {
        beginScope();
        _case->accept(this);
        endScope();
    }
}

void UpvalueFinder::visitCaseStmt(AST::CaseStmt* stmt) {
    for (AST::ASTNodePtr caseStmt : stmt->stmts) {
        caseStmt->accept(this);
    }
}

void UpvalueFinder::visitAdvanceStmt(AST::AdvanceStmt* stmt) {}

void UpvalueFinder::visitReturnStmt(AST::ReturnStmt* stmt) {
    if (stmt->expr != nullptr) stmt->expr->accept(this);
}

#pragma region helpers

#pragma region Variables

// If a local is found to be accessed by a closure, it's turned into a local upvalue
void UpvalueFinder::namedVar(Token token, bool canAssign) {
    int arg = resolveLocal(token);
    if(arg == -1) resolveUpvalue(current, token);
}

void UpvalueFinder::declareGlobalVar(AST::ASTVar& var) {
    var.type = AST::ASTVarType::GLOBAL;
}

// Makes sure the compiler is aware that a stack slot is occupied by this local variable
void UpvalueFinder::declareLocalVar(AST::ASTVar& var) {
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }
        string str = var.name.getLexeme();
        if (str.compare(local->var->name.getLexeme()) == 0) {

        }
    }
    addLocal(var);
}

void UpvalueFinder::addLocal(AST::ASTVar& var) {
    if (current->localCount == 256) {
        return;
    }
    var.type = AST::ASTVarType::LOCAL;
    Local* local = &current->locals[current->localCount++];
    local->var = &var;
    local->depth = -1;
}

void UpvalueFinder::beginScope() {
    current->scopeDepth++;
}

void UpvalueFinder::endScope() {
    current->scopeDepth--;//first lower the scope, the check for every var that is deeper than the current scope
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        current->localCount--;
    }
}

int UpvalueFinder::resolveLocal(CurrentChunkInfo* func, Token name) {
    //checks to see if there is a local variable with a provided name, if there is return the index of the stack slot of the var
    for (int i = func->localCount - 1; i >= 0; i--) {
        Local* local = &func->locals[i];
        string str = name.getLexeme();
        if (str.compare(local->var->name.getLexeme()) == 0) {
            return i;
        }
    }

    return -1;
}

int UpvalueFinder::resolveLocal(Token name) {
    return resolveLocal(current, name);
}

int UpvalueFinder::resolveUpvalue(CurrentChunkInfo* func, Token name) {
    if (func->enclosing == nullptr) return -1;

    int local = resolveLocal(func->enclosing, name);
    if (local != -1) {
        return addUpvalue(func, (uint8_t)local, true);
    }
    int upvalue = resolveUpvalue(func->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(func, (uint8_t)upvalue, false);
    }

    return -1;
}

int UpvalueFinder::addUpvalue(CurrentChunkInfo* func, byte index, bool isLocal) {
    int upvalueCount = func->upvalCount;
    // First check if this upvalue has already been captured
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upval = &func->upvalues[i];
        if (upval->index == index && upval->isLocal == isLocal) {
            return i;
        }
    }
    if (upvalueCount == 256) return -1;
    func->upvalues[upvalueCount].isLocal = isLocal;
    func->upvalues[upvalueCount].index = index;
    if(isLocal) func->enclosing->locals[index].var->type = AST::ASTVarType::LOCAL_UPVALUE;
    return func->upvalCount++;
}

#pragma endregion
#pragma endregion
