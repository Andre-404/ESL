#include "ASTOptimization.h"
#include "../../ErrorHandling/errorHandler.h"
#include "../../Includes/fmt/format.h"
#include <unordered_map>

using namespace AST;

ASTOptimizer::ASTOptimizer(errorHandler::ErrorHandler& errHandler) : errHandler(errHandler) {

}

void ASTOptimizer::process(vector<ASTModule> &units) {
    for(ASTModule& module : units){
        for(auto stmt : module.stmts){
            stmt->accept(this);
        }
    }
}

static bool stmtIsTerminator(const ASTNodePtr stmt){
    return stmt->type == ASTType::BREAK || stmt->type == ASTType::CONTINUE || stmt->type == ASTType::ADVANCE || stmt->type == ASTType::RETURN;
}

void ASTOptimizer::visitAssignmentExpr(AST::AssignmentExpr* expr) {
    expr->value->accept(this);
}
void ASTOptimizer::visitSetExpr(AST::SetExpr* expr){
    expr->callee->accept(this);
    expr->field->accept(this);
    expr->value->accept(this);
}
void ASTOptimizer::visitConditionalExpr(AST::ConditionalExpr* expr) {
    expr->condition->accept(this);
    expr->mhs->accept(this);
    expr->rhs->accept(this);
}
void ASTOptimizer::visitBinaryExpr(AST::BinaryExpr* expr){
    expr->left->accept(this);
    expr->right->accept(this);
}
void ASTOptimizer::visitUnaryExpr(AST::UnaryExpr* expr){
    expr->right->accept(this);
}
void ASTOptimizer::visitCallExpr(AST::CallExpr* expr) {
    expr->callee->accept(this);
    for(auto arg : expr->args) arg->accept(this);
}
void ASTOptimizer::visitNewExpr(AST::NewExpr* expr) {
    expr->call->accept(this);
}
void ASTOptimizer::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    expr->callee->accept(this);
    expr->field->accept(this);
}
void ASTOptimizer::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    for(auto _expr : expr->members) _expr->accept(this);
}
void ASTOptimizer::visitStructLiteralExpr(AST::StructLiteral* expr) {
    for(StructEntry& pair : expr->fields) pair.expr->accept(this);
}
void ASTOptimizer::visitLiteralExpr(AST::LiteralExpr* expr) {
    // Nothing
}
void ASTOptimizer::visitFuncLiteral(AST::FuncLiteral* expr) {
    expr->body->accept(this);
}
void ASTOptimizer::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    // Nothing
}
void ASTOptimizer::visitMacroExpr(AST::MacroExpr* expr) {
    // Nothing
}

void ASTOptimizer::visitVarDecl(AST::VarDecl* decl) {
    if(decl->value) decl->value->accept(this);
}
void ASTOptimizer::visitFuncDecl(AST::FuncDecl* decl) {;
    decl->body->accept(this);
}

void ASTOptimizer::visitClassDecl(AST::ClassDecl* decl) {
    for(auto method : decl->methods) {
        method.method->accept(this);
    }
}

void ASTOptimizer::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
}
void ASTOptimizer::visitSpawnStmt(AST::SpawnStmt* stmt) {
    stmt->callExpr->accept(this);
}
void ASTOptimizer::visitBlockStmt(AST::BlockStmt* stmt) {
    for(int i = 0; i < stmt->statements.size(); i++){
        ASTNodePtr _stmt = stmt->statements[i];
        if(stmtIsTerminator(_stmt) && i != stmt->statements.size()-1){
            // TODO: warning
            stmt->statements.resize(i+1);
        }
        _stmt->accept(this);
    }
}
void ASTOptimizer::visitIfStmt(AST::IfStmt* stmt) {
    stmt->condition->accept(this);
    stmt->thenBranch->accept(this);
    if(stmt->elseBranch) stmt->elseBranch->accept(this);
}
void ASTOptimizer::visitWhileStmt(AST::WhileStmt* stmt) {
    stmt->condition->accept(this);
    stmt->body->accept(this);
}
void ASTOptimizer::visitForStmt(AST::ForStmt* stmt) {
    if(stmt->init) stmt->init->accept(this);
    if(stmt->condition) stmt->condition->accept(this);
    if(stmt->increment) stmt->increment->accept(this);
    stmt->body->accept(this);
}
void ASTOptimizer::visitBreakStmt(AST::BreakStmt* stmt) {
    // Nothing
}
void ASTOptimizer::visitContinueStmt(AST::ContinueStmt* stmt) {
    // Nothing
}

void ASTOptimizer::visitSwitchStmt(AST::SwitchStmt* stmt) {
    stmt->expr->accept(this);
    for(auto _case : stmt->cases) {
        _case->accept(this);
    }
}
void ASTOptimizer::visitCaseStmt(AST::CaseStmt* _case) {
    for(int i = 0; i < _case->stmts.size(); i++){
        ASTNodePtr stmt = _case->stmts[i];
        if(stmtIsTerminator(stmt) && i != _case->stmts.size()-1){
            // TODO: warning
            _case->stmts.resize(i+1);
        }
        stmt->accept(this);
    }
}
void ASTOptimizer::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    // Nothing
}
void ASTOptimizer::visitReturnStmt(AST::ReturnStmt* stmt) {
    stmt->expr->accept(this);
}
