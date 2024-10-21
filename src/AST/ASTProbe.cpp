#include "ASTProbe.h"

void AST::ASTProbe::visitAssignmentExpr(AssignmentExpr* expr){}
void AST::ASTProbe::visitSetExpr(SetExpr* expr){}
void AST::ASTProbe::visitConditionalExpr(ConditionalExpr* expr){}
void AST::ASTProbe::visitBinaryExpr(BinaryExpr* expr){}
void AST::ASTProbe::visitUnaryExpr(UnaryExpr* expr) {}
void AST::ASTProbe::visitCallExpr(CallExpr* expr) {}
void AST::ASTProbe::visitNewExpr(NewExpr* expr) {}
void AST::ASTProbe::visitFieldAccessExpr(FieldAccessExpr* expr) {}
void AST::ASTProbe::visitArrayLiteralExpr(ArrayLiteralExpr* expr) {}
void AST::ASTProbe::visitStructLiteralExpr(StructLiteral* expr) {}
void AST::ASTProbe::visitLiteralExpr(LiteralExpr* expr) { probedToken = expr->token; }
void AST::ASTProbe::visitFuncLiteral(FuncLiteral* expr) {}
void AST::ASTProbe::visitModuleAccessExpr(ModuleAccessExpr* expr) {}
void AST::ASTProbe::visitMacroExpr(MacroExpr* expr) {}

void AST::ASTProbe::visitVarDecl(VarDecl* decl) {}
void AST::ASTProbe::visitFuncDecl(FuncDecl* decl) {}
void AST::ASTProbe::visitClassDecl(ClassDecl* decl) {}

void AST::ASTProbe::visitExprStmt(ExprStmt* stmt) { expr = stmt->expr; }
void AST::ASTProbe::visitSpawnStmt(SpawnStmt* stmt){}
void AST::ASTProbe::visitBlockStmt(BlockStmt* stmt) {}
void AST::ASTProbe::visitIfStmt(IfStmt* stmt) {}
void AST::ASTProbe::visitWhileStmt(WhileStmt* stmt) {}
void AST::ASTProbe::visitForStmt(ForStmt* stmt) {}
void AST::ASTProbe::visitBreakStmt(BreakStmt* stmt) {}
void AST::ASTProbe::visitContinueStmt(ContinueStmt* stmt) {}
void AST::ASTProbe::visitSwitchStmt(SwitchStmt* stmt) {}
void AST::ASTProbe::visitCaseStmt(CaseStmt* _case) {}
void AST::ASTProbe::visitAdvanceStmt(AdvanceStmt* stmt) {}
void AST::ASTProbe::visitReturnStmt(ReturnStmt* stmt) {}

Token AST::ASTProbe::getProbedToken() { return probedToken; }
AST::ASTNodePtr AST::ASTProbe::getExtractedExpr() { return expr; }
