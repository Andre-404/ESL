#pragma once
#include "ASTDefs.h"

namespace AST {
	//used for probing AST nodes for fetching identifier names (and doing other useful stuff)
	class ASTProbe : public Visitor {
	private:
		Token probedToken = Token();
        ASTNodePtr expr = nullptr;
	public:
		void visitAssignmentExpr(AssignmentExpr* expr);
		void visitSetExpr(SetExpr* expr);
		void visitConditionalExpr(ConditionalExpr* expr);
		void visitBinaryExpr(BinaryExpr* expr);
		void visitUnaryExpr(UnaryExpr* expr);
		void visitCallExpr(CallExpr* expr);
        void visitNewExpr(NewExpr* expr);
		void visitFieldAccessExpr(FieldAccessExpr* expr);
		void visitArrayLiteralExpr(ArrayLiteralExpr* expr);
		void visitStructLiteralExpr(StructLiteral* expr);
		void visitLiteralExpr(LiteralExpr* expr);
		void visitFuncLiteral(FuncLiteral* expr);
		void visitModuleAccessExpr(ModuleAccessExpr* expr);
        void visitMacroExpr(MacroExpr* expr);


		void visitVarDecl(VarDecl* decl);
		void visitFuncDecl(FuncDecl* decl);
		void visitClassDecl(ClassDecl* decl);

		void visitExprStmt(ExprStmt* stmt);
        void visitSpawnStmt(SpawnStmt* stmt);
		void visitBlockStmt(BlockStmt* stmt);
		void visitIfStmt(IfStmt* stmt);
		void visitWhileStmt(WhileStmt* stmt);
		void visitForStmt(ForStmt* stmt);
		void visitBreakStmt(BreakStmt* stmt);
		void visitContinueStmt(ContinueStmt* stmt);
		void visitSwitchStmt(SwitchStmt* stmt);
		void visitCaseStmt(CaseStmt* _case);
		void visitAdvanceStmt(AdvanceStmt* stmt);
		void visitReturnStmt(ReturnStmt* stmt);

		Token getProbedToken();
	    ASTNodePtr getExtractedExpr();
    };
}