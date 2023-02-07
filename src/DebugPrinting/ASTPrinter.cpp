#include "ASTPrinter.h"
#include <iostream>

using namespace AST;
using std::cout;
using std::endl;

void ASTPrinter::visitAssignmentExpr(AssignmentExpr* expr) {
	cout << expr->name.getLexeme() << " = ";
	expr->value->accept(this);
}

void ASTPrinter::visitSetExpr(SetExpr* expr) {
	expr->callee->accept(this);
	cout << expr->accessor.getLexeme();
	expr->field->accept(this);
	cout << (expr->accessor.type == TokenType::LEFT_BRACKET ? "]" : "");
	cout << " = ";
	expr->value->accept(this);
}

void ASTPrinter::visitConditionalExpr(ConditionalExpr* expr) {
	expr->condition->accept(this);
	cout << " ? ";
	expr->thenBranch->accept(this);
	cout << " : ";
	expr->elseBranch->accept(this);
}

void ASTPrinter::visitBinaryExpr(BinaryExpr* expr) {
	cout << "(" << expr->op.getLexeme()<<" ";
	expr->left->accept(this);
	cout << " ";
	expr->right->accept(this);
	cout << ")";
}

void ASTPrinter::visitUnaryExpr(UnaryExpr* expr) {
	cout << "(" << (expr->isPrefix ? expr->op.getLexeme() : "") << " ";
	expr->right->accept(this);
	cout << (expr->isPrefix ? "" : expr->op.getLexeme()) << ")";
}

void ASTPrinter::visitCallExpr(CallExpr* expr) {
	expr->callee->accept(this);
	cout << "( ";
	for (shared_ptr<ASTNode> node : expr->args) {
		node->accept(this);
		cout << ", ";
	}
	cout << ")";
}

void ASTPrinter::visitFieldAccessExpr(FieldAccessExpr* expr) {
	expr->callee->accept(this);
	cout << expr->accessor.getLexeme();
	expr->field->accept(this);
	cout << (expr->accessor.type == TokenType::LEFT_BRACKET ? "]" : "");
}

void ASTPrinter::visitGroupingExpr(GroupingExpr* expr) {
	cout << "(group ";
	expr->expr->accept(this);
	cout << ")";
}

void ASTPrinter::visitAsyncExpr(AsyncExpr* expr) {
	cout << "await ";
	expr->callee->accept(this);
}

void ASTPrinter::visitAwaitExpr(AwaitExpr* expr) {
	cout << "async";
	expr->expr->accept(this);
}

void ASTPrinter::visitArrayLiteralExpr(ArrayLiteralExpr* expr) {
	cout << "[ ";
	for (shared_ptr<ASTNode> node : expr->members) {
		node->accept(this);
		cout << ", ";
	}
	cout << "]";
}

void ASTPrinter::visitStructLiteralExpr(StructLiteral* expr) {
	cout << "{ ";
	for (StructEntry entry : expr->fields) {
		cout << entry.name.getLexeme() << " : ";
		entry.expr->accept(this);
		cout << ", ";
	}
	cout << "}";
}

void ASTPrinter::visitLiteralExpr(LiteralExpr* expr) {
	cout << expr->token.getLexeme();
}

void ASTPrinter::visitFuncLiteral(FuncLiteral* expr) {
	cout << "func literal ( ";
	for (Token arg : expr->args) {
		cout << arg.getLexeme() << ", ";
	}
	cout << ") ";
	expr->body->accept(this);
	cout << endl;
}

void ASTPrinter::visitSuperExpr(SuperExpr* expr) {
	cout << "super." << expr->methodName.getLexeme();
}

void ASTPrinter::visitModuleAccessExpr(ModuleAccessExpr* expr) {
	cout << expr->moduleName.getLexeme() << "::" << expr->ident.getLexeme();
}



void ASTPrinter::visitVarDecl(VarDecl* decl) {
	cout << "var decl: " << decl->name.getLexeme() << " = ";
	if (decl->value != nullptr) {
		decl->value->accept(this);
	}
	else {
		cout << "nil";
	}
	cout << endl;
}

void ASTPrinter::visitFuncDecl(FuncDecl* decl) {
	cout << "func " << decl->name.getLexeme() << "( ";
	for (Token arg : decl->args) {
		cout << arg.getLexeme() << ", ";
	}
	cout << ") ";
	decl->body->accept(this);
	cout << endl;
}

void ASTPrinter::visitClassDecl(ClassDecl* decl) {
	cout << "class " << decl->name.getLexeme()
		<< (decl->inherits ? ":" : "");
	if(decl->inherits) decl->inheritedClass->accept(this);
	cout << "{ " << endl;
	for (shared_ptr<ASTNode> method : decl->methods) {
		method->accept(this);
	}
	cout << "}" << endl;
}



void ASTPrinter::visitPrintStmt(PrintStmt* stmt) {
	cout << "print: ";
	stmt->expr->accept(this);
	cout << endl;
}

void ASTPrinter::visitExprStmt(ExprStmt* stmt) {
	cout << "Expr stmt: ";
	stmt->expr->accept(this);
	cout << endl;
}

void ASTPrinter::visitBlockStmt(BlockStmt* stmt) {
	cout << "{ " << endl;
	for (shared_ptr<ASTNode> line : stmt->statements) {
		line->accept(this);
	}
	cout << "}" << endl;
}

void ASTPrinter::visitIfStmt(IfStmt* stmt) {
	cout << "if (";
	stmt->condition->accept(this);
	cout << ")";
	stmt->thenBranch->accept(this);
	if (stmt->elseBranch != nullptr) {
		cout << "else";
		stmt->elseBranch->accept(this);
	}
}

void ASTPrinter::visitWhileStmt(WhileStmt* stmt) {
	cout << "while (";
	stmt->condition->accept(this);
	cout << ")";
	stmt->body->accept(this);
}

void ASTPrinter::visitForStmt(ForStmt* stmt) {
	cout << "for (";
	if (stmt->init != nullptr) {
		stmt->init->accept(this);
		cout << "; ";
	}
	if (stmt->condition != nullptr) {
		stmt->condition->accept(this);
		cout << "; ";
	}
	if (stmt->increment != nullptr) {
		stmt->increment->accept(this);
	}
	cout << ")";
	stmt->body->accept(this);
}

void ASTPrinter::visitBreakStmt(BreakStmt* stmt) {
	cout << "break" << endl;
}

void ASTPrinter::visitContinueStmt(ContinueStmt* stmt) {
	cout << "continue" << endl;
}

void ASTPrinter::visitSwitchStmt(SwitchStmt* stmt) {
	cout << "switch (";
	stmt->expr->accept(this);
	cout << ") {" << endl;
	for (shared_ptr<CaseStmt> _case : stmt->cases) {
		_case->accept(this);
	}
	cout << "}" << endl;
}

void ASTPrinter::visitCaseStmt(CaseStmt* stmt) {
	cout << (stmt->caseType.type == TokenType::CASE ? "case " : "default ");
	for (Token token : stmt->constants) {
		cout << token.getLexeme() << " | ";
	}
	cout << ": " << endl;
	for (shared_ptr<ASTNode> statement : stmt->stmts) {
		statement->accept(this);
	}
}

void ASTPrinter::visitAdvanceStmt(AdvanceStmt* stmt) {

}

void ASTPrinter::visitReturnStmt(ReturnStmt* stmt) {
	cout << "return ";
	if (stmt->expr != nullptr) {
		stmt->expr->accept(this);
	}
	cout << endl;
}