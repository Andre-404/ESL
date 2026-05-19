#pragma once
#include "../../../Includes/fmt/core.h"
#include "../../../TypedAST/TypedASTDefs.h"

class CFGPrinter : CFG::CFGVisitor{
    string indent;
    string ty_to_str(types::tyPtr ty) {
        using namespace types;
        switch (ty->type) {
            case TypeFlag::NIL: return "nil";
            case TypeFlag::BOOL: return "bool";
            case TypeFlag::NUMBER: return "number";
            case TypeFlag::STRING: return "string";
            case TypeFlag::MUTEX: return "mutex";
            case TypeFlag::FILE: return "file";
            case TypeFlag::ANY: return "any";
            case TypeFlag::ARRAY:
                return fmt::format("array<{}>", ty_to_str(std::reinterpret_pointer_cast<ArrayType>(ty)->itemType));
            case TypeFlag::FUNCTION: {
                string st = "function (";
                auto fn_ty = std::reinterpret_pointer_cast<FunctionType>(ty);
                for (auto& arg : fn_ty->paramTypes) st += fmt::format("<{}>, ", ty_to_str(arg));
                st.pop_back();
                st.append(fmt::format(") -> <{}>", ty_to_str(fn_ty->retType)));
                return st;
            }
            case TypeFlag::HASHMAP:
                return fmt::format("hashmap<{}>", ty_to_str(std::reinterpret_pointer_cast<HashMapType>(ty)->itemType));
            case TypeFlag::INSTANCE:
                return fmt::format("instance<{}>", std::reinterpret_pointer_cast<InstanceType>(ty)->klass->name);
            case TypeFlag::CLASS:
                return fmt::format("class {}", std::reinterpret_pointer_cast<ClassType>(ty)->name);
            case TypeFlag::UNKNOWN: return "unknown";
        }
    }
    public:
    void run(std::shared_ptr<CFG::Function> fn) {
        for (auto stmt : fn->block.stmts) stmt->accept(this);
    }
    void visitVarDecl(CFG::VarDecl* decl) override {
        fmt::print("{}Decl for var {}\n", indent, decl->dbgInfo.varName.getLexeme());
    }
    void visitVarRead(CFG::VarRead* expr) override {
        fmt::print("{}Var read {} of type {}\n", indent, expr->dbgInfo.varName.getLexeme(), ty_to_str(expr->exprType));
    }
    void visitVarStore(CFG::VarStore* expr) override {
        fmt::print("{}Var store {} of type {}\n", indent, expr->dbgInfo.varName.getLexeme(), ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->toStore->accept(this);
        indent.pop_back();
    }
    void visitVarReadNative(CFG::VarReadNative* expr) override {
        fmt::print("{}Var native read {} of type {}\n", indent, expr->dbgInfo.varName.getLexeme(), ty_to_str(expr->exprType));
    }
    void visitArithmeticExpr(CFG::ArithmeticExpr* expr) override {
        fmt::print("{}Arithmetic expr with op {} of type {}\n", indent, expr->dbgInfo.op.getLexeme(), ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->lhs->accept(this);
        expr->rhs->accept(this);
        indent.pop_back();
    }
    void visitComparisonExpr(CFG::ComparisonExpr* expr) override {
        fmt::print("{}Comparison expr with op {} of type {}\n", indent, expr->dbgInfo.op.getLexeme(), ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->lhs->accept(this);
        expr->rhs->accept(this);
        indent.pop_back();
    }
    void visitInstanceofExpr(CFG::InstanceofExpr* expr) override {
        fmt::print("{}Instanceof expr {} of type {}\n", indent, expr->dbgInfo.op.getLexeme(), ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->lhs->accept(this);
        indent.pop_back();
    }
    void visitUnaryExpr(CFG::UnaryExpr* expr) override {
        fmt::print("{}Unary expr with op {} of type {}\n", indent, expr->dbgInfo.op.getLexeme(), ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->rhs->accept(this);
        indent.pop_back();
    }
    void visitLiteralExpr(CFG::LiteralExpr* expr) override {
        fmt::print("{}Literal expr {} of type {}\n", indent, expr->dbgInfo.literal.getLexeme(), ty_to_str(expr->exprType));
    }
    void visitHashmapExpr(CFG::HashmapExpr* expr) override {
        fmt::print("{}Hashmap expr of type {}\n", indent, ty_to_str(expr->exprType));

        indent.push_back(' ');
        for (auto [field, expr] : expr->fields) {
            fmt::print("{} {} :\n", indent, field);
            indent.push_back(' ');
            expr->accept(this);
            indent.pop_back();
        }
        indent.pop_back();
    }
    void visitArrayExpr(CFG::ArrayExpr* expr) override {
        fmt::print("{}Array expr of type {}\n", indent, ty_to_str(expr->exprType));

        indent.push_back(' ');
        int i = 0;
        for (auto field : expr->fields) {
            fmt::print("{} [{}] :\n", indent, i);
            indent.push_back(' ');
            field->accept(this);
            indent.pop_back();
            i++;
        }
        indent.pop_back();
    }
    void visitCollectionGet(CFG::CollectionGet* expr) override {
        fmt::print("{}Collection get expr of type {}\n", indent, ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->field->accept(this);
        expr->collection->accept(this);
        indent.pop_back();
    }
    void visitCollectionSet(CFG::CollectionSet* expr) override {
        fmt::print("{}Collection set expr of type {}\n", indent, ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->field->accept(this);
        expr->collection->accept(this);
        expr->toStore->accept(this);
        indent.pop_back();
    }
    void visitConditionalExpr(CFG::ConditionalExpr* expr) override {
        fmt::print("{}Conditional expr of type {}\n", indent, ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->cond->accept(this);
        expr->thenExpr->accept(this);
        expr->elseExpr->accept(this);
        indent.pop_back();
    }
    void visitCallExpr(CFG::CallExpr* expr) override {
        fmt::print("{}Call expr of type {}\n", indent, ty_to_str(expr->exprType));

        indent.push_back(' ');
        expr->callee->accept(this);
        for (auto field : expr->args) {
            field->accept(this);
        }
        indent.pop_back();
    }
    void visitInvokeExpr(CFG::InvokeExpr* expr) override {
        fmt::print("{}Invoke expr on field {} of type {}\n", indent, expr->field, ty_to_str(expr->exprType));

        indent.push_back(' ');
        expr->inst->accept(this);
        for (auto field : expr->args) {
            field->accept(this);
        }
        indent.pop_back();
    }
    void visitNewExpr(CFG::NewExpr* expr) override {
        fmt::print("{}New expr of type {}\n", indent, ty_to_str(expr->exprType));

        indent.push_back(' ');
        for (auto field : expr->args) {
            field->accept(this);
        }
        indent.pop_back();
    }
    void visitSpawnStmt(CFG::SpawnStmt* stmt) override {
        fmt::print("{}Spawn stmt\n", indent);
        indent.push_back(' ');
        stmt->call->accept(this);
        indent.pop_back();
    }
    void visitCreateClosureExpr(CFG::CreateClosureExpr* expr) override {
        fmt::print("{}Closure expr of type {}\n", indent, ty_to_str(expr->exprType));

        indent.append("  ");
        for (auto stmt : expr->fn->block.stmts) stmt->accept(this);
        indent.pop_back();
        indent.pop_back();
    }
    void visitFuncDecl(CFG::FuncDecl* decl) override {
        fmt::print("{}Func decl {}\n", indent, decl->dbgInfo.name.getLexeme());

        indent.append("  ");
        for (auto stmt : decl->fn->block.stmts) stmt->accept(this);
        indent.pop_back();
        indent.pop_back();
    }
    void visitExprStmt(CFG::ExprStmt* stmt) override {
        fmt::print("{}Expr stmt\n", indent);
        indent.push_back(' ');
        stmt->expr->accept(this);
        indent.pop_back();
    }
    void visitReturnStmt(CFG::ReturnStmt* stmt) override {
        fmt::print("{}Return stmt\n", indent);
        indent.push_back(' ');
        if (stmt->expr) stmt->expr->accept(this);
        indent.pop_back();
    }
    void visitUncondJump(CFG::UncondJump* stmt) override {
        fmt::print("{}Unconditional jump stmt\n", indent);
    }
    void visitIfStmt(CFG::IfStmt* stmt) override {
        fmt::print("{}If stmt\n", indent);
        indent.push_back(' ');
        stmt->cond->accept(this);
        for (auto stmt : stmt->thenBlock.stmts) stmt->accept(this);
        for (auto stmt : stmt->elseBlock.stmts) stmt->accept(this);
        indent.pop_back();
    }
    void visitWhileStmt(CFG::WhileStmt* stmt) override {
        fmt::print("{}While stmt\n", indent);
        indent.push_back(' ');
        if (stmt->cond) stmt->cond->accept(this);
        for (auto stmt : stmt->loopBody.stmts) stmt->accept(this);
        if (stmt->afterLoopExpr) stmt->afterLoopExpr->accept(this);
        indent.pop_back();
    }
    void visitSwitchStmt(CFG::SwitchStmt* stmt) override {
        fmt::print("{}Switch stmt\n", indent);
        indent.push_back(' ');
        stmt->cond->accept(this);
        for (auto& _case : stmt->cases) {
            fmt::print("{}Case\n", indent);
            indent.push_back(' ');
            for (auto& stmt : _case.stmts) stmt->accept(this);
            indent.pop_back();
        }
        indent.pop_back();
    }
    void visitClassDecl(CFG::ClassDecl* decl) override {
        fmt::print("{}Class decl {} with parent {}\n", indent, decl->fullName, decl->parentClassName);
        indent.push_back(' ');
        for (auto& [name, method] : decl->methods) {
            fmt::print("{}Method {}\n", indent, name);
            indent.push_back(' ');
            for (auto& stmt : method.first.code->block.stmts) stmt->accept(this);
            indent.pop_back();
        }
        indent.pop_back();
    }
    void visitInstGet(CFG::InstGet* expr) override {
        fmt::print("{}Inst get expr of type {}\n", indent, ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->instance->accept(this);
        indent.pop_back();
    }
    void visitInstSet(CFG::InstSet* expr) override {
        fmt::print("{}Inst set expr of type {}\n", indent, ty_to_str(expr->exprType));
        indent.push_back(' ');
        expr->instance->accept(this);
        expr->toStore->accept(this);
        indent.pop_back();
    }
    void visitScopeBlock(CFG::ScopeEdge* stmt) override {
        fmt::print("{}Scope {}\n", indent, stmt->edgeType == CFG::ScopeEdgeType::START ? "start" : "end");
    }
};