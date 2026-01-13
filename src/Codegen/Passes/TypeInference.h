#pragma once
#include "computeClassHierarchy.h"
#include "../../TypedAST/TypedASTDefs.h"
#include <unordered_map>
#include <unordered_set>

class TypeInferencePass : CFG::CFGVisitor{
    CFG::CFGStmt* _cur_stmt;
    std::shared_ptr<types::FunctionType> _cur_fn;
    std::unordered_set<std::shared_ptr<types::FunctionType>> _processed_funcs;
    std::unordered_map<uInt64, std::shared_ptr<types::FunctionType>> _function_types;
    types::tyPtr getVarType(std::shared_ptr<CFG::VarDecl> decl);
    // if we haven't completely eval-ed a function we can't know its return type for sure
    bool func_complete(types::tyPtr func);
    std::pair<std::shared_ptr<types::FunctionType>, CFG::CFGStmt*> start_func(std::shared_ptr<types::FunctionType> ty) {
        std::pair tmp = { _cur_fn, _cur_stmt };
        _cur_fn = ty;
        _cur_stmt = nullptr;
        ty->retType = types::getBasicType(types::TypeFlag::UNKNOWN);
        return tmp;
    }
    void end_func(const std::pair<std::shared_ptr<types::FunctionType>, CFG::CFGStmt*>& temp_data) {
        _processed_funcs.insert(_cur_fn);
        _cur_fn = temp_data.first;
        _cur_stmt = temp_data.second;
    }
public:
    TypeInferencePass() {
        _cur_stmt = nullptr;
        _cur_fn = nullptr;
    }
    void run(std::pair<std::shared_ptr<CFG::Function>, vector<File*>>& main_fn, bool should_print);


    void visitVarDecl(CFG::VarDecl* decl) override;
    void visitVarRead(CFG::VarRead* expr) override;
    void visitVarStore(CFG::VarStore* expr) override;
    void visitVarReadNative(CFG::VarReadNative* expr) override;
    void visitArithmeticExpr(CFG::ArithmeticExpr* expr) override;
    void visitComparisonExpr(CFG::ComparisonExpr* expr) override;
    void visitInstanceofExpr(CFG::InstanceofExpr* expr) override;
    void visitUnaryExpr(CFG::UnaryExpr* expr) override;
    void visitLiteralExpr(CFG::LiteralExpr* expr) override;
    void visitHashmapExpr(CFG::HashmapExpr* expr) override;
    void visitArrayExpr(CFG::ArrayExpr* expr) override;
    void visitCollectionGet(CFG::CollectionGet* expr) override;
    void visitCollectionSet(CFG::CollectionSet* expr) override;
    void visitConditionalExpr(CFG::ConditionalExpr* expr) override;
    void visitCallExpr(CFG::CallExpr* expr) override;
    void visitInvokeExpr(CFG::InvokeExpr* expr) override;
    void visitNewExpr(CFG::NewExpr* expr) override;
    void visitSpawnStmt(CFG::SpawnStmt* stmt) override;
    void visitCreateClosureExpr(CFG::CreateClosureExpr* expr) override;
    void visitFuncDecl(CFG::FuncDecl* decl) override;
    void visitExprStmt(CFG::ExprStmt* stmt) override;
    void visitReturnStmt(CFG::ReturnStmt* stmt) override;
    void visitUncondJump(CFG::UncondJump* stmt) override;
    void visitIfStmt(CFG::IfStmt* stmt) override;
    void visitWhileStmt(CFG::WhileStmt* stmt) override;
    void visitSwitchStmt(CFG::SwitchStmt* stmt) override;
    void visitClassDecl(CFG::ClassDecl* decl) override;
    void visitInstGet(CFG::InstGet* expr) override;
    void visitInstSet(CFG::InstSet* expr) override;
    void visitScopeBlock(CFG::ScopeEdge* stmt) override;
};
