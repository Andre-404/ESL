#pragma once

#include <array>
#include "../../Parsing/ASTDefs.h"
#include <unordered_set>
// Identifies types of variable declarations(and function arguments)
// Variable declarations can be: global, local, upvalue
// Function arguments can be: local, upvalue

namespace variableFinder {
    struct Local {
        AST::ASTVar* var = nullptr;
        int depth = -1;
    };

    struct Upvalue {
        int index = 0;
        bool isLocal = false;
        // Name of this variable
        string name = "";

        Upvalue(int _index, bool _isLocal, string _name) : index(_index), isLocal(_isLocal), name(_name) {}
    };

    struct CurrentChunkInfo {
        // For closures
        CurrentChunkInfo *enclosing;
        //locals
        vector<Local> locals;
        uInt scopeDepth;
        vector<Upvalue> upvalues;

        CurrentChunkInfo(CurrentChunkInfo *_enclosing);
    };


    class VariableTypeFinder : public AST::Visitor {
    public:
        CurrentChunkInfo* current;
        bool hadError;

        VariableTypeFinder(vector<ESLModule*>& units);

        std::unordered_map<AST::FuncLiteral*, vector<Upvalue>> generateUpvalueMap();

        #pragma region Visitor pattern
        void visitAssignmentExpr(AST::AssignmentExpr* expr) override;
        void visitRangeExpr(AST::RangeExpr *expr) override;
        void visitSetExpr(AST::SetExpr* expr) override;
        void visitConditionalExpr(AST::ConditionalExpr* expr) override;
        void visitBinaryExpr(AST::BinaryExpr* expr) override;
        void visitUnaryExpr(AST::UnaryExpr* expr) override;
        void visitCallExpr(AST::CallExpr* expr) override;
        void visitNewExpr(AST::NewExpr* expr) override;
        void visitFieldAccessExpr(AST::FieldAccessExpr* expr) override;
        void visitAsyncExpr(AST::AsyncExpr* expr) override;
        void visitAwaitExpr(AST::AwaitExpr* expr) override;
        void visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) override;
        void visitStructLiteralExpr(AST::StructLiteral* expr) override;
        void visitLiteralExpr(AST::LiteralExpr* expr) override;
        void visitSuperExpr(AST::SuperExpr* expr) override;
        void visitFuncLiteral(AST::FuncLiteral* expr) override;
        void visitModuleAccessExpr(AST::ModuleAccessExpr* expr) override;
        void visitMacroExpr(AST::MacroExpr* expr) override;

        void visitVarDecl(AST::VarDecl* decl) override;
        void visitFuncDecl(AST::FuncDecl* decl) override;
        void visitClassDecl(AST::ClassDecl* decl) override;

        void visitExprStmt(AST::ExprStmt* stmt) override;
        void visitBlockStmt(AST::BlockStmt* stmt) override;
        void visitIfStmt(AST::IfStmt* stmt) override;
        void visitWhileStmt(AST::WhileStmt* stmt) override;
        void visitForStmt(AST::ForStmt* stmt) override;
        void visitBreakStmt(AST::BreakStmt* stmt) override;
        void visitContinueStmt(AST::ContinueStmt* stmt) override;
        void visitSwitchStmt(AST::SwitchStmt* stmt) override;
        void visitCaseStmt(AST::CaseStmt* _case) override;
        void visitAdvanceStmt(AST::AdvanceStmt* stmt) override;
        void visitReturnStmt(AST::ReturnStmt* stmt) override;
        #pragma endregion
    private:
        std::unordered_map<AST::FuncLiteral*, vector<Upvalue>> upvalueMap;

        #pragma region Helpers
        //variables
        void declareGlobalVar(AST::ASTVar& var);
        void namedVar(Token name);
        //locals
        void declareLocalVar(AST::ASTVar& var);
        void addLocal(AST::ASTVar& name);
        int resolveLocal(Token name);
        int resolveLocal(CurrentChunkInfo* func, Token name);
        int resolveUpvalue(CurrentChunkInfo* func, Token name);
        int addUpvalue(CurrentChunkInfo* func, int index, bool isLocal, string name);
        void beginScope();
        void endScope();
        #pragma endregion
    };

}