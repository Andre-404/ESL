#pragma once
#include "../Parsing/ASTDefs.h"
// Identifies types of variable declarations(and function arguments)
// Variable declarations can be: global, local, local upvalue
// Function arguments can be: local, local upvalue
//
// If a variable is of type "local upvalue" after it's defined OP_CREATE_UPVALUE is called,
// and that stack slot is replaced with a ObjUpvalue which contains the value
// To mutate/access a local upvalue OP_GET_LOCAL_UPVALUE and OP_SET_LOCAL_UPVALUE are emitted by the compiler
//
// This AST pass doesn't emit any errors and in namedVar only locals and upvalues are looked at, it ignores globals and natives
// Lets the compiler worry about semantic correctness of the variable declarations, this pass only cares if some local var is accessed
// by a closure, in which case it must be turned into an upvaulue

namespace upvalueFinder {
    struct Local {
        AST::ASTVar* var = nullptr;
        int depth = -1;
    };

    struct Upvalue {
        uint8_t index = 0;
        bool isLocal = false;
    };

    struct CurrentChunkInfo {
        // For closures
        CurrentChunkInfo *enclosing;
        //locals
        Local locals[256];
        uInt localCount;
        uInt upvalCount;
        uInt scopeDepth;
        std::array<Upvalue, 256> upvalues;

        CurrentChunkInfo(CurrentChunkInfo *_enclosing);
    };


    class UpvalueFinder : public AST::Visitor {
    public:
        CurrentChunkInfo* current;

        UpvalueFinder(vector<CSLModule*>& units);

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

        #pragma region Helpers
        //variables
        void declareGlobalVar(AST::ASTVar& var);
        void namedVar(Token name, bool canAssign);
        //locals
        void declareLocalVar(AST::ASTVar& var);
        void addLocal(AST::ASTVar& name);
        int resolveLocal(Token name);
        int resolveLocal(CurrentChunkInfo* func, Token name);
        int resolveUpvalue(CurrentChunkInfo* func, Token name);
        int addUpvalue(CurrentChunkInfo* func, byte index, bool isLocal);
        void beginScope();
        void endScope();
        #pragma endregion
    };

}