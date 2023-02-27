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
        void visitAssignmentExpr(AST::AssignmentExpr* expr);
        void visitSetExpr(AST::SetExpr* expr);
        void visitConditionalExpr(AST::ConditionalExpr* expr);
        void visitBinaryExpr(AST::BinaryExpr* expr);
        void visitUnaryExpr(AST::UnaryExpr* expr);
        void visitCallExpr(AST::CallExpr* expr);
        void visitNewExpr(AST::NewExpr* expr);
        void visitFieldAccessExpr(AST::FieldAccessExpr* expr);
        void visitAsyncExpr(AST::AsyncExpr* expr);
        void visitAwaitExpr(AST::AwaitExpr* expr);
        void visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr);
        void visitStructLiteralExpr(AST::StructLiteral* expr);
        void visitLiteralExpr(AST::LiteralExpr* expr);
        void visitSuperExpr(AST::SuperExpr* expr);
        void visitFuncLiteral(AST::FuncLiteral* expr);
        void visitModuleAccessExpr(AST::ModuleAccessExpr* expr);
        void visitMacroExpr(AST::MacroExpr* expr);

        void visitVarDecl(AST::VarDecl* decl);
        void visitFuncDecl(AST::FuncDecl* decl);
        void visitClassDecl(AST::ClassDecl* decl);

        void visitExprStmt(AST::ExprStmt* stmt);
        void visitBlockStmt(AST::BlockStmt* stmt);
        void visitIfStmt(AST::IfStmt* stmt);
        void visitWhileStmt(AST::WhileStmt* stmt);
        void visitForStmt(AST::ForStmt* stmt);
        void visitBreakStmt(AST::BreakStmt* stmt);
        void visitContinueStmt(AST::ContinueStmt* stmt);
        void visitSwitchStmt(AST::SwitchStmt* stmt);
        void visitCaseStmt(AST::CaseStmt* _case);
        void visitAdvanceStmt(AST::AdvanceStmt* stmt);
        void visitReturnStmt(AST::ReturnStmt* stmt);
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