#pragma once
#include "../../Parsing/ASTDefs.h"
#include "../TypedAST/TypedASTDefs.h"
#include "variableFinder.h"
#include "../../Includes/unorderedDense.h"

namespace passes{
namespace typedASTParser{
    enum class FuncType {
        TYPE_FUNC,
        TYPE_METHOD,
        TYPE_CONSTRUCTOR,
        TYPE_SCRIPT,
    };

    using varPtr = std::shared_ptr<typedAST::VarDecl>;

    struct Local {
        string name;
        int depth;
        varPtr ptr; // Value that this local holds, in IR pointer to an alloca
        // Whether this local variable has been captured as an upvalue and should be accessed through ObjUpval
        bool isUpval;

        Local(string _name, int _depth, bool _isUpval) : name(_name), depth(_depth), isUpval(_isUpval) {}
        Local(){
            name = "";
            depth = -1;
            ptr = nullptr;
            isUpval = false;
            types = nullptr;
        }
    };

    // Represents an Upvalue held in ObjClosure, at function entry all
    struct Upvalue {
        string name = "";
        varPtr ptr = nullptr;

        Upvalue(string _name, std::shared_ptr<typedAST::VarDecl> _val) : name(_name), ptr(_val) {
            types = nullptr;
        }
    };


    // Information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
    struct CurrentChunkInfo {
        // For closures
        CurrentChunkInfo* enclosing;
        typedAST::Function func;
        std::shared_ptr<types::TypeUnion> retTy;
        FuncType type;
        // First ptr is pointer to the VarDecl from an outer function to store to the closure,
        // second is to the VarDecl used inside this function
        vector<std::pair<std::shared_ptr<typedAST::VarDecl>, std::shared_ptr<typedAST::VarDecl>>> upvalPtrs;

        int line;
        int scopeDepth;
        // Stack can grow an arbitrary amount
        vector<Local> locals;
        vector<Upvalue> upvalues;
        CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, string funcName);
    };

    struct ClassChunkInfo {
        // Privates are prefixed with "priv."
        // Int is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, int> fields;
        std::unordered_map<string, std::pair<typedAST::Function, int>> methods;
        types::tyVarIdx classTypeIdx;
        std::shared_ptr<types::ClassType> classTy;
        string mangledName;

        std::shared_ptr<ClassChunkInfo> parent;

        ClassChunkInfo(string _name, std::shared_ptr<types::ClassType> _classTy, types::tyVarIdx _classType, std::shared_ptr<ClassChunkInfo> _parent = nullptr){
            mangledName = _name;
            classTypeIdx = _classType;
            parent = _parent;
            classTy = _classTy;
        }

        void inherit(std::shared_ptr<ClassChunkInfo> _parent){
            methods = _parent->methods;
            fields = _parent->fields;
        }
    };

    struct TransformerException {

    };

    struct Globalvar {
        varPtr valPtr;
        bool isDefined;

        Globalvar(std::shared_ptr<typedAST::VarDecl> _val) {
            valPtr = _val;
            isDefined = false;
        }
    };

    class ASTTransformer : public AST::Visitor {
    public:
        // Passed to other passes, used for highlighting errors
        vector<File*> sourceFiles;
        bool hadError;

        ASTTransformer();
        std::pair<typedAST::Function, vector<File*>> run(vector<ESLModule*>& units, std::unordered_map<AST::FuncLiteral*, vector<variableFinder::Upvalue>> upvalMap);

        #pragma region Visitor pattern
        void visitAssignmentExpr(AST::AssignmentExpr* expr) override;
        void visitSetExpr(AST::SetExpr* expr) override;
        void visitConditionalExpr(AST::ConditionalExpr* expr) override;
        void visitRangeExpr(AST::RangeExpr* expr) override;
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
        // Compiler only ever emits the code for a single function, top level code is considered a function
        CurrentChunkInfo* current;
        std::shared_ptr<ClassChunkInfo> currentClass;

        ESLModule* curUnit;
        int curUnitIndex;
        vector<ESLModule*> units;
        std::unordered_map<AST::FuncLiteral*, vector<variableFinder::Upvalue>> upvalueMap;
        ankerl::unordered_dense::map<string, Globalvar> globals;
        ankerl::unordered_dense::map<string, std::shared_ptr<ClassChunkInfo>> globalClasses;
        ankerl::unordered_dense::map<string, types::tyVarIdx> nativesTypes;
        // Bool is to know if a type is collapsed
        vector<std::pair<types::tyPtr, bool>> typeEnv;

        vector<typedAST::nodePtr> nodesToReturn;
        typedAST::exprPtr returnedExpr;

        #pragma region Helpers
        // Variables
        // Checks all imports to see if the symbol 'token' is imported
        varPtr checkSymbol(Token symbol);
        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        varPtr resolveGlobal(Token symbol, bool canAssign);
        varPtr declareGlobalVar(string name, AST::ASTDeclType type, types::tyVarIdx typeConstraint = nullptr);
        void defineGlobalVar(string name);

        varPtr declareLocalVar(AST::ASTVar& name);
        void defineLocalVar();

        varPtr addLocal(AST::ASTVar name);
        int resolveLocal(Token name);

        int resolveUpvalue(Token name);

        typedAST::exprPtr readVar(Token name);
        typedAST::exprPtr storeToVar(Token name, typedAST::exprPtr toStore);

        void beginScope();
        void endScope();
        // Functions
        typedAST::Function endFuncDecl();
        // Classes and methods
        typedAST::Function createMethod(AST::FuncDecl* _method, string className, types::tyVarIdx fnTy, std::shared_ptr<types::TypeUnion> retTy);
        std::shared_ptr<typedAST::InvokeExpr> tryConvertToInvoke(typedAST::exprPtr callee, vector<typedAST::exprPtr> args);
        // Resolve implicit object field access
        std::shared_ptr<typedAST::InstGet> resolveClassFieldRead(Token name);
        std::shared_ptr<typedAST::InstSet> resolveClassFieldStore(Token name, typedAST::exprPtr toStore);
        Globalvar& getClassFromExpr(AST::ASTNodePtr expr);
        std::shared_ptr<ClassChunkInfo> getClassInfoFromExpr(AST::ASTNodePtr expr);
        // Resolve public/private fields when this.field in encountered in code
        std::shared_ptr<typedAST::InstGet> tryResolveThis(AST::FieldAccessExpr* expr);
        std::shared_ptr<typedAST::InstSet> tryResolveThis(AST::SetExpr* expr);
        // Misc
        Token syntheticToken(string str);
        void updateLine(Token token);
        void error(Token token, const string& msg) noexcept(false);
        void error(const string& message) noexcept(false);
        typedAST::Block parseStmtsToBlock(vector<AST::ASTNodePtr> stmts);
        typedAST::Block parseStmtToBlock(AST::ASTNodePtr stmt);
        typedAST::exprPtr evalASTExpr(std::shared_ptr<AST::ASTNode> node);
        vector<typedAST::nodePtr> evalASTStmt(std::shared_ptr<AST::ASTNode> node);
        types::tyVarIdx addType(types::tyPtr ty);
        types::tyVarIdx getBasicType(types::TypeFlag ty);
        #pragma endregion
    };

}
}