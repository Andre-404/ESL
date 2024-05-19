#pragma once
#include "../../AST/ASTDefs.h"
#include "../../TypedAST/TypedASTDefs.h"
#include "closureConverter.h"
#include "../../Includes/unorderedDense.h"
#include "computeClassHierarchy.h"

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
        // Whether this local variable has been captured as an upvalue and should be accessed through ObjFreevar
        bool isUpval;

        Local(const string _name, int _depth, bool _isUpval) : name(_name), depth(_depth), isUpval(_isUpval) {}
        Local(){
            name = "";
            depth = -1;
            ptr = nullptr;
            isUpval = false;
        }
    };

    // Represents a freevar held in ObjClosure, at function entry all freevars are loaded from an object to the stack
    // "ptr" represents the value loaded to the stack
    struct Upvalue {
        string name = "";
        varPtr ptr = nullptr;

        Upvalue(string _name, std::shared_ptr<typedAST::VarDecl> _val) : name(_name), ptr(_val) {}
    };


    // Information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
    struct CurrentChunkInfo {
        // For closures
        CurrentChunkInfo* enclosing;
        std::shared_ptr<typedAST::Function> func;
        FuncType type;
        // First ptr is pointer to the VarDecl from an outer function to store to the closure,
        // second is to the VarDecl used inside this function
        vector<std::pair<std::shared_ptr<typedAST::VarDecl>, std::shared_ptr<typedAST::VarDecl>>> freevarPtrs;

        int line;
        int scopeDepth;
        // Stack can grow an arbitrary amount
        vector<Local> locals;
        vector<Upvalue> freevars;
        CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, string funcName);
    };

    struct ClassChunkInfo {
        // Privates are prefixed with "priv."
        // Int is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, int> fields;
        std::unordered_map<string, std::pair<typedAST::ClassMethod, int>> methods;
        std::shared_ptr<types::ClassType> classTy;
        const string mangledName;

        std::shared_ptr<ClassChunkInfo> parent;

        ClassChunkInfo(string _name, std::shared_ptr<types::ClassType> _classTy, types::tyVarIdx _classTypeIdx)
            : mangledName(_name){
            parent = nullptr;
            classTy = _classTy;
        }

        void inherit(std::shared_ptr<ClassChunkInfo> _parent){
            methods = _parent->methods;
            fields = _parent->fields;
            parent = _parent;
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
        std::pair<std::shared_ptr<typedAST::Function>, vector<File*>>
        run(vector<ESLModule*>& units, std::unordered_map<AST::FuncLiteral*, vector<closureConversion::FreeVariable>> freevarMap);

        vector<types::tyPtr> getTypeEnv();

        ankerl::unordered_dense::map<string, std::pair<int, int>> getClassHierarchy();

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
        bool transformedAST; // Whether the run function was called(used by getTypeEnv)
        // Compiler only ever emits the code for a single function, top level code is considered a function
        CurrentChunkInfo* current;
        std::shared_ptr<ClassChunkInfo> currentClass;

        ESLModule* curUnit;
        int curUnitIndex;
        vector<ESLModule*> units;

        std::unordered_map<AST::FuncLiteral*, vector<closureConversion::FreeVariable>> freevarMap;
        ankerl::unordered_dense::map<string, Globalvar> globals;

        unordered_map<string, std::shared_ptr<ClassChunkInfo>> globalClasses;
        ankerl::unordered_dense::map<string, types::tyVarIdx> nativesTypes;
        // Empty constraint set means type is collapsed
        vector<std::pair<types::tyPtr, vector<std::shared_ptr<types::TypeConstraint>>>> typeEnv;
        ankerl::unordered_dense::map<string, computeClassHierarchy::ClassNode> classNodes;

        vector<typedAST::nodePtr> nodesToReturn;
        typedAST::exprPtr returnedExpr;

        #pragma region Helpers
        // Variables
        // Checks all imports to see if the symbol 'token' is imported
        varPtr checkSymbol(const Token symbol);
        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        varPtr resolveGlobal(const Token symbol, const bool canAssign);
        varPtr declareGlobalVar(const string& name, const AST::ASTDeclType type, const types::tyVarIdx defaultTy);
        void defineGlobalVar(const string& name, AST::VarDeclDebugInfo dbgInfo);

        varPtr declareLocalVar(const AST::ASTVar& name, const types::tyVarIdx defaultTy);
        void defineLocalVar(AST::VarDeclDebugInfo dbgInfo);

        varPtr addLocal(const AST::ASTVar& name, const types::tyVarIdx defaultTy);
        int resolveLocal(const Token name);

        int resolveUpvalue(const Token name);

        typedAST::exprPtr readVar(const Token name);
        typedAST::exprPtr storeToVar(const Token name, const Token op, typedAST::exprPtr toStore);

        std::shared_ptr<typedAST::ScopeEdge> beginScope();
        std::shared_ptr<typedAST::ScopeEdge> endScope();
        // Functions
        std::shared_ptr<typedAST::Function> endFuncDecl();
        void declareFuncArgs(vector<AST::ASTVar>& args);
        types::tyVarIdx createNewFunc(const string name, const int arity, const FuncType fnKind, const bool isClosure);

        // Classes and methods
        typedAST::ClassMethod createMethod(AST::FuncDecl* _method, const Token overrides, const string className,
                                           std::shared_ptr<types::FunctionType> fnTy);
        std::shared_ptr<typedAST::InvokeExpr> tryConvertToInvoke(typedAST::exprPtr callee, vector<typedAST::exprPtr>& args,
                                                                 const Token paren1, const Token paren2);
        void detectDuplicateSymbol(const Token publicName, const bool isMethod, const bool methodOverrides);
        void processMethods(const string className, vector<AST::ClassMethod>& methods,
                            vector<std::shared_ptr<types::FunctionType>>& methodTys);
        std::shared_ptr<typedAST::InstanceofExpr> createInstanceofExpr(typedAST::exprPtr lhs, AST::ASTNodePtr rhs, AST::BinaryExprDebugInfo dbg);

        // Resolve implicit object field access
        std::shared_ptr<typedAST::InstGet> resolveClassFieldRead(const Token name);
        std::shared_ptr<typedAST::InstSet> resolveClassFieldStore(const Token name, typedAST::exprPtr toStore, const Token op);
        Globalvar& getClassFromExpr(AST::ASTNodePtr expr);
        std::shared_ptr<ClassChunkInfo> getClassInfoFromExpr(AST::ASTNodePtr expr);

        // Resolve public/private fields when this.field in encountered in code
        std::shared_ptr<typedAST::InstGet> tryResolveThis(AST::FieldAccessExpr* expr);
        std::shared_ptr<typedAST::InstSet> tryResolveThis(AST::SetExpr* expr, typedAST::SetType operationTy);

        // Type stuff
        types::tyVarIdx addType(types::tyPtr ty);
        types::tyVarIdx createEmptyTy();
        void addTypeConstraint(const types::tyVarIdx ty, std::shared_ptr<types::TypeConstraint> constraint);
        types::tyVarIdx getBasicType(const types::TypeFlag ty);
        void addBasicTypes();
        types::tyVarIdx getInstFieldTy(const types::tyVarIdx possibleInstTy, string field);

        // Misc
        Token syntheticToken(const string& str);
        void updateLine(const Token token);
        void error(const Token token, const string& msg) noexcept(false);
        void error(const string& message) noexcept(false);
        vector<std::variant<double, bool, void*, string>> getCaseConstants(vector<Token> constants);

        typedAST::Block parseStmtsToBlock(vector<AST::ASTNodePtr>& stmts);
        typedAST::Block parseStmtToBlock(AST::ASTNodePtr stmt);
        typedAST::exprPtr evalASTExpr(std::shared_ptr<AST::ASTNode> node);
        vector<typedAST::nodePtr> evalASTStmt(std::shared_ptr<AST::ASTNode> node);
        #pragma endregion
    };

}
}