#pragma once
#include "../../../AST/ASTDefs.h"
#include "../../../TypedAST/TypedASTDefs.h"
#include "closureConverter.h"
#include "../../../Includes/unorderedDense.h"
#include "../computeClassHierarchy.h"

namespace errorHandler{
    class ErrorHandler;
}

namespace passes{
namespace typedASTParser{
    enum class FuncType {
        TYPE_FUNC,
        TYPE_METHOD,
        TYPE_CONSTRUCTOR,
        TYPE_SCRIPT,
    };

    using varPtr = std::shared_ptr<CFG::VarDecl>;

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

        Upvalue(string _name, std::shared_ptr<CFG::VarDecl> _val) : name(_name), ptr(_val) {}
    };


    // Information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
    struct CurrentChunkInfo {
        // For closures
        CurrentChunkInfo* enclosing;
        std::shared_ptr<CFG::Function> func;
        FuncType type;
        // First ptr is pointer to the VarDecl from an outer function to store to the closure,
        // second is to the VarDecl used inside this function
        vector<std::pair<std::shared_ptr<CFG::VarDecl>, std::shared_ptr<CFG::VarDecl>>> freevarPtrs;

        int line;
        int scopeDepth;
        // Stack can grow an arbitrary amount
        vector<Local> locals;
        vector<Upvalue> freevars;
    private:
        vector<CFG::nodePtr> stmt_antecedents;
        vector<CFG::nodePtr> break_antecedents;
        vector<CFG::nodePtr> continue_antecedents;
        vector<CFG::nodePtr> advance_antecedents;
        struct loop_data {
            vector<CFG::nodePtr> break_antecedents;
            vector<CFG::nodePtr> continue_antecedents;
        };
        struct switch_data {
            vector<CFG::nodePtr> break_antecedents;
            vector<CFG::nodePtr> advance_antecedents;
        };
    public:
        vector<CFG::nodePtr>&& get_stmt() {
            return std::move(stmt_antecedents);
        }
        vector<CFG::nodePtr>&& get_advance() {
            return std::move(advance_antecedents);
        }
        loop_data prep_loop() {
            return {std::move(break_antecedents), std::move(continue_antecedents) };
        }
        vector<CFG::nodePtr> finish_loop(loop_data& data) {
            stmt_antecedents.insert(stmt_antecedents.end(), continue_antecedents.begin(), continue_antecedents.end());
            auto tmp = stmt_antecedents;
            stmt_antecedents = break_antecedents;
            break_antecedents = std::move(data.break_antecedents);
            continue_antecedents = std::move(data.continue_antecedents);
            return std::move(tmp);
        }
        switch_data prep_switch() {
            return { std::move(break_antecedents), std::move(advance_antecedents) };
        }
        void finish_switch(switch_data& data) {
            stmt_antecedents = break_antecedents;
            stmt_antecedents.insert(stmt_antecedents.end(), advance_antecedents.begin(), advance_antecedents.end());
            break_antecedents = std::move(data.break_antecedents);
            advance_antecedents = std::move(data.advance_antecedents);
        }

        void set_stmt(vector<CFG::nodePtr> cur) {
            stmt_antecedents = std::move(cur);
        }

        void add_stmt(CFG::nodePtr cur) {
            stmt_antecedents.push_back(cur);
        }
        void add_break(CFG::nodePtr cur) {
            break_antecedents.push_back(cur);
        }
        void add_continue(CFG::nodePtr cur) {
            continue_antecedents.push_back(cur);
        }
        void add_advance(CFG::nodePtr cur) {
            advance_antecedents.push_back(cur);
        }
        template<class NodeTy>
        inline NodeTy set_antecedents(NodeTy stmt) {
            stmt->antecedents = std::move(get_stmt());
            add_stmt(stmt);
            return stmt;
        }
        CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, string funcName);
    };

    struct ClassChunkInfo {
        // Privates are prefixed with "priv."
        // Int is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, int> fields;
        std::unordered_map<string, std::pair<CFG::ClassMethod, int>> methods;
        std::shared_ptr<types::ClassType> classTy;
        const string mangledName;

        std::shared_ptr<ClassChunkInfo> parent;

        ClassChunkInfo(string _name, std::shared_ptr<types::ClassType> _classTy)
            : mangledName(_name){
            parent = nullptr;
            classTy = _classTy;
        }

        void inherit(std::shared_ptr<ClassChunkInfo> _parent){
            fields = _parent->fields;
            parent = _parent;
        }
    };

    struct TransformerException {

    };

    struct Globalvar {
        varPtr valPtr;
        bool isDefined;

        Globalvar(std::shared_ptr<CFG::VarDecl> _val) {
            valPtr = _val;
            isDefined = false;
        }
    };

    class ASTTransformer : public AST::Visitor {
    public:
        // Passed to other passes, used for highlighting errors
        vector<File*> sourceFiles;
        bool hadError;

        ASTTransformer(vector<AST::ASTModule> &_units, errorHandler::ErrorHandler& errHandler);
        std::pair<std::shared_ptr<CFG::Function>, vector<File*>>
        run(std::unordered_map<AST::FuncLiteral*, vector<closureConversion::FreeVariable>> freevarMap);

        ankerl::unordered_dense::map<string, std::pair<int, int>> getClassHierarchy();
        ankerl::unordered_dense::map<string, types::tyPtr>& getNativeFuncTypes();

#pragma region Visitor pattern
        void visitAssignmentExpr(AST::AssignmentExpr* expr) override;
        void visitSetExpr(AST::SetExpr* expr) override;
        void visitConditionalExpr(AST::ConditionalExpr* expr) override;
        void visitBinaryExpr(AST::BinaryExpr* expr) override;
        void visitUnaryExpr(AST::UnaryExpr* expr) override;
        void visitCallExpr(AST::CallExpr* expr) override;
        void visitNewExpr(AST::NewExpr* expr) override;
        void visitFieldAccessExpr(AST::FieldAccessExpr* expr) override;
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
        void visitSpawnStmt(AST::SpawnStmt* stmt) override;
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


        vector<AST::ASTModule>& units;
        int curUnitIndex;

        std::unordered_map<AST::FuncLiteral*, vector<closureConversion::FreeVariable>> freevarMap;
        ankerl::unordered_dense::map<string, Globalvar> globals;

        unordered_map<string, std::shared_ptr<ClassChunkInfo>> globalClasses;
        ankerl::unordered_dense::map<string, types::tyPtr> nativesTypes;
        ankerl::unordered_dense::map<string, computeClassHierarchy::ClassNode> classNodes;

        vector<CFG::nodePtr> nodesToReturn;
        CFG::exprPtr returnedExpr;

        errorHandler::ErrorHandler& errHandler;

        #pragma region Helpers
        // Variables
        // Checks all imports to see if the symbol 'token' is imported
        varPtr checkSymbol(const Token symbol);
        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        varPtr resolveGlobal(const Token symbol, const bool canAssign);
        varPtr declareGlobalVar(const string& name, const AST::ASTDeclType type);
        void defineGlobalVar(const string& name, AST::VarDeclDebugInfo dbgInfo);

        varPtr declareLocalVar(const AST::ASTVar& name);
        void defineLocalVar(AST::VarDeclDebugInfo dbgInfo);

        varPtr addLocal(const AST::ASTVar& name);
        int resolveLocal(const Token name);

        int resolveUpvalue(const Token name);

        CFG::exprPtr readVar(const Token name);
        CFG::exprPtr storeToVar(const Token name, const Token op, CFG::exprPtr toStore);

        std::shared_ptr<CFG::ScopeEdge> beginScope(Token location);
        std::shared_ptr<CFG::ScopeEdge> endScope(Token location);
        // Functions
        std::shared_ptr<CFG::Function> endFuncDecl(Token endLoc);
        void declareFuncArgs(vector<AST::ASTVar>& args);
        void createNewFunc(const string name, const int arity, const FuncType fnKind);

        // Classes and methods
        CFG::ClassMethod createMethod(AST::FuncDecl* _method, const Token overrideTok, const string className,
                                      std::shared_ptr<types::FunctionType> fnTy);
        std::shared_ptr<CFG::InvokeExpr> tryConvertToInvoke(CFG::exprPtr callee, vector<CFG::exprPtr>& args,
                                                            const Token paren1, const Token paren2);
        void detectDuplicateSymbol(const Token publicName, const bool isMethod, const bool methodOverrides);
        void processMethods(const string className, vector<AST::ClassMethod>& methods,
                            vector<std::shared_ptr<types::FunctionType>>& methodTys);
        std::shared_ptr<CFG::InstanceofExpr> createInstanceofExpr(CFG::exprPtr lhs, AST::ASTNodePtr rhs, AST::BinaryExprDebugInfo dbg);

        // Resolve implicit object field access
        std::shared_ptr<CFG::InstGet> resolveClassFieldRead(const Token name);
        std::shared_ptr<CFG::InstSet> resolveClassFieldStore(const Token name, CFG::exprPtr toStore, const Token op);
        Globalvar& getClassFromExpr(AST::ASTNodePtr expr);
        std::shared_ptr<ClassChunkInfo> getClassInfoFromExpr(AST::ASTNodePtr expr);

        // Resolve public/private fields when this.field in encountered in code
        std::shared_ptr<CFG::InstGet> tryResolveThis(AST::FieldAccessExpr* expr);
        std::shared_ptr<CFG::InstSet> tryResolveThis(AST::SetExpr* expr, CFG::SetType operationTy);

        // Misc
        Token syntheticToken(const string& str);
        void error(const Token token, const string& msg) noexcept(false);
        void error(const string& message) noexcept(false);
        vector<std::variant<double, bool, void*, string>> getCaseConstants(vector<Token>& constants);
        string computeFullSymbol(string symbol, int moduleIndex);

        CFG::Block parseStmtsToBlock(vector<AST::ASTNodePtr>& stmts);
        CFG::Block parseStmtToBlock(AST::ASTNodePtr stmt);
        CFG::exprPtr evalASTExpr(std::shared_ptr<AST::ASTNode> node);
        vector<CFG::nodePtr> evalASTStmt(std::shared_ptr<AST::ASTNode> node);
        void createNativeFn(string name, int arity, types::tyPtr retTy);
        void declareNativeFunctions();
        #pragma endregion
    };

}
}