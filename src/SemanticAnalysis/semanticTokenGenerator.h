#include "../common.h"
#include "../Parsing/ASTDefs.h"
#include "../Parsing/parser.h"


namespace SemanticAnalysis {
    enum class FuncType {
        TYPE_FUNC,
        TYPE_METHOD,
        TYPE_CONSTRUCTOR,
        TYPE_SCRIPT,
    };

    struct Local {
        string name = "";
        int depth = -1;
        bool isLocalUpvalue = false;//whether this local variable has been captured as an upvalue
        bool isFunctionParameter = false;
    };

    struct Upvalue {
        uint8_t index = 0;
        bool isLocal = false;
    };

    struct ClassChunkInfo {
        Token name;
        std::unordered_map<string, bool> fields;
        std::shared_ptr<ClassChunkInfo> superclass;

        ClassChunkInfo(Token _name) : name(_name) {
            superclass = nullptr;
        };
    };


    //information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
    struct CurrentChunkInfo {
        //for closures
        CurrentChunkInfo *enclosing;
        FuncType type;
        //locals
        Local locals[LOCAL_MAX];
        uInt localCount;
        uInt scopeDepth;
        std::array<Upvalue, UPVAL_MAX> upvalues;
        bool hasCapturedLocals;

        CurrentChunkInfo(CurrentChunkInfo *_enclosing, FuncType _type);
    };

    enum class GlobalvarType {
        VARIABLE,
        FUNCTION,
        CLASS,
        NONE
    };

    struct GlobalVar {
        Token name;
        GlobalvarType type;
        //If type is CLASS ptr points to the class(used for inheritance)
        std::shared_ptr<ClassChunkInfo> ptr;

        GlobalVar() {
            type = GlobalvarType::NONE;
            ptr = nullptr;
        }

        GlobalVar(Token _name) {
            name = _name;
            type = GlobalvarType::NONE;
            ptr = nullptr;
        }
    };

    struct SemanticAnalyzerException {
    };

    struct SemanticToken {
        int line = 0;
        int start = 0;
        int length = 0;

        string type;
        vector<string> modifiers;

        SemanticToken(int _line, int _column, int _length, string _type, vector<string> _modifiers = vector<string>()) {
            line = _line;
            start = _column;
            length = _length;
            type = _type;
            modifiers = _modifiers;
        }

        SemanticToken(Token token, string _type, vector<string> _modifiers = vector<string>()) {
            line = token.str.line;
            start = token.str.column;
            length = token.str.length;
            type = _type;
            modifiers = _modifiers;
        }

        string toJSON(){
            string final = "{";
            final += fmt::format("\"line\": {}, \"start\": {}, \"length\": {}, \"type\": \"{}\"",
                                 line, start, length, type);
            final += ",\"modifiers\": [";
            for(auto str : modifiers){
                final += "\"" + str + "\",";
            }
            if(modifiers.size() > 0) final.pop_back();
            final += "]";
            final += "}";
            return final;
        }
    };

    struct DiagnosticRelatedInfo{
        string message;
        int line, start, length;
        DiagnosticRelatedInfo(){
            line = 0;
            start = 0;
            length = 0;
        }
        DiagnosticRelatedInfo(Token token, string _message){
            line = token.str.line;
            start = token.str.column;
            length = token.str.length;
            message = _message;
        }

        string toJSON(){
            return "{" + fmt::format("\"line\": {}, \"start\": {}, \"end\": {}, \"message\": {}", line, start, start + length, message) + "}";
        }
    };

    struct Diagnostic{
        int line, start, length;
        int code;

        string message;
        string severity;
        string path;
        vector<DiagnosticRelatedInfo> relatedInfo;
        Diagnostic(){
            line = 0;
            start = 0;
            length = 0;
            code = 0;
            severity = "error";
            path = "";
        }
        Diagnostic(Token token, string _message, int _code){
            line = token.str.line;
            start = token.str.column;
            length = token.str.length;
            code = _code;
            severity = "error";
            message = _message;
            path = token.str.sourceFile->path;
        }
        string toJSON(){
            string final = "{";
            final += fmt::format("\"path\": \"{}\",\"line\": {}, \"start\": {}, \"end\": {}, \"message\": \"{}\", \"code\": {}, \"severity\": \"{}\"",
                                 path, line, start, start + length, message, code, severity);
            final += ",\"relatedInformation\": [";
            for(DiagnosticRelatedInfo& info : relatedInfo){
                final += info.toJSON() + ",";
            }
            if(relatedInfo.size() > 0) final.pop_back();
            final += "]";
            final += "}";
            return final;
        }
    };

    // Generates diagnostics for entire project and semantic tokens on a file basis
    class SemanticAnalyzer : public AST::Visitor {
    public:

        CurrentChunkInfo *current;

        SemanticAnalyzer();

        string highlight(vector<CSLModule *> &units, CSLModule* unitToHighlight);

        string generateDiagnostics(vector<CSLModule *> &units);

        #pragma region Visitor pattern

        void visitAssignmentExpr(AST::AssignmentExpr *expr) override;

        void visitSetExpr(AST::SetExpr *expr) override;

        void visitConditionalExpr(AST::ConditionalExpr *expr) override;

        void visitRangeExpr(AST::RangeExpr *expr) override;

        void visitBinaryExpr(AST::BinaryExpr *expr) override;

        void visitUnaryExpr(AST::UnaryExpr *expr) override;

        void visitCallExpr(AST::CallExpr *expr) override;

        void visitNewExpr(AST::NewExpr *expr) override;

        void visitFieldAccessExpr(AST::FieldAccessExpr *expr) override;

        void visitAsyncExpr(AST::AsyncExpr *expr) override;

        void visitAwaitExpr(AST::AwaitExpr *expr) override;

        void visitArrayLiteralExpr(AST::ArrayLiteralExpr *expr) override;

        void visitStructLiteralExpr(AST::StructLiteral *expr) override;

        void visitLiteralExpr(AST::LiteralExpr *expr) override;

        void visitSuperExpr(AST::SuperExpr *expr) override;

        void visitFuncLiteral(AST::FuncLiteral *expr) override;

        void visitModuleAccessExpr(AST::ModuleAccessExpr *expr) override;

        void visitMacroExpr(AST::MacroExpr *expr) override;

        void visitVarDecl(AST::VarDecl *decl) override;

        void visitFuncDecl(AST::FuncDecl *decl) override;

        void visitClassDecl(AST::ClassDecl *decl) override;

        void visitExprStmt(AST::ExprStmt *stmt) override;

        void visitBlockStmt(AST::BlockStmt *stmt) override;

        void visitIfStmt(AST::IfStmt *stmt) override;

        void visitWhileStmt(AST::WhileStmt *stmt) override;

        void visitForStmt(AST::ForStmt *stmt) override;

        void visitBreakStmt(AST::BreakStmt *stmt) override;

        void visitContinueStmt(AST::ContinueStmt *stmt) override;

        void visitSwitchStmt(AST::SwitchStmt *stmt) override;

        void visitCaseStmt(AST::CaseStmt *_case) override;

        void visitAdvanceStmt(AST::AdvanceStmt *stmt) override;

        void visitReturnStmt(AST::ReturnStmt *stmt) override;

        #pragma endregion
    private:
        CSLModule *curUnit;
        int curUnitIndex;
        int curGlobalIndex;
        vector<CSLModule *> units;

        vector<GlobalVar> globals;
        std::shared_ptr<ClassChunkInfo> currentClass;

        vector<SemanticToken> semanticTokens;
        // Analyzer needs to run through all files to understand the code, but it only returns semantic tokens for the requested file
        bool generateSemanticTokens;

        vector<Diagnostic> diagnostics;

        #pragma region Helpers

        void createSemanticToken(Token token, string type, std::vector<string> modifiers = std::vector<string>());

        void namedVar(Token name, bool canAssign);

        // Locals
        void declareLocalVar(AST::ASTVar &name, bool isParam);

        void defineLocalVar();

        uint16_t declareGlobalVar(Token name);

        void defineGlobalVar(uInt16 name, GlobalvarType type, Token token);

        void addLocal(AST::ASTVar name, bool isParam);

        int resolveLocal(Token name);

        int resolveLocal(CurrentChunkInfo *func, Token name);

        int resolveUpvalue(CurrentChunkInfo *func, Token name);

        int addUpvalue(CurrentChunkInfo *func, byte index, bool isLocal);

        void beginScope();

        void endScope();

        // Classes and methods
        void method(AST::FuncDecl *_method, Token className);

        bool invoke(AST::CallExpr *expr);

        string resolveClassField(Token name, bool canAssign);

        void resolveSuperClassField(Token name);

        std::shared_ptr<ClassChunkInfo> getClassFromExpr(AST::ASTNodePtr expr);

        // Resolve public/private fields when this.object_field in encountered
        bool resolveThis(AST::FieldAccessExpr *expr);

        bool resolveThis(AST::SetExpr *expr);

        bool resolveImplicitObjectField(AST::CallExpr *expr);

        SemanticAnalyzerException error(Token token, const string &msg) noexcept(false);

        // Checks all imports to see if the symbol 'token' is imported
        int checkSymbol(Token token);

        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        int resolveGlobal(Token token, bool canAssign);

        // Given a token for module alias and a token for variable name, returns correct symbol to use
        uInt resolveModuleVariable(Token moduleAlias, Token variable);

        #pragma endregion
    };
}