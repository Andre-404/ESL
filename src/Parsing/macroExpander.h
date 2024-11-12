#pragma once
#include "../AST/ASTDefs.h"
#include "../Includes/fmt/format.h"
#include <map>
#include <queue>
#include <iterator>
#include <unordered_set>

#define MACRO_RECURSION_DEPTH 128

namespace AST {
    class Parser;
    class ASTPrinter;

    enum class TransitionType {
        None,
        ConsumeExpr,
        ConsumeTT,
        LoopBegin,
        LoopIterate,
        LoopEnd
    };

    enum class MatcherTokenType {
        Neutral,
        Iterate,
        Skippable,
        LoopEnd,
        LoopBegin,
        Ignore
    };

    struct Transition {
        // i - position in arguments, j - position in pattern
        int i = 0, j = 0;
        TransitionType type = TransitionType::None;

        Transition() = default;
        Transition(int _i, int _j, TransitionType _type){
            i = _i;
            j = _j;
            type = _type;
        }
    };

    struct ExprMetaVar {
        std::map<vector<int>, ASTNodePtr> values;
        vector<ASTNodePtr> valueList;
        int loopDepth = 0;
        int current = 0;

        ExprMetaVar() {};
        ASTNodePtr get() {
            return valueList[current++];
        }
    };

    struct TTMetaVar {
        std::map<vector<int>, vector<Token>> values;
        int loopDepth = 0;

        TTMetaVar() {};
    };

    class MatchPattern {
    private:
        vector<Token> pattern;
        vector<int> topoSort; // Order in which to process pattern indices
        vector<int> loopJumps; // Marks where to jump when encountering macro loops
        vector<MatcherTokenType> tokenTypes; // Marks types of tokens in the matcher pattern
        Parser* parser;

        void processPattern();

    public:
        MatchPattern(vector<Token> _pattern, Parser* _parser) {
            pattern = _pattern;
            parser = _parser;
            tokenTypes = vector<MatcherTokenType>(pattern.size(), MatcherTokenType::Neutral);
            loopJumps = vector<int>(pattern.size(), -1);
            processPattern();
        }

        bool interpret(vector<Token>& args) const;

        vector<Token> getPattern() { return pattern; }
    };

    class Macro {
    private:
        Parser* parser;
    public:
        Token name;
        vector<MatchPattern> matchers;
        vector<vector<Token>> transcribers;

        Macro();
        Macro(Token _name, Parser* _parser);

        ASTNodePtr expand(vector<Token>& args, const Token& callerToken);
        vector<Token> expandLoops(vector<Token>& readFrom, int begin, int end, vector<int> loopIndices);
    };

    // Used for expanding macros in the AST
    class MacroExpander : public Visitor {
    private:
        int recursionDepth = 0;
        Parser* parser;
        ASTNodePtr expansion = nullptr;

    public:
        explicit MacroExpander(Parser* _parser);

        void expand(ASTNodePtr& node);

        void visitAssignmentExpr(AssignmentExpr* expr) override;
        void visitSetExpr(SetExpr* expr) override;
        void visitConditionalExpr(ConditionalExpr* expr) override;
        void visitBinaryExpr(BinaryExpr* expr) override;
        void visitUnaryExpr(UnaryExpr* expr) override;
        void visitCallExpr(CallExpr* expr) override;
        void visitNewExpr(NewExpr* expr) override;
        void visitFieldAccessExpr(FieldAccessExpr* expr) override;
        void visitArrayLiteralExpr(ArrayLiteralExpr* expr) override;
        void visitStructLiteralExpr(StructLiteral* expr) override;
        void visitLiteralExpr(LiteralExpr* expr) override;
        void visitFuncLiteral(FuncLiteral* expr) override;
        void visitModuleAccessExpr(ModuleAccessExpr* expr) override;
        void visitMacroExpr(MacroExpr* expr) override;

        void visitVarDecl(VarDecl* decl) override;
        void visitFuncDecl(FuncDecl* decl) override;
        void visitClassDecl(ClassDecl* decl) override;

        void visitExprStmt(ExprStmt* stmt) override;
        void visitSpawnStmt(SpawnStmt* stmt) override;
        void visitBlockStmt(BlockStmt* stmt) override;
        void visitIfStmt(IfStmt* stmt) override;
        void visitWhileStmt(WhileStmt* stmt) override;
        void visitForStmt(ForStmt* stmt) override;
        void visitBreakStmt(BreakStmt* stmt) override;
        void visitContinueStmt(ContinueStmt* stmt) override;
        void visitSwitchStmt(SwitchStmt* stmt) override;
        void visitCaseStmt(CaseStmt* _case) override;
        void visitAdvanceStmt(AdvanceStmt* stmt) override;
        void visitReturnStmt(ReturnStmt* stmt) override;
    };
}