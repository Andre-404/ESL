#pragma once
#include "../AST/ASTProbe.h"
#include "macroExpander.h"
#include <initializer_list>

namespace AST {
	using std::unique_ptr;
	class Parser;
    class Macro;
    class MacroExpr;
    class MacroExpander;
    class MatchPattern;
    class ExprMetaVar;
    class TTMetaVar;
    class ASTPrinter;

	enum class Precedence {
		NONE,
		ASSIGNMENT,
		CONDITIONAL,
        RANGE,
		OR,
		AND,
        COMPARISON,
		BIN_OR,
		BIN_XOR,
		BIN_AND,
		BITSHIFT,
		SUM,
		FACTOR,
        UNARY_PREFIX,
        UNARY_POSTFIX,
		CALL,
        INSTANCEOF,
		PRIMARY
	};
	// Conversion from enum to 1 byte number
	inline constexpr unsigned operator+ (Precedence const val) { return static_cast<byte>(val); }

    using PrefixFunc = ASTNodePtr(*)(Parser* parser, Token token);
    using InfixFunc = ASTNodePtr(*)(Parser* parser, ASTNodePtr left, Token token);

    struct ParserException : public std::exception {
        bool macroRecursionLimitExceeded = false;
    };

    enum class ParseMode{
        Standard,
        Macro,
        Matcher
    };

	class Parser {
	public:
		Parser();
		void parse(vector<ESLModule*>& modules);
        void verifySymbolImports(vector<ESLModule*>& modules);
        void highlight(vector<ESLModule*>& modules, string moduleToHighlight);
	private:
		ASTProbe* probe;
		MacroExpander* macroExpander;

		ESLModule* parsedUnit;

        vector<Token>* currentContainer;
        int currentPtr;

		int loopDepth;
		int switchDepth;

		unordered_map<TokenType, std::pair<int, PrefixFunc>> prefixParselets;
		unordered_map<TokenType, std::pair<int, InfixFunc>> infixParselets;
        // Postfix parselets use infix funcs since those are effectively the same as infix
        unordered_map<TokenType, std::pair<int, InfixFunc>> postfixParselets;
        int prefixPrecLevel(const TokenType type);
        int infixPrecLevel(const TokenType type);
        int postfixPrecLevel(const TokenType type);

        // For macro expansions
		unordered_map<string, unique_ptr<Macro>> macros;
        unordered_map<string, unique_ptr<ExprMetaVar>> exprMetaVars;
        unordered_map<string, unique_ptr<TTMetaVar>> ttMetaVars;

		ParseMode parseMode = ParseMode::Standard;

		void addPrefix(const TokenType type, const Precedence prec, const PrefixFunc func);
		void addInfix(const TokenType type, const Precedence prec, const InfixFunc func);
        void addPostfix(const TokenType type, const Precedence prec, const InfixFunc func);

		void defineMacro();

        #pragma region Expressions
		ASTNodePtr expression(const int prec);
		ASTNodePtr expression();

		// Parselets that need to have access to private methods of parser
        friend ASTNodePtr parsePrefix(Parser* parser, const Token token);
        friend ASTNodePtr parseLiteral(Parser* parser, const Token token);
        friend ASTNodePtr parseAssignment(Parser* parser, ASTNodePtr left, const Token token);
        friend ASTNodePtr parseConditional(Parser* parser, ASTNodePtr left, const Token token);
		friend ASTNodePtr parseBinary(Parser* parser, ASTNodePtr left, const Token token);
		friend ASTNodePtr parsePostfix(Parser* parser, ASTNodePtr left, const Token token);
		friend ASTNodePtr parseCall(Parser* parser, ASTNodePtr left, const Token token);
		friend ASTNodePtr parseFieldAccess(Parser* parser, ASTNodePtr left, const Token token);

		// Macro expander needs to be able to parse additional tokens and report errors
		friend class MacroExpander;

        // Macro should be able to report errors in expansion
        friend class Macro;

        // Match pattern needs to be able to attempt to generate an expression
        friend class MatchPattern;

        #pragma endregion

        #pragma region Statements
		ASTNodePtr topLevelDeclaration();
		ASTNodePtr localDeclaration();
		shared_ptr<VarDecl> varDecl();
		shared_ptr<FuncDecl> funcDecl();
		shared_ptr<ClassDecl> classDecl();

		ASTNodePtr statement();
        shared_ptr<ExprStmt> exprStmt();
        shared_ptr<BlockStmt> blockStmt();
        shared_ptr<IfStmt> ifStmt();
        shared_ptr<WhileStmt> whileStmt();
        shared_ptr<ForStmt> forStmt();
        shared_ptr<BreakStmt> breakStmt();
        shared_ptr<ContinueStmt> continueStmt();
        shared_ptr<SwitchStmt> switchStmt();
		shared_ptr<CaseStmt> caseStmt(vector<Token> constants);
        shared_ptr<AdvanceStmt> advanceStmt();
        shared_ptr<ReturnStmt> returnStmt();

        #pragma endregion

        #pragma region Helpers

		bool match(const std::initializer_list<TokenType>& tokenTypes);

		bool match(const TokenType type);

		bool isAtEnd();

        bool check(const std::initializer_list<TokenType>& tokenTypes);

		bool check(const TokenType type);

		Token advance();

		Token peek();

		Token peekNext();

		Token previous();

		Token consume(const TokenType type, const string msg);

		ParserException error(const Token token, const string msg);

		vector<Token> readTokenTree(bool isNonLeaf = true);

		void expandMacros();

		void sync();

        void checkDependency(ESLModule* unit, Dependency& dep, std::unordered_map<string, Dependency*>& symbols,
                             std::unordered_map<string, Dependency*>& aliases);
        #pragma endregion

	};

}