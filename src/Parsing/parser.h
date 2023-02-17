#pragma once
#include "../common.h"
#include "ASTDefs.h"
#include "ASTProbe.h"
#include "MacroExpander.h"
#include <initializer_list>
#include <map>
#include <unordered_map>

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
		OR,
		AND,
		BIN_OR,
		BIN_XOR,
		BIN_AND,
		EQUALITY,
		COMPARISON,
		BITSHIFT,
		SUM,
		FACTOR,
		NOT,
		ALTER,
		ASYNC,
		CALL,
		PRIMARY
	};
	//conversion from enum to 1 byte number
	inline constexpr unsigned operator+ (Precedence const val) { return static_cast<byte>(val); }

	class PrefixParselet {
	public:
		virtual ASTNodePtr parse(Token token) = 0;
		Parser* cur;
		int prec;
	};

	class InfixParselet {
	public:
		virtual ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) = 0;
		Parser* cur;
		int prec;
	};

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
		void parse(vector<CSLModule*>& modules);

	private:
		ASTProbe* probe;
		MacroExpander* macroExpander;

		CSLModule* parsedUnit;

        vector<Token>* currentContainer;
        int currentPtr;

		int loopDepth;
		int switchDepth;

		unordered_map<TokenType, unique_ptr<PrefixParselet>> prefixParselets;
		unordered_map<TokenType, unique_ptr<InfixParselet>> infixParselets;

        // For macro expansions
		unordered_map<string, unique_ptr<Macro>> macros;
        unordered_map<string, unique_ptr<ExprMetaVar>> exprMetaVars;
        unordered_map<string, unique_ptr<TTMetaVar>> ttMetaVars;

		ParseMode parseMode = ParseMode::Standard;

		template<typename ParsletType>
		void addPrefix(TokenType type, Precedence prec);
		template<typename ParsletType>
		void addInfix(TokenType type, Precedence prec);

		void defineMacro();

#pragma region Expressions
		ASTNodePtr expression(int prec);
		ASTNodePtr expression();

		// Parselets that need to have access to private methods of parser
		friend class FieldAccessParselet;
		friend class CallParselet;
		friend class BinaryParselet;
		friend class ConditionalParselet;
		friend class AssignmentParselet;
		friend class LiteralParselet;
		friend class UnaryPrefixParselet;
		friend class UnaryPostfixParselet;

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
		shared_ptr<ASTDecl> varDecl();
		shared_ptr<ASTDecl> funcDecl();
		shared_ptr<ASTDecl> classDecl();

		ASTNodePtr statement();
        shared_ptr<ExprStmt> exprStmt();
		shared_ptr<BlockStmt> blockStmt();
        shared_ptr<IfStmt> ifStmt();
        shared_ptr<WhileStmt> whileStmt();
        shared_ptr<ForStmt> forStmt();
        shared_ptr<BreakStmt> breakStmt();
        shared_ptr<ContinueStmt> continueStmt();
        shared_ptr<SwitchStmt> switchStmt();
		shared_ptr<CaseStmt> caseStmt();
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

		Token consume(TokenType type, string msg);

		ParserException error(Token token, string msg);

		vector<Token> readTokenTree(bool isNonLeaf = true);

		void expandMacros();

		void sync();

		int getPrec();
#pragma endregion

	};

}