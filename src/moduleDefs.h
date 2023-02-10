#pragma once
#include "common.h"
#include <memory>

enum class TokenType {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    LEFT_BRACKET, RIGHT_BRACKET,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR, PERCENTAGE,
    QUESTIONMARK, COLON, TILDA, DOLLAR,
    // One or two character tokens.
    BITWISE_AND, BITWISE_OR, BITWISE_XOR,
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL, PLUS_EQUAL, MINUS_EQUAL, STAR_EQUAL, SLASH_EQUAL,
    PERCENTAGE_EQUAL, BITWISE_AND_EQUAL, BITWISE_OR_EQUAL, BITWISE_XOR_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    BITSHIFT_LEFT, BITSHIFT_RIGHT,
    INCREMENT, DECREMENT, DOUBLE_COLON, ARROW,
    // Literals.
    IDENTIFIER, STRING, NUMBER,
    // Keywords.
    AND, OR,
    NIL, FALSE, TRUE,
    IF, ELSE,
    FUNC, RETURN,
    WHILE, FOR, CONTINUE, BREAK, ADVANCE,
    CLASS, THIS, SUPER,
    SWITCH, CASE, DEFAULT,
    VAR,
    IMPORT, EXPORT, AS,
    AWAIT, ASYNC, ADDMACRO, EXPR, TT,

    NEWLINE, ERROR, NONE
};

struct File {
    //file name
    string name;
    string sourceFile;
    //number that represents start of each line in the source string
    std::vector<uInt> lines;
    File(string& src, string& _name) : sourceFile(src), name(_name) {};
    File() = default;
};

//span of characters in a source file of code
struct Span {
    //sourceFile.lines[line - 1] + column is the start of the string
    int line = 0;
    int column = 0;
    int length = 0;

    File* sourceFile = nullptr;

    Span() = default;
    Span(int _line, int _column, int _len, File* _src) : line(_line), column(_column), length(_len), sourceFile(_src) {};

    // Get string corresponding to this Span
    [[nodiscard]] string getStr() const {
        int start = sourceFile->lines[line] + column;
        return sourceFile->sourceFile.substr(start, length);
    }

    // Get the entire line this span is in.
    string getLine() const {
        int start = sourceFile->lines[line];
        // If this Span is located on the last line of the file, then the line ends at the end of the file.
        int end = (line + 1 >= sourceFile->lines.size()) ? sourceFile->sourceFile.size() : sourceFile->lines[line + 1];

        string line = sourceFile->sourceFile.substr(start, end - start);

        // Remove the '\n' at the end of the line.
        if (line.back() == '\n') line.pop_back();

        return line;
    }
};

struct Token {
    TokenType type;
    Span str;

    //for things like synthetic tokens and expanded macros
    bool isSynthetic;
    string syntheticStr;
    std::shared_ptr<Token> parentPtr; // Pointer to the token this token originated from
    //default constructor
    Token() {
        isSynthetic = false;
        parentPtr = nullptr;
        type = TokenType::NONE;
    }
    //construct a token from source file string data
    Token(Span _str, TokenType _type) {
        isSynthetic = false;
        parentPtr = nullptr;
        str = _str;
        type = _type;
    }
    //construct a token which doesn't appear in the source file(eg. splitting a += b into a = a + b, where '+' is synthetic)
    Token(TokenType _type, Token parentToken) {
        parentPtr = std::make_shared<Token>(parentToken);
        isSynthetic = true;
        type = _type;
    }
    Token(TokenType _type, string str) {
        isSynthetic = true;
        parentPtr = nullptr;
        type = _type;
        syntheticStr = str;
    }
    string getLexeme() const {
        if (type == TokenType::ERROR) { return "Unexpected character."; }
        else if (isSynthetic) { return syntheticStr; }
        return str.getStr();
    }

    bool equals(const Token& token) const {
        return getLexeme().compare(token.getLexeme()) == 0 && type == token.type;
    }
};

struct CSLModule;

namespace AST {
    class ASTNode;
}

struct Dependency {
    Token alias;
    Token pathString;//for error reporting in the compiler
    CSLModule* module;

    Dependency(Token _alias, Token _pathString, CSLModule* _module) : alias(_alias), pathString(_pathString), module(_module) {};
};

struct CSLModule {
    File* file;
    vector<Token> tokens;
    vector<Dependency> deps;
    //whether the dependency tree of this module has been resolved, if it hasn't and we try to parse
    //this module again we have a circular dependency and an error will be thrown
    bool resolvedDeps;
    //used for toposort once we have resolved all dependencies
    bool traversed;

    //AST of this file
    vector<std::shared_ptr<AST::ASTNode>> stmts;
    //exported declarations
    vector<Token> exports;
    //used by the compiler to look up if a global variable exists since globals are late bound
    vector<Token> topDeclarations;

    CSLModule(vector<Token> _tokens, File* _file) {
        tokens = _tokens;
        file = _file;
        resolvedDeps = false;
        traversed = false;
    };
};
