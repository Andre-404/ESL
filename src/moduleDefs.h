#pragma once
#include "common.h"
#include <memory>

enum class TokenType {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    LEFT_BRACKET, RIGHT_BRACKET,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR, PERCENTAGE, DIV,
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
    DOUBLE_DOT, DOUBLE_QUESTIONMARK, DOT_QUESTIONMARK,
    // Three character tokens
    DOUBLE_DOT_EQUAL,
    // Literals.
    IDENTIFIER, STRING, NUMBER,
    // Keywords.
    AND, OR,
    NIL, FALSE, TRUE,
    IF, ELSE,
    FN, RETURN,
    WHILE, FOR, CONTINUE, BREAK, ADVANCE, IN,
    CLASS, THIS, SUPER,
    SWITCH, CASE, DEFAULT,
    LET, STATIC,
    IMPORT, PUB, AS, INSTANCEOF, NEW, OVERRIDE,
    AWAIT, ASYNC, ADDMACRO, EXPR, TT,
    TRY, CATCH, ERRDEFFER, UNREACHABLE,

    NEWLINE, ERROR, NONE
};

struct File {
    //file name
    string name;
    string sourceFile;
    string path;
    //number that represents start of each line in the source string
    std::vector<uInt> lines;
    File(string _sourceFile, string& _name, string _path) : sourceFile(_sourceFile), name(_name), path(_path) {};
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

        string ln = sourceFile->sourceFile.substr(start, end - start);

        // Remove the '\n' at the end of the line.
        if (ln.back() == '\n') ln.pop_back();

        return ln;
    }
};

struct Token {
    TokenType type;
    Span str;

    //for things like synthetic tokens and expanded macros
    bool isSynthetic;
    string syntheticStr;
    bool isPartOfMacro;
    //default constructor
    Token() {
        isSynthetic = false;
        isPartOfMacro = false;
        type = TokenType::NONE;
    }
    //construct a token from source file string data
    Token(Span _str, TokenType _type) {
        isSynthetic = false;
        isPartOfMacro = false;
        str = _str;
        type = _type;
    }
    Token(TokenType _type, string str) {
        isSynthetic = true;
        isPartOfMacro = true;
        type = _type;
        syntheticStr = str;
    }
    string getLexeme() const {
        if (type == TokenType::ERROR) { return "Unexpected character."; }
        else if (isSynthetic) { return syntheticStr; }
        return str.getStr();
    }

    bool equals(const Token& token) const {
        return type == token.type && getLexeme().compare(token.getLexeme()) == 0;
    }
};

struct ESLModule;

namespace AST {
    class ASTNode;
    class ASTDecl;
}

struct Dependency {
    Token alias;
    Token pathString;// For error reporting in the compiler
    ESLModule* module;

    Dependency(Token _alias, Token _pathString, ESLModule* _module) : alias(_alias), pathString(_pathString), module(_module) {};
};

struct ESLModule {
    File* file;
    vector<Token> tokens;
    vector<Dependency> deps;
    // Whether the dependency tree of this module has been resolved, if it hasn't and we try to parse
    // this module again we have a circular dependency and an error will be thrown
    bool resolvedDeps;
    // Used for topsort once we have resolved all dependencies
    bool traversed;

    int id;

    // AST of this file
    vector<std::shared_ptr<AST::ASTNode>> stmts;
    // Exported declarations
    vector<std::shared_ptr<AST::ASTDecl>> exports;
    // Used by the compiler to look up if a global variable exists since globals are late bound
    vector<std::shared_ptr<AST::ASTDecl>> topDeclarations;

    static int count;

    ESLModule(vector<Token> _tokens, File* _file) {
        tokens = _tokens;
        file = _file;
        resolvedDeps = false;
        traversed = false;
        id = count++;
    };
};