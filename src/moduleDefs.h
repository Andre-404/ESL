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
    DOUBLE_QUESTIONMARK, DOT_QUESTIONMARK, DOUBLE_BANG,
    // Literals.
    IDENTIFIER, STRING, NUMBER,
    // Keywords.
    AND, OR,
    NIL, FALSE, TRUE,
    IF, ELSE,
    FN, RETURN,
    WHILE, FOR, CONTINUE, BREAK, ADVANCE,
    CLASS, THIS,
    SWITCH, CASE, DEFAULT,
    LET, STATIC,
    IMPORT, PUB, IS, NEW, OVERRIDE, CHECK, AS,
    SPAWN, DEFER,
    ADDMACRO, EXPR, TT,

    NEWLINE, ERROR, NONE
};

struct File {
    string name;
    // Raw src
    string sourceFile;
    string path;
    File(const string _sourceFile, const string& _name, const string _path) : sourceFile(_sourceFile), name(_name), path(_path) {};
    File() = default;

    string getFullPath() { return path + name; }
};

// Span of characters in a source file of code
struct Span {
    //sourceFile.lines[line - 1] + column is the start of the string
    int start;
    int end;

    File* sourceFile = nullptr;

    Span() : start(0), end(0) {};
    Span(int _start, int _end, File* _src) : start(_start), end(_end), sourceFile(_src) {};

    // Get string corresponding to this Span
    [[nodiscard]] string getStr() const {
        return sourceFile->sourceFile.substr(start, end - start);
    }

    // Get the entire line this span is in.
    std::string_view getLine() const {
        int lineStart = start;
        while(lineStart >= 0 && sourceFile->sourceFile[lineStart] != '\n') lineStart--;
        lineStart++;
        int lineEnd = end;
        while(lineEnd < sourceFile->sourceFile.size() && sourceFile->sourceFile[lineEnd] != '\n') lineEnd++;
        lineEnd--;

        return std::string_view(sourceFile->sourceFile.data()+lineStart, lineEnd - lineStart+1);
    }
    int computeLine(){
        int i = start;
        int line = 0;
        while(i >= 0){
            if(sourceFile->sourceFile[i] == '\n') line++;
            i--;
        }
        return line+1;
    }
    int computeColumn(){
        int lineStart = start;
        while(lineStart >= 0 && sourceFile->sourceFile[lineStart] != '\n') lineStart--;
        lineStart++;
        return start - lineStart;
    }
};

struct Token {
    // For things like synthetic tokens and expanded macros
    bool isSynthetic;
    bool isPartOfMacro;
    TokenType type;
    Span str;
    string syntheticStr;
    //default constructor
    Token() {
        isSynthetic = true;
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
        if (type == TokenType::ERROR) return "Unexpected character.";
        else if (isSynthetic) return syntheticStr;
        return str.getStr();
    }
    int getLength(){
        if (type == TokenType::ERROR) return 0;
        else if (isSynthetic) return syntheticStr.size();
        return str.end - str.start;
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

    ESLModule(vector<Token> _tokens, File* _file) {
        static int count = 0;
        tokens = _tokens;
        file = _file;
        resolvedDeps = false;
        traversed = false;
        id = count++;
    };
};