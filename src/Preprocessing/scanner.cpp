#include "scanner.h"
#include "../files.h"
#include <iostream>
#include <filesystem>

// Map to convert keywords in string form to their corresponding TokenType
std::unordered_map<string, TokenType> keywordToTokenType = {
        {"and", TokenType::AND},
        {"break", TokenType::BREAK},
        {"class", TokenType::CLASS},
        {"case", TokenType::CASE},
        {"continue", TokenType::CONTINUE},
        {"default", TokenType::DEFAULT},
        {"else", TokenType::ELSE},
        {"pub", TokenType::PUB},
        {"if", TokenType::IF},
        {"import", TokenType::IMPORT},
        {"as", TokenType::AS},
        {"null", TokenType::NIL},
        {"advance", TokenType::ADVANCE},
        {"or", TokenType::OR},
        {"return", TokenType::RETURN},
        {"switch", TokenType::SWITCH},
        {"let", TokenType::LET},
        {"while", TokenType::WHILE},
        {"false", TokenType::FALSE},
        {"for", TokenType::FOR},
        {"fn", TokenType::FN},
        {"this", TokenType::THIS},
        {"true", TokenType::TRUE},
        {"addMacro", TokenType::ADDMACRO},
        {"expr", TokenType::EXPR},
        {"tt", TokenType::TT},
        {"static", TokenType::STATIC},
        {"is", TokenType::IS},
        {"new", TokenType::NEW},
        {"spawn", TokenType::SPAWN},
        {"override", TokenType::OVERRIDE},
        {"check", TokenType::CHECK},
        {"defer", TokenType::DEFER},
};

using namespace preprocessing;

Scanner::Scanner() {
    curFile = nullptr;
    start = 0;
    current = 0;
    hadError = false;
    curFile = nullptr;
}

vector<Token> Scanner::tokenizeSource(const string path, const string sourceName) {
    // Setup
    curFile = new File(readFile(path), sourceName, std::filesystem::path(path).parent_path().generic_string());
    start = 0;
    current = start;
    hadError = false;

    // Tokenization
    Token token;
    vector<Token> tokens;
    while (!isAtEnd()) {
        consumeWhitespace();
        if (isAtEnd()) break;
        token = scanToken();
        tokens.push_back(token);
    }

    return tokens;
}

#pragma region Helpers
bool Scanner::isAtEnd() {
    return current >= curFile->sourceFile.size();
}

// Checks if a given index is contained within the file (Is non-negative and less than file size)
bool Scanner::isIndexInFile(const int index) {
    return 0 <= index && index < curFile->sourceFile.size();
}

// If matched consume the character
bool Scanner::match(const char expected) {
    if (isAtEnd()) return false;
    if (curFile->sourceFile[current] != expected) return false;
    current++;
    return true;
}

char Scanner::advance() {
    return curFile->sourceFile[current++];
}

Token Scanner::scanToken() {
    start = current;

    char c = advance();
    // Identifiers start with _ or [a-z][A-Z]
    if (isdigit(c)) return number();
    if (isalpha(c) || c == '_') return identifier();

    switch (c) {
        case '(': return makeToken(TokenType::LEFT_PAREN);
        case ')': return makeToken(TokenType::RIGHT_PAREN);
        case '{': return makeToken(TokenType::LEFT_BRACE);
        case '}': return makeToken(TokenType::RIGHT_BRACE);
        case '[': return makeToken(TokenType::LEFT_BRACKET);
        case ']': return makeToken(TokenType::RIGHT_BRACKET);
        case ';': return makeToken(TokenType::SEMICOLON);
        case ',': return makeToken(TokenType::COMMA);
        case '.': {
            // Either . or .?
            return makeToken(match('?') ? TokenType::DOT_QUESTIONMARK : TokenType::DOT);
        }

        case '$': return makeToken(TokenType::DOLLAR);
        case '-': {
            return makeToken(match('=') ? TokenType::MINUS_EQUAL : match('-') ? TokenType::DECREMENT : TokenType::MINUS);
        }
        case '+': return makeToken(match('=') ? TokenType::PLUS_EQUAL : match('+') ? TokenType::INCREMENT : TokenType::PLUS);
        case '/': return makeToken(match('=') ? TokenType::SLASH_EQUAL : TokenType::SLASH);
        case '*': return makeToken(match('=') ? TokenType::STAR_EQUAL : TokenType::STAR);
        case '&': return makeToken(match('=') ? TokenType::BITWISE_AND_EQUAL : match('&') ? TokenType::AND : TokenType::BITWISE_AND);
        case '|': return makeToken(match('=') ? TokenType::BITWISE_OR_EQUAL : match('|') ? TokenType::OR : TokenType::BITWISE_OR);
        case '^': return makeToken(match('=') ? TokenType::BITWISE_XOR_EQUAL : TokenType::BITWISE_XOR);
        case '%': return makeToken(match('=') ? TokenType::PERCENTAGE_EQUAL : TokenType::PERCENTAGE);
        case '~': return makeToken(TokenType::TILDA);
        case '!': {
            TokenType t = TokenType::BANG;
            if(match('!')) t = TokenType::DOUBLE_BANG;
            else if(match('=')) t = TokenType::BANG_EQUAL;

            return makeToken(t);
        }
        case '=': return makeToken(match('=') ? TokenType::EQUAL_EQUAL : (match('>') ? TokenType::ARROW : TokenType::EQUAL));
        case '<': return makeToken(match('=') ? TokenType::LESS_EQUAL : match('<') ? TokenType::BITSHIFT_LEFT : TokenType::LESS);
        case '>': return makeToken(match('=') ? TokenType::GREATER_EQUAL : match('>') ? TokenType::BITSHIFT_RIGHT : TokenType::GREATER);
        case '"': return string_();
        case ':': return makeToken(match(':') ? TokenType::DOUBLE_COLON : TokenType::COLON);
        case '?': return makeToken(match('?') ? TokenType::DOUBLE_QUESTIONMARK : TokenType::QUESTIONMARK);
        case '\n':
            Token newLineToken = makeToken(TokenType::NEWLINE);
            return newLineToken;
    }

    return makeToken(TokenType::ERROR);
}

Token Scanner::makeToken(const TokenType type) {
    Span newSpan(start, current, curFile);
    Token token(newSpan, type);
    return token;
}

char Scanner::peek() {
    if (isAtEnd()) return '\0';
    return curFile->sourceFile[current];
}

char Scanner::peekNext() {
    if (!isIndexInFile(current+1)) return '\0';
    return curFile->sourceFile[current + 1];
}

void Scanner::consumeWhitespace() {
    while (true) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '/':
                // Standard comment
                if (peekNext() == '/') while (!isAtEnd() && peek() != '\n') advance();
                    // Multi-line comment
                else if (peekNext() == '*') {
                    advance();
                    advance();
                    while (!isAtEnd() && !(peek() == '*' && peekNext() == '/')) {
                        advance();
                    }
                    if (!isAtEnd()) {
                        advance();
                        advance();
                    }
                }
                else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

Token Scanner::string_() {
    while (!isAtEnd()) {
        if (peek() == '"') break;
        advance();
    }

    if (isAtEnd()) return makeToken(TokenType::ERROR);

    // The closing quote.
    advance();
    return makeToken(TokenType::STRING);
}

Token Scanner::number() {
    while (isdigit(peek())) advance();

    // Look for a fractional part.
    if (peek() == '.' && isdigit(peekNext())) {
        // Consume the ".".
        advance();

        while (isdigit(peek())) advance();
    }

    return makeToken(TokenType::NUMBER);
}

Token Scanner::identifier() {
    // First character of the identifier has to be alphabetical, rest can be alphanumerical and '_'
    while (isalnum(peek()) || peek() == '_') advance();
    return makeToken(identifierType());
}

TokenType Scanner::identifierType() {
    string tokenString = curFile->sourceFile.substr(start, current - start);

    // Language keyword
    if (keywordToTokenType.contains(tokenString)) return keywordToTokenType[tokenString];

    // Variable name
    return TokenType::IDENTIFIER;
}

bool Scanner::checkKeyword(const int keywordOffset, const string keyword) {
    if (!isIndexInFile(start + keywordOffset + keyword.length())) return false;
    if (curFile->sourceFile.substr(start + keywordOffset, keyword.length()) == keyword) {
        return true;
    }
    return false;
}
#pragma endregion
