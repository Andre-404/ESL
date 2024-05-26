#include "scanner.h"
#include "../files.h"
#include <iostream>

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
        {"as", TokenType::AS},
        {"await", TokenType::AWAIT},
        {"async", TokenType::ASYNC},
        {"addMacro", TokenType::ADDMACRO},
        {"expr", TokenType::EXPR},
        {"tt", TokenType::TT},
        {"static", TokenType::STATIC},
        {"instanceof", TokenType::INSTANCEOF},
        {"new", TokenType::NEW},
        {"in", TokenType::IN},
        {"as", TokenType::AS},
        {"override", TokenType::OVERRIDE}
};

using namespace preprocessing;

Scanner::Scanner() {
    curFile = nullptr;
    line = 0;
    start = 0;
    current = 0;
    hadError = false;
    curFile = nullptr;
}

vector<Token> Scanner::tokenizeSource(const string path, const string sourceName) {
    // Setup
    curFile = new File(readFile(path), sourceName, path);
    line = 0;
    start = 0;
    current = start;
    curFile->lines.push_back(0);
    hadError = false;
    tokens.clear();

    // Tokenization
    Token token;
    while (!isAtEnd()) {
        consumeWhitespace();
        if (isAtEnd()) break;
        token = scanToken();
        tokens.push_back(token);
    }

    return tokens;
}

void Scanner::reset() {
    curFile = nullptr;
    line = 0;
    start = 0;
    current = 0;
    hadError = false;
    tokens.clear();
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
            // Either .. or ..=
            if(match('.')){
                return makeToken((match('=') ? TokenType::DOUBLE_DOT_EQUAL : TokenType::DOUBLE_DOT));
            }
            // Either . or .?
            return makeToken(match('?') ? TokenType::DOUBLE_QUESTIONMARK : TokenType::DOT);
        }

        case '$': return makeToken(TokenType::DOLLAR);
        case '-': {
            // Negative literal numbers are constants
            if(isdigit(peek())) {
                advance();
                return number();
            }
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
            curFile->lines.push_back(current);
            line++;
            return newLineToken;
    }

    return makeToken(TokenType::ERROR);
}

Token Scanner::makeToken(const TokenType type) {
    Span newSpan(line, start - curFile->lines.back(), current - start, curFile);
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
                        if (peek() == '\n') {
                            line++;
                            curFile->lines.push_back(current);
                        }
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
        if (peek() == '\n') {
            line++;
            curFile->lines.push_back(current);
        }
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
