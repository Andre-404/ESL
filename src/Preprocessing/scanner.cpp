#include "scanner.h"
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
	{"export", TokenType::EXPORT},
	{"if", TokenType::IF},
	{"import", TokenType::IMPORT},
	{"nil", TokenType::NIL},
	{"advance", TokenType::ADVANCE},
	{"or", TokenType::OR},
	{"print", TokenType::PRINT},
	{"return", TokenType::RETURN},
	{"super", TokenType::SUPER},
	{"switch", TokenType::SWITCH},
	{"var", TokenType::VAR},
	{"while", TokenType::WHILE},
	{"false", TokenType::FALSE},
	{"for", TokenType::FOR},
	{"func", TokenType::FUNC},
	{"this", TokenType::THIS},
	{"true", TokenType::TRUE},
	{"as", TokenType::AS},
	{"await", TokenType::AWAIT},
	{"async", TokenType::ASYNC},
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

vector<Token> Scanner::tokenizeSource(string source, string sourceName) {
	// Setup
	curFile = new File(source, sourceName);
	line = 0;
	start = 0;
	current = start;
	curFile->lines.push_back(0);
	hadError = false;
	tokens.clear();

	// Tokenization
	Token token;
	do {
		token = scanToken();
		tokens.push_back(token);
	} while (token.type != TokenType::TOKEN_EOF);

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
bool Scanner::isIndexInFile(int index) {
	return 0 <= index && index < curFile->sourceFile.size();
}

//if matched we consume the character
bool Scanner::match(char expected) {
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
	if (isAtEnd()) return makeToken(TokenType::TOKEN_EOF);
	
	consumeWhitespace();
	start = current;

	//a comment could go to the end of the file
	if (isAtEnd()) return makeToken(TokenType::TOKEN_EOF);

	char c = advance();
	//identifiers start with _ or [a-z][A-Z]
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
	case '.': return makeToken(TokenType::DOT);
	case '-': return makeToken(match('=') ? TokenType::MINUS_EQUAL : match('-') ? TokenType::DECREMENT : TokenType::MINUS);
	case '+': return makeToken(match('=') ? TokenType::PLUS_EQUAL : match('+') ? TokenType::INCREMENT : TokenType::PLUS);
	case '/': return makeToken(match('=') ? TokenType::SLASH_EQUAL : TokenType::SLASH);
	case '*': return makeToken(match('=') ? TokenType::STAR_EQUAL : TokenType::STAR);
	case '&': return makeToken(match('=') ? TokenType::BITWISE_AND_EQUAL : match('&') ? TokenType::AND : TokenType::BITWISE_AND);
	case '|': return makeToken(match('=') ? TokenType::BITWISE_OR_EQUAL : match('|') ? TokenType::OR : TokenType::BITWISE_OR);
	case '^': return makeToken(match('=') ? TokenType::BITWISE_XOR_EQUAL : TokenType::BITWISE_XOR);
	case '%': return makeToken(match('=') ? TokenType::PERCENTAGE_EQUAL : TokenType::PERCENTAGE);
	case '~': return makeToken(TokenType::TILDA);
	case '!': return makeToken(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
	case '=': return makeToken(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
	case '<': return makeToken(match('=') ? TokenType::LESS_EQUAL : match('<') ? TokenType::BITSHIFT_LEFT : TokenType::LESS);
	case '>': return makeToken(match('=') ? TokenType::GREATER_EQUAL : match('>') ? TokenType::BITSHIFT_RIGHT : TokenType::GREATER);
	case '"': return string_();
	case ':': return makeToken(match(':') ? TokenType::DOUBLE_COLON : TokenType::COLON);
	case '?': return makeToken(TokenType::QUESTIONMARK);
	case '\n':
		Token newLineToken = makeToken(TokenType::NEWLINE);
		curFile->lines.push_back(current);
		line++;
		return newLineToken;
	}

	return makeToken(TokenType::ERROR);
}

Token Scanner::makeToken(TokenType type) {
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
	//first character of the identifier has to be alphabetical, rest can be alphanumerical and '_'
	while (isalnum(peek()) || peek() == '_') advance();
	return makeToken(identifierType());
}

TokenType Scanner::identifierType() {
	string tokenString = curFile->sourceFile.substr(start, current - start);
	
	// language keyword
	if (keywordToTokenType.contains(tokenString)) return keywordToTokenType[tokenString];

	// variable name
	return TokenType::IDENTIFIER;
}

bool Scanner::checkKeyword(int keywordOffset, string keyword) {
	if (!isIndexInFile(start + keywordOffset + keyword.length())) return false;
	if (curFile->sourceFile.substr(start + keywordOffset, keyword.length()) == keyword) {
		return true;
	}
	return false;
}
#pragma endregion
