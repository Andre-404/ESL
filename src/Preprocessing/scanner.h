#pragma once
#include "../moduleDefs.h"

namespace preprocessing {

class Scanner {
	public:
		vector<Token> tokenizeSource(const string source, const string sourcename);
		File* getFile() { return curFile; }
		Scanner();
	private:
		File* curFile;
		int line;
		int start;
		int current;
		bool hadError;
		vector<Token> tokens;

		Token scanToken();
		Token makeToken(const TokenType type);

		bool isAtEnd();
		bool isIndexInFile(const int index);
		bool match(const char expected);
		bool checkKeyword(const int keywordOffset, const string keyword);
		char advance();
		char peek();
		char peekNext();
		void consumeWhitespace();

		Token string_();

		Token number();

		Token identifier();
		TokenType identifierType();

		void reset();
	};
}
