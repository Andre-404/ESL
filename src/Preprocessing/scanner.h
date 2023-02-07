#pragma once
#include "../moduleDefs.h"

namespace preprocessing {

class Scanner {
	public:
		vector<Token> tokenizeSource(string source, string sourcename);
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
		Token makeToken(TokenType type);

		bool isAtEnd();
		bool isIndexInFile(int index);
		bool match(char expected);
		bool checkKeyword(int keywordOffset, string keyword);
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
