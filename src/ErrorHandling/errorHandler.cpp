#include "errorHandler.h"
#include "../Preprocessing/scanner.h"
#include <iostream>

//name:line:column: error: msg
//line
//which part
//symbol: token.getLexeme()

const string cyan = "\u001b[38;5;117m";
const string black = "\u001b[0m";
const string red = "\u001b[38;5;196m";
const string yellow = "\u001b[38;5;220m";

// Highlights a token
void highlightToken(Token token) {
	File* src = token.str.sourceFile;

	string lineNumber = std::to_string(token.str.line + 1);
	std::cout << yellow << src->name << black << ":" << cyan << lineNumber << " | " << black;
	std::cout << token.str.getLine() << std::endl;

	string highlight;
	highlight.insert(highlight.end(), src->name.length() + lineNumber.length() + 4 + token.str.column, ' ');
	highlight.insert(highlight.end(), token.str.length, '^');

	std::cout << red << highlight << black << "\n";
}
/*
// Reports the origin of a token (climbs the expansion chain the given token took during preprocessing)
void logExpansionPath(Token token) {
	if (!token.macroPtr) return;

	Token macroToken = *token.macroPtr;
	std::cout << red << "Note: " << black << "in expansion of macro '" << yellow << macroToken.getLexeme() << black << "'" << std::endl;
	highlightToken(macroToken);
	logExpansionPath(macroToken);
}

// Used for tokens which appear in macro arguments. Reports the argument substitution path used during preprocessing.
void logDefinitionPath(Token token) {
	if (!token.parentPtr || token.parentPtr == token.macroPtr) return;

	Token parentToken = *token.parentPtr;
	Token macroToken = *token.macroPtr;
	std::cout << red << "Note: " << black << "in definition of macro '" << yellow << macroToken.getLexeme() << black << "'" << std::endl;
	highlightToken(parentToken);
	logDefinitionPath(macroToken);
}
*/
void report(File* src, Token& token, string msg) {
	if (token.type == TokenType::TOKEN_EOF) {
		std::cout << "End of file. \n" << msg;
		return;
	}
	string name = "\u001b[38;5;220m" + src->name + black;
	std::cout << red + "error: " + black + msg + "\n";

	highlightToken(token);
	//logDefinitionPath(token);
	//logExpansionPath(token);
	std::cout << "\n";
}

namespace errorHandler {
	namespace {
		struct SystemError {
			string errorText;

			SystemError(string _errorText) {
				errorText = _errorText;
			}
		};
		struct CompileTimeError {
			string errorText;
			File* origin;
			Token token;

			CompileTimeError(string _errorText, File* _origin, Token _token) {
				errorText = _errorText;
				origin = _origin;
				token = _token;
			}
		};
		struct RuntimeError {
			string errorText;
			string funcName;
			CSLModule* origin;

			RuntimeError(string _errorText, CSLModule* _origin, string _funcName) {
				errorText = _errorText;
				origin = _origin;
				funcName = _funcName;
			}
		};

		//errors during preprocessing, building of the AST tree and compiling
		vector<CompileTimeError> compileErrors;
		//stack trace when a runtime error occurs
		vector<RuntimeError> runtimeErrors;
		//system level errors(eg. not being able to access a file)
		vector<SystemError> systemErrors;
	}

	void showCompileErrors() {
		for (CompileTimeError error : compileErrors) {
			report(error.origin, error.token, error.errorText);
		}
	}

	void showRuntimeErrors() {
		//TODO: implement this when you get to stack tracing
	}
	void showSystemErrors() {
		for (SystemError error : systemErrors) {
			std::cout << "System error: " << error.errorText << "\n";
		}
	}

	void addCompileError(string msg, Token token) {
		compileErrors.push_back(CompileTimeError(msg, token.str.sourceFile, token));
	}
	void addRuntimeError(string msg, string funcName, CSLModule* origin) {
		runtimeErrors.push_back(RuntimeError(msg, origin, funcName));
	}
	void addSystemError(string msg) {
		systemErrors.push_back(SystemError(msg));
	}

	bool hasErrors() {
		return !compileErrors.empty() || !runtimeErrors.empty() || !systemErrors.empty();
	}
}