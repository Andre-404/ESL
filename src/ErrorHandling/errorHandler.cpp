#include "errorHandler.h"
#include "../Preprocessing/scanner.h"
#include "../Includes/fmt/format.h"
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
	string line = token.str.getLine();
    std::cout << yellow << src->name << black << ":" << cyan << lineNumber << " | " << black;
	std::cout << line << std::endl;

	string highlight = "";
    highlight.insert(highlight.end(), src->name.length() + lineNumber.length() + 4, ' ');
    for (int i = 0; i < token.str.column; i++){
        if (line[i] == '\t') highlight += '\t';
        else highlight += ' ';
    }
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
	string name = "\u001b[38;5;220m" + src->name + black;
	std::cout << red + "error: " + black + msg + "\n";

	highlightToken(token);
	//logDefinitionPath(token);
	//logExpansionPath(token);
	std::cout << "\n";
}

namespace errorHandler {
	namespace {
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

		//errors during preprocessing, building of the AST tree and compiling
		vector<CompileTimeError> compileErrors;
	}

	void showCompileErrors() {
		for (CompileTimeError error : compileErrors) {
			report(error.origin, error.token, error.errorText);
		}
	}

	void addCompileError(string msg, Token token) {
		compileErrors.emplace_back(msg, token.str.sourceFile, token);
	}
	void addSystemError(string msg) {
        std::cout<<msg;
        exit(1);
	}

	bool hasErrors() {
		return !(compileErrors.size() == 0);
	}

    vector<string> convertCompilerErrorsToJson(){
        vector<string> errors;
        for (CompileTimeError error : compileErrors) {
            string final = "{";
            final += fmt::format("\"path\": \"{}\", \"code\": {}, \"message\": \"{}\", \"line\": {}, \"start\": {}, \"end\": {}, \"severity\": \"{}\", \"relatedInformation\" :",
                                 error.origin->path, 0, error.errorText, error.token.str.line, error.token.str.column, error.token.str.column + error.token.str.length,
                                 "error");
            final += "[] }";
            errors.push_back(final);
        }
        return errors;
    }
}