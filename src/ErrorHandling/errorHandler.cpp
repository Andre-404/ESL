#include "errorHandler.h"
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
void highlightToken(Span span) {
	File* src = span.sourceFile;

	string lineNumber = std::to_string(span.computeLine());
	std::string_view line = span.getLine();
    std::cout << yellow << src->name << black << ":" << cyan << lineNumber << " | " << black;
	std::cout << line << std::endl;

	string highlight = "";
    highlight.insert(highlight.end(), src->name.length() + lineNumber.length() + 4, ' ');
    for (int i = 0; i < span.computeColumn(); i++){
        if (line[i] == '\t') highlight += '\t';
        else highlight += ' ';
    }
	highlight.insert(highlight.end(), span.end - span.start, '^');

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
void display(File* src, Span& token, string msg, bool isWarning) {
	string name = yellow + src->name + black;
	std::cout << (isWarning ? yellow : red) + "error: " + black + msg + "\n";

	highlightToken(token);
	std::cout << "\n";
}

namespace errorHandler {
    void ErrorHandler::reportWarning(std::string msg, const std::initializer_list<Token> &tokens) {
        Span hightlight;
        for(const Token& token : tokens){
            hightlight.sourceFile = token.str.sourceFile ? token.str.sourceFile : hightlight.sourceFile;
            hightlight.start = token.str.start < hightlight.start ? token.str.start : hightlight.start;
            hightlight.end = token.str.end < hightlight.end ? token.str.end : hightlight.end;
        }
        warnings.emplace_back(msg, hightlight);
    }
    void ErrorHandler::reportError(std::string msg, const std::initializer_list<Token> &tokens) {
        Span highlight;
        highlight.start = -1;
        for(const Token& token : tokens){
            highlight.sourceFile = token.str.sourceFile ? token.str.sourceFile : highlight.sourceFile;
            highlight.start = token.str.start < highlight.start || highlight.start == -1 ? token.str.start : highlight.start;
            highlight.end = token.str.end > highlight.end ? token.str.end : highlight.end;
        }
        errors.emplace_back(msg, highlight);
    }
    void ErrorHandler::reportUnrecoverableError(std::string msg) {
        std::cout<<msg;
        exit(64);
    }
    vector<string> ErrorHandler::convertToJSON(){
        vector<string> convertedErrors;
        for (Error& error : errors) {
            string final = "{";
            Span span = error.highlightArea;
            final += fmt::format("\"path\": \"{}\", \"code\": {}, \"message\": \"{}\", \"line\": {}, \"start\": {}, \"end\": {}, \"severity\": \"{}\", \"relatedInformation\" :",
                                 span.sourceFile->path, 0, error.msg, span.computeLine(), span.computeColumn(),
                                 span.computeColumn() + span.end - span.start, "error");
            final += "[] }";
            convertedErrors.push_back(final);
        }
        return convertedErrors;
    }
    void ErrorHandler::displayErrors(){
        std::sort(errors.begin(), errors.end(), [](Error& err1, Error& err2){
            return err1.highlightArea.start < err2.highlightArea.start;
        });
        for(Error& error : errors){
            display(error.highlightArea.sourceFile, error.highlightArea, error.msg, false);
        }
    }
    void ErrorHandler::displayWarnings() {
        std::sort(warnings.begin(), warnings.end(), [](Error& warn1, Error& warn2){
            return warn1.highlightArea.start < warn2.highlightArea.start;
        });
        for(Error& warning : warnings){
            display(warning.highlightArea.sourceFile, warning.highlightArea, warning.msg, true);
        }
    }
}
