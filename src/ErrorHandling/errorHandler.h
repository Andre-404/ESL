#pragma once
#include "../moduleDefs.h"

namespace errorHandler {
    class Error{
    public:
        string msg;
        Span highlightArea;
    };
    class ErrorHandler{
    public:
        ErrorHandler(){

        }

        void reportError(string msg, const std::initializer_list<Token>& tokens);
        void reportWarning(string msg, const std::initializer_list<Token>& tokens);
        void reportError(string msg, Token token){
            reportError(msg, {token});
        }
        void reportWarning(string msg, Token token){
            reportWarning(msg, {token});
        }
        void reportUnrecoverableError(string msg);
        bool hasErrors(){
            return !errors.empty();
        }

        vector<string> convertToJSON();
        void displayErrors();
        void displayWarnings();
    private:
        vector<Error> errors;
        vector<Error> warnings;
    };
}