#pragma once
#include "scanner.h"
#include <unordered_map>


namespace errorHandler{
    class ErrorHandler;
}

namespace preprocessing {
    using std::unordered_map;
    using std::unique_ptr;
    using std::pair;

    class Preprocessor {
    public:
        Preprocessor(errorHandler::ErrorHandler& errorH);
        void preprocessProject(const string mainFilePath);

        vector<ESLModule*> getSortedUnits() { return sortedUnits; }
    private:
        string projectRootPath;
        Scanner scanner;
        errorHandler::ErrorHandler& errorH;

        unordered_map<string, ESLModule*> allUnits;
        vector<ESLModule*> sortedUnits;

        vector<pair<Token, Token>> parseImports(ESLModule* unit);

        void processImports(ESLModule* unit, vector<pair<Token, Token>>& depsToParse, const string absolutePath);

        ESLModule* scanFile(const string unitName);
        void toposort(ESLModule* unit);
    };

}
