#pragma once
#include "scanner.h"
#include <unordered_map>
#include <memory>
#include <tuple>

namespace preprocessing {
    using std::unordered_map;
    using std::unique_ptr;
    using std::pair;

    class Preprocessor {
    public:
        Preprocessor();
        ~Preprocessor();
        void preprocessProject(const string mainFilePath);

        vector<ESLModule*> getSortedUnits() { return sortedUnits; }
    private:
        string projectRootPath;
        Scanner scanner;

        unordered_map<string, ESLModule*> allUnits;
        vector<ESLModule*> sortedUnits;

        vector<pair<Token, Token>> retrieveDirectives(ESLModule* unit);

        void processDirectives(ESLModule* unit, vector<pair<Token, Token>>& depsToParse, const string absolutePath);

        ESLModule* scanFile(const string unitName);
        void toposort(ESLModule* unit);
    };

}
