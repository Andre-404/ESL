#include "preprocessor.h"
#include <filesystem>
#include "../files.h"
#include <iostream>
#include "../ErrorHandling/errorHandler.h"


using std::unordered_set;
using std::unordered_map;
using std::pair;
using namespace preprocessing;
using namespace errorHandler;
using namespace std::filesystem;

bool isAtEnd(const vector<Token>& tokens, const int pos) {
    return pos >= tokens.size();
}

bool match(TokenType type, const vector<Token>& tokens, const int pos) {
    if (pos < 0 || isAtEnd(tokens, pos)) return false;
    return true;
}

Preprocessor::Preprocessor(){
    projectRootPath = "";
    curUnit = nullptr;
}

Preprocessor::~Preprocessor() {
}

void Preprocessor::preprocessProject(string mainFilePath) {
    path p(mainFilePath);

    // Check file validity
    if (p.extension().string() != ".csl" || p.stem().string() != "main" || !exists(p)) {
        errorHandler::addSystemError("Couldn't find main.csl");
        return;
    }

    projectRootPath = p.parent_path().string() + "/";
    CSLModule* mainModule = scanFile("main.csl");
    toposort(mainModule);
}

CSLModule* Preprocessor::scanFile(string moduleName) {
    string fullPath = projectRootPath + moduleName;
    vector<Token> tokens = scanner.tokenizeSource(readFile(fullPath), moduleName);
    CSLModule* unit = new CSLModule(tokens, scanner.getFile());
    allUnits[moduleName] = unit;
    curUnit = unit;

    // Macros


    // Dependencies
    vector<pair<Token, Token>> depsToParse = retrieveDirectives(unit);

    processDirectives(unit, depsToParse);

    return unit;
}

void Preprocessor::toposort(CSLModule* unit) {
    //TODO: we can get a stack overflow if this goes into deep recursion, try making a iterative stack based DFS implementation
    //TODO: basically implement Kahn's algorithm...
    unit->traversed = true;
    for (Dependency& dep : unit->deps) {
        if (!dep.module->traversed) toposort(dep.module);
    }
    sortedUnits.push_back(unit);
}

// Gets directives to import into the parserCurrent file
vector<pair<Token, Token>> Preprocessor::retrieveDirectives(CSLModule* unit) {
    vector<Token>& tokens = unit->tokens;
    vector<Token> resultTokens; // Tokenization after processing imports and macros
    vector<pair<Token, Token>> importTokens;

    auto getErrorToken = [&](int i) {
        if (i < 0 || tokens.size() <= i) return tokens[tokens.size() - 1];
        return tokens[i];
    };

    for (int i = 0; i < tokens.size(); i++) {
        Token& token = tokens[i];
        // New line tokens from final tokenization (they are only used for macro processing)
        if (token.type == TokenType::NEWLINE) { continue; }
        // Add a dependency
        if (token.type == TokenType::IMPORT) {
            // Move to dependency name
            if (match(TokenType::STRING, tokens, ++i)) {
                addCompileError("Expected a module name.", getErrorToken(i));
                continue;
            }

            // Add dependency to list of dependencies
            Token dependencyName = tokens[i];
            Token alias;
            if (match(TokenType::AS, tokens, i + 1)) {
                if (match(TokenType::IDENTIFIER, tokens, i + 2)) alias = tokens[i + 2];
                else {
                    addCompileError("Expected alias for module. ", getErrorToken(i + 2));
                    continue;
                }
                i += 2;
            }

            importTokens.emplace_back(dependencyName, alias);
        }
        else resultTokens.push_back(token);
    }

    unit->tokens = resultTokens;

    return importTokens;
}

// Processes directives to import into the parserCurrent file
void Preprocessor::processDirectives(CSLModule* unit, vector<pair<Token, Token>>& depsToParse) {
    for (auto& [path, alias] : depsToParse) {
        string depName = path.getLexeme();
        std::cout << depName << '\n';
        depName = depName.substr(1, depName.size() - 2); // Extract dependency name from "" (it's a string)

        // If we have already scanned module with name 'dep' we add it to the deps list of this module to topsort it later
        if (allUnits.count(depName)) {
            // If we detect a cyclical import we still continue parsing other files to detect as many errors as possible
            if (!allUnits[depName]->resolvedDeps) {
                addCompileError("Cyclical importing detected.", path);
                continue;
            }
            unit->deps.push_back(Dependency(alias, path, allUnits[depName]));

            continue;
        }

        std::filesystem::path p(projectRootPath + depName);
        if (std::filesystem::exists(p)) {
            Dependency dep = Dependency(alias, path, scanFile(depName));
            unit->deps.push_back(dep);
        }
        else {
            addCompileError("File " + depName + " doesn't exist.", path);
        }
    }

    unit->resolvedDeps = true;
}
