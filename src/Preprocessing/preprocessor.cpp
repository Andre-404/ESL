#include "preprocessor.h"
#include "../files.h"
#include "../Includes/fmt/format.h"
#include <filesystem>
#include "../ErrorHandling/errorHandler.h"

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
    return tokens[pos].type == type;
}
// Converts relative path that appears after 'import' to an absolute path to be used by preprocessor
string parsePath(path& absP, string relativeP){
    path p = absP;
    while(relativeP.substr(0, 3) == "../"){
        p = p.parent_path();
        relativeP.erase(0, 3);
    }
    return p.string() + "/" + relativeP;
}

Preprocessor::Preprocessor(errorHandler::ErrorHandler& errorH) : errorH(errorH){
    projectRootPath = "";
}

void Preprocessor::preprocessProject(const string mainFilePath) {
    path p(mainFilePath);

    // Check file validity
    if (p.extension().string() != ".esl" || !exists(p)) {
        errorH.reportUnrecoverableError(fmt::format("Couldn't find {}.esl", p.stem().string()));
        return;
    }

    projectRootPath = p.parent_path().string() + "/";
    ESLModule* mainModule = scanFile(p.string());
    toposort(mainModule);
    // Used later to get dependencies from array
    for(int i = 0; i < sortedUnits.size(); i++) sortedUnits[i]->id = i;
}

ESLModule* Preprocessor::scanFile(const string filePath) {
    path p(filePath);
    // Scan the file and cache it in allUnits
    vector<Token> tokens = scanner.tokenizeSource(filePath, p.stem().string());
    ESLModule* unit = new ESLModule(tokens, scanner.getFile());
    allUnits[filePath] = unit;

    // Dependencies
    vector<pair<Token, Token>> depsToParse = parseImports(unit);

    processImports(unit, depsToParse, p.string());

    return unit;
}

void Preprocessor::toposort(ESLModule* unit) {
    //TODO: basically implement Kahn's algorithm...
    unit->traversed = true;
    for (Dependency& dep : unit->deps) {
        if (!dep.module->traversed) toposort(dep.module);
    }
    sortedUnits.push_back(unit);
}

// Gets directives to import into the parserCurrent file
vector<pair<Token, Token>> Preprocessor::parseImports(ESLModule* unit) {
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
            if (!match(TokenType::STRING, tokens, ++i)) {
                errorH.reportError("Expected a module path.", getErrorToken(i));
                continue;
            }

            // Add dependency to list of dependencies
            Token dependencyPath = tokens[i];
            Token alias;
            if (match(TokenType::AS, tokens, i + 1)) {
                if (match(TokenType::IDENTIFIER, tokens, i + 2)) alias = tokens[i + 2];
                else {
                    errorH.reportError("Expected alias for module. ", getErrorToken(i + 2));
                    continue;
                }
                i += 2;
            }

            importTokens.emplace_back(dependencyPath, alias);
        }
        else resultTokens.push_back(token);
    }

    unit->tokens = resultTokens;

    return importTokens;
}

// Processes directives to import into the parserCurrent file
void Preprocessor::processImports(ESLModule* unit, vector<pair<Token, Token>>& depsToParse, const string absolutePath) {
    // Absolute path to the same directory as the unit that these directives belong to
    path absP = path(absolutePath).parent_path();
    for (auto& [pathToken, alias] : depsToParse) {
        string path = pathToken.getLexeme();
        // Calculates absolute path to the file with the relative path given by pathToken
        path = parsePath(absP, path.substr(1, path.size() - 2)); // Extract dependency path from "" (string token)

        // If we have already scanned the module pointed to by pathToken then just add it
        if (allUnits.count(path)) {
            // If we detect a cyclical import we still continue parsing other files to detect as many errors as possible
            if (!allUnits[path]->resolvedDeps) {
                errorH.reportError("Cyclical importing detected.", pathToken);
                continue;
            }
            unit->deps.push_back(Dependency(alias, pathToken, allUnits[path]));
            continue;
        }

        std::filesystem::path p(path);
        if (std::filesystem::exists(p)) {
            Dependency dep = Dependency(alias, pathToken, scanFile(path));
            unit->deps.push_back(dep);
        }
        else {
            errorH.reportError("File " + path + " doesn't exist.", pathToken);
        }
    }

    unit->resolvedDeps = true;
}
