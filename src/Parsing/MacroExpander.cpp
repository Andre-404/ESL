#include "MacroExpander.h"

AST::MacroExpander::MacroExpander(Parser* _parser) {
    parser = _parser;
}

void AST::MacroExpander::expand(ASTNodePtr& node){
    if (!node) return;
    node->accept(this);
    if (expansion != nullptr) {
        node = expansion;
    }
    expansion = nullptr;
}

void AST::MacroExpander::visitAssignmentExpr(AssignmentExpr* expr) {
    expand(expr->value);
}
void AST::MacroExpander::visitSetExpr(SetExpr* expr) { expand(expr->value); }
void AST::MacroExpander::visitConditionalExpr(ConditionalExpr* expr) {
    expand(expr->condition);
    expand(expr->thenBranch);
    expand(expr->elseBranch);
}
void AST::MacroExpander::visitBinaryExpr(BinaryExpr* expr) {
    expand(expr->left);
    expand(expr->right);
}
void AST::MacroExpander::visitUnaryExpr(UnaryExpr* expr) {
    expand(expr->right);
}
void AST::MacroExpander::visitCallExpr(CallExpr* expr) {
    for (auto& arg : expr->args) {
        expand(arg);
    }
}
void AST::MacroExpander::visitFieldAccessExpr(FieldAccessExpr* expr) {}
void AST::MacroExpander::visitAsyncExpr(AsyncExpr* expr) {
    for (auto& arg : expr->args) {
        expand(arg);
    }
}
void AST::MacroExpander::visitAwaitExpr(AwaitExpr* expr) { expand(expr->expr); }
void AST::MacroExpander::visitArrayLiteralExpr(ArrayLiteralExpr* expr) {
    for (auto& member : expr->members) {
        expand(member);
    }
}
void AST::MacroExpander::visitStructLiteralExpr(StructLiteral* expr) {
    for (StructEntry& entry : expr->fields) {
        expand(entry.expr);
    }
}
void AST::MacroExpander::visitLiteralExpr(LiteralExpr* expr) {}
void AST::MacroExpander::visitFuncLiteral(FuncLiteral* expr) {
    expand(expr->body);
}
void AST::MacroExpander::visitSuperExpr(SuperExpr* expr) {}
void AST::MacroExpander::visitModuleAccessExpr(ModuleAccessExpr* expr) {}

void AST::MacroExpander::visitMacroExpr(MacroExpr* expr) {
    recursionDepth++;
    if (recursionDepth >= MACRO_RECURSION_DEPTH) {
        ParserException e = parser->error(expr->macroName, fmt::format("Macro recursion limit({}) reached.", MACRO_RECURSION_DEPTH));
        e.macroRecursionLimitExceeded = true;
        throw e;
    }

    ASTNodePtr curExpansion = nullptr;
    try {
        curExpansion = parser->macros[expr->macroName.getLexeme()]->expand(expr->args, expr->macroName);
        expand(curExpansion);
    }
    catch (ParserException& e) {
        if (!e.macroRecursionLimitExceeded) parser->error(expr->macroName, "Note: in expansion of macro");
        throw e;
    }

    expansion = curExpansion;

    recursionDepth--;
}

void AST::MacroExpander::visitVarDecl(VarDecl* decl) {
    expand(decl->value);
}

void AST::MacroExpander::visitFuncDecl(FuncDecl* decl) { expand(decl->body); }
void AST::MacroExpander::visitClassDecl(ClassDecl* decl) {
    for (auto& method : decl->methods) {
        expand(method);
    }
}

void AST::MacroExpander::visitExprStmt(ExprStmt* stmt) { expand(stmt->expr); }
void AST::MacroExpander::visitBlockStmt(BlockStmt* stmt) {
    for (auto& line : stmt->statements) {
        expand(line);
    }
}
void AST::MacroExpander::visitIfStmt(IfStmt* stmt) {
    expand(stmt->condition);
    expand(stmt->thenBranch);
    expand(stmt->elseBranch);
}
void AST::MacroExpander::visitWhileStmt(WhileStmt* stmt) {
    expand(stmt->condition);
    expand(stmt->body);
}
void AST::MacroExpander::visitForStmt(ForStmt* stmt) {
    expand(stmt->init);
    expand(stmt->condition);
    expand(stmt->increment);
    expand(stmt->body);
}
void AST::MacroExpander::visitBreakStmt(BreakStmt* stmt) {}
void AST::MacroExpander::visitContinueStmt(ContinueStmt* stmt) {}
void AST::MacroExpander::visitSwitchStmt(SwitchStmt* stmt) {
    expand(stmt->expr);
    for (auto& _case : stmt->cases) {
        for (auto& stmt : _case->stmts){
            expand(stmt);
        }
    }
}
void AST::MacroExpander::visitCaseStmt(CaseStmt* stmt) {
    for (auto& statement : stmt->stmts) {
        expand(statement);
    }
}
void AST::MacroExpander::visitAdvanceStmt(AdvanceStmt* stmt) {}
void AST::MacroExpander::visitReturnStmt(ReturnStmt* stmt) { expand(stmt->expr); }

AST::Macro::Macro() {
    parser = nullptr;
}

AST::Macro::Macro(Token _name, Parser *_parser) {
    name = _name;
    parser = _parser;
}

// TODO: remove later?
void debugTokens(vector<Token> tokens){
    for (const auto& token : tokens){
        std::cout << token.getLexeme() << " ";
    }
    std::cout << "\n";
}

AST::ASTNodePtr AST::Macro::expand(vector<Token> &args, const Token &callerToken) {
    // Attempt to match every macro matcher to arguments ...
    for (int i = 0; i < matchers.size(); i++){
        parser->exprMetaVars.clear();
        parser->ttMetaVars.clear();

        // Try to interpret arguments into meta variables
        if (!matchers[i].interpret(args)) { continue; }

        // Expand all loops and substitute all token tree meta variables
        vector<Token> expansion = expandLoops(transcribers[i], 0, transcribers[i].size() - 1, {});

        bool isStmt = false;
        for (Token token : expansion){
            if (token.type == TokenType::SEMICOLON) isStmt = true;
        }

        // Convert tokens to AST and substitute all expr meta variables
        parser->parseMode = ParseMode::Macro;
        parser->currentContainer = &expansion;
        parser->currentPtr = 0;
        vector<ASTNodePtr> stmts;
        // TODO: Fix here and in other places: too hacky
        if (!isStmt){
            expansion.push_back(Token());
            expansion.back().type = TokenType::SEMICOLON;
            stmts.push_back(parser->expression());
        } else {
            while (!parser->isAtEnd()) {
                stmts.push_back(parser->localDeclaration());
            }
        }

        parser->parseMode = ParseMode::Standard;

        // Return AST
        if (!isStmt){
            return stmts[0];
        } else {
            return make_shared<AST::BlockStmt>(stmts);
        }
    }

    // ... we found no appropriate matcher
    throw parser->error(callerToken, "Couldn't find an appropriate matcher for the given macro arguments.");
    return nullptr;
}


vector<Token> AST::Macro::expandLoops(vector<Token>& readFrom, int begin, int end, vector<int> loopIndices) {
    vector<Token> expansion;
    auto check = [&](int i, TokenType type) {
        if (i < begin || i > end) return false;
        return readFrom[i].type == type;
    };

    int badVariables = 0;
    int variables = 0;
    int loops = 0;

    int loopDepth = loopIndices.size();

    // TODO: This can be optimized (first check if all variables are good, then expand them and the loops)
    for (int i = begin; i <= end; i++){
        if (check(i, TokenType::DOLLAR)){
            // Meta variables
            if (check(i + 1, TokenType::IDENTIFIER)){
                string varName = readFrom[i + 1].getLexeme();
                if (!parser->ttMetaVars.contains(varName) && !parser->exprMetaVars.contains(varName)) {
                    throw parser->error(readFrom[i + 1],
                                        fmt::format("Meta variable {} doesn't appear in matcher pattern.", varName));
                }
                // Token tree meta variable
                if (parser->ttMetaVars.contains(varName)){
                    std::unique_ptr<TTMetaVar>& var = parser->ttMetaVars[varName];
                    if (var->loopDepth > loopDepth){
                        throw parser->error(readFrom[i + 1], "Macro variable is at incorrect loop depth.");
                    }
                    if (var->loopDepth == loopDepth) variables++;
                    vector<int> curIndices = loopIndices;
                    while (curIndices.size() > var->loopDepth) curIndices.pop_back();
                    if (!var->values.contains(curIndices)) {
                        badVariables++;
                        break;
                    }
                    vector<Token> tokenTree = var->values[loopIndices];
                    expansion.insert(expansion.end(), tokenTree.begin(), tokenTree.end());
                }
                else if (parser->exprMetaVars.contains(varName)) {
                    std::unique_ptr<ExprMetaVar>& var = parser->exprMetaVars[varName];
                    if (var->loopDepth > loopDepth){
                        throw parser->error(readFrom[i + 1], "Macro variables is at incorrect loop depth.");
                    }
                    if (var->loopDepth == loopDepth) variables++;
                    vector<int> curIndices = loopIndices;
                    while (curIndices.size() > var->loopDepth) curIndices.pop_back();
                    if (!var->values.contains(curIndices)){
                        badVariables++;
                        break;
                    }
                    expansion.push_back(readFrom[i]);
                    expansion.push_back(readFrom[i+1]);
                    var->valueList.push_back(var->values[curIndices]);
                }
                i++;
            }
            // Loops
            else if (check(i + 1, TokenType::LEFT_PAREN)){
                loops++;
                int loopStart = i + 2;
                parser->currentContainer = &readFrom;
                parser->currentPtr = i + 1;
                parser->readTokenTree();
                i = parser->currentPtr;

                bool hasDelim = false;

                if (check(i, TokenType::COMMA) || check(i, TokenType::SEMICOLON)) { i++; hasDelim = true; }

                if (!check(i, TokenType::STAR)){ throw parser->error(readFrom[i], "Expected '*' terminating macro loop."); }

                vector<Token> expandedLoop;
                loopIndices.push_back(0);
                do {
                    expandedLoop = expandLoops(readFrom, loopStart, i - 2 - hasDelim, loopIndices);
                    if (hasDelim && loopIndices.back() > 0 && !expandedLoop.empty()) { expansion.push_back(readFrom[i-1]); }
                    expansion.insert(expansion.end(), expandedLoop.begin(), expandedLoop.end());
                    loopIndices.back()++;
                } while (!expandedLoop.empty());
                loopIndices.pop_back();
            } else {
                throw parser->error(readFrom[i], "Expected '(' or identifier after '$' in macro transcriber");
            }
        } else {
            expansion.push_back(readFrom[i]);
        }
    }
    if (badVariables > 0 && badVariables != variables) {
        // TODO: Improve error (highlight entire loop)
        throw parser->error(readFrom[begin], "Macro loop should only contain meta variables which loop the same amount of times.");
    }
    if (variables == 0 && loops == 0 && !loopIndices.empty()){
        // TODO: Improve error (highlight entire loop)
        throw parser->error(readFrom[begin], "Macro loop must have meta variables.");
    }
    if (badVariables) return {};
    return expansion;
}


bool AST::MatchPattern::interpret(vector<Token> &args) const {
    parser->currentContainer = &args;
    parser->parseMode = ParseMode::Standard;

    auto isAtEnd = [&](int i) { return i >= args.size(); };

    // Indicates possible transitions which consume a valid expression
    vector<vector<int>> exprConsumeTransitions(args.size(), vector<int>(args.size() + 1));
    parser->parseMode = ParseMode::Matcher;
    for (int i = 0; i < args.size(); i++){
        vector<Token> exprContainer;
        parser->currentContainer = &exprContainer;
        for (int len = 1; i + len <= args.size(); len++){
            parser->currentPtr = 0;
            exprContainer.push_back(args[i + len - 1]);
            // Append a semicolon to terminate the expression
            exprContainer.push_back(Token());
            exprContainer.back().type = TokenType::SEMICOLON;
            try {
                parser->expression();
                // We can fully read the expression
                if (parser->currentPtr == len){
                    exprConsumeTransitions[i].push_back(i + len);
                }
            }
            catch (ParserException& e) {}
            // Remove the appended semicolon
            exprContainer.pop_back();
        }
    }
    parser->parseMode = ParseMode::Standard;

    vector<vector<int>> dp(args.size() + 1, vector<int>(pattern.size() + 1));
    vector<vector<Transition>> backTransitions(args.size() + 1, vector<Transition>(pattern.size() + 1));
    dp[0][0] = 1;

    for (int i = 0; i < args.size(); i++){
        for (int j : topoSort) {
            // This state is unreachable
            if (dp[i][j] == 0) continue;

            auto transition = [&](int n_i, int n_j, TransitionType type){
                dp[n_i][n_j] += dp[i][j];
                backTransitions[n_i][n_j] = Transition(i, j, type);
            };

            // Tokens for matcher loops specific tokens
            if (tokenTypes[j] != MatcherTokenType::Neutral){
                if (tokenTypes[j] == MatcherTokenType::Ignore){
                    transition(i, j + 1, TransitionType::None);
                }
                else if (tokenTypes[j] == MatcherTokenType::Skippable){
                    transition(i, loopJumps[j], TransitionType::None);
                    transition(i, j + 1, TransitionType::None);
                }
                else if (tokenTypes[j] == MatcherTokenType::Iterate){
                    transition(i, loopJumps[j], TransitionType::LoopIterate);
                }
                else if (tokenTypes[j] == MatcherTokenType::LoopEnd){
                    transition(i, loopJumps[j], TransitionType::LoopEnd);
                    transition(i, j + 1, TransitionType::None);
                }
                else if (tokenTypes[j] == MatcherTokenType::LoopBegin){
                    transition(i, j + 1, TransitionType::LoopBegin);
                }
                continue;
            }

            // Regular token
            if (pattern[j].type != TokenType::DOLLAR){
                if (pattern[j].equals(args[i])) { transition(i + 1, j + 1, TransitionType::None); }
                continue;
            }
            // Meta variables
            // Token tree
            if (pattern[j+3].type == TokenType::TT){
                parser->currentPtr = i;
                parser->currentContainer = &args;
                parser->parseMode = ParseMode::Matcher;
                try {
                    parser->readTokenTree(false);
                    transition(parser->currentPtr, j + 4, TransitionType::ConsumeTT);
                }
                catch (ParserException& e) {}
                parser->parseMode = ParseMode::Standard;
            }
            // Expression
            else {
                for (int n_i : exprConsumeTransitions[i]){
                    transition(n_i, j + 4, TransitionType::ConsumeExpr);
                }
            }
        }
    }

    // Impossible to interpret arguments with this matcher pattern
    if (dp[args.size()][pattern.size()] == 0) return false;

    // Multiple interpretations of arguments possible
    if (dp[args.size()][pattern.size()] > 1) {
        // TODO: Make this error better (again, highlight a list of tokens)
        throw parser->error(args[0], "Multiple interpretations of macro arguments possible.");
        return false;
    }

    // There is exactly 1 possible interpretation of the arguments.
    int i = args.size(), j = pattern.size();
    vector<Transition> path;
    while (i != 0 || j != 0){
        Transition t = backTransitions[i][j];
        path.emplace_back(i, j, t.type);
        i = t.i;
        j = t.j;
    }
    std::reverse(path.begin(), path.end());

    // Finally, save the meta variables
    vector<int> loopIndices; // Maintains indexes of loop iterations
    i = 0; j = 0;
    for (Transition t : path){
        if (t.type == TransitionType::LoopBegin){
            loopIndices.push_back(0);
        } else if (t.type == TransitionType::LoopIterate){
            loopIndices.back()++;
        } else if (t.type == TransitionType::LoopEnd){
            loopIndices.pop_back();
        } else if (t.type == TransitionType::ConsumeExpr){
            vector<Token> exprTokens;
            for (int k = i; k < t.i; k++){
                exprTokens.push_back(args[k]);
            }
            // Add a dummy semicolon token to terminate the expression
            exprTokens.push_back(Token());
            exprTokens.back().type = TokenType::SEMICOLON;

            // Fetch the expression
            parser->currentPtr = 0;
            parser->currentContainer = &exprTokens;
            ASTNodePtr expr = parser->expression();

            string metaVarName = pattern[j + 1].getLexeme();
            if (!parser->exprMetaVars.contains(metaVarName)){
                parser->exprMetaVars.insert(std::make_pair(metaVarName, std::make_unique<ExprMetaVar>()));
            }
            parser->exprMetaVars[metaVarName]->values[loopIndices] = expr;
            parser->exprMetaVars[metaVarName]->loopDepth = loopIndices.size();
        } else if (t.type == TransitionType::ConsumeTT){
            // Fetch the token tree
            parser->currentPtr = i;
            parser->currentContainer = &args;
            vector<Token> tokenTree = parser->readTokenTree(false);
            string metaVarName = pattern[j + 1].getLexeme();
            if (!parser->ttMetaVars.contains(metaVarName)){
                parser->ttMetaVars.insert(std::make_pair(metaVarName, std::make_unique<TTMetaVar>()));
            }
            parser->ttMetaVars[metaVarName]->values[loopIndices] = tokenTree;
            parser->ttMetaVars[metaVarName]->loopDepth = loopIndices.size();
        }
        i = t.i;
        j = t.j;
    }
    return true;
}

// Checks if matcher pattern contains properly written loops and meta variables.
// Loops are also precalculated here.
void AST::MatchPattern::processPattern() {
    std::unordered_set<string> variables;

    auto isAtEnd = [&](int i) {
        return i >= pattern.size();
    };

    auto check = [&](int i, TokenType type) {
        if (i < 0 || i >= pattern.size()) return false;
        return pattern[i].type == type;
    };

    // Maintains closers and their positions to properly handle macro loops
    vector<TokenType> closers;
    vector<int> closerPositions;

    // Pattern transitions which don't consume tokens (they must form a DAG)
    vector<vector<int>> adj(pattern.size());
    vector<int> inDeg(pattern.size());

    auto addEdge = [&](int u, int v){
        adj[u].push_back(v);
        inDeg[v]++;
    };

    for (int i = 0; i < pattern.size(); i++){
        if (pattern[i].type == TokenType::DOLLAR){
            // Meta variable
            if (check(i+1, TokenType::IDENTIFIER)) {
                string variableName = pattern[i+1].getLexeme();
                if (variables.contains(variableName)){
                    throw parser->error(pattern[i+1], "Meta variable should only appear once in matcher expression.");
                }
                variables.insert(variableName);
                i += 2;
                if (!check(i, TokenType::COLON)) {
                    throw parser->error(pattern[i], "Expected ':' after meta variable.");
                    continue;
                }
                i++;
                if (!check(i, TokenType::EXPR) && !check(i, TokenType::TT)){
                    throw parser->error(pattern[i], "Expected 'expr' or 'tt' type fragments for meta variable.");
                    continue;
                }
                continue;
            }
            // Loop
            if (check(i + 1, TokenType::LEFT_PAREN)){
                closers.push_back(TokenType::LEFT_PAREN);
                closerPositions.push_back(++i);
                continue;
            }
            throw parser->error(pattern[i], "Expected '(' or identifier following '$'.");
        } else {
            if (check(i, TokenType::LEFT_PAREN) || check(i, TokenType::LEFT_BRACE) || check(i, TokenType::LEFT_BRACKET)) {
                closers.push_back(pattern[i].type);
                closerPositions.push_back(-1);
            }
            if (check(i, TokenType::RIGHT_PAREN)) {
                closers.pop_back();
                int startParenIdx = closerPositions.back(); closerPositions.pop_back();
                // This ')' finishes a loop
                if (startParenIdx != -1){
                    int endParenIdx = i++;

                    // Delimiter
                    if (check(i, TokenType::COMMA) || check(i, TokenType::ARROW) || check(i, TokenType::SEMICOLON)){ i++; }

                    // Starting loop paren should be ignored
                    tokenTypes[startParenIdx] = MatcherTokenType::LoopBegin;
                    addEdge(startParenIdx, startParenIdx + 1);

                    // Skippable loop
                    if (check(i, TokenType::STAR)) {
                        // Skip loop
                        tokenTypes[startParenIdx - 1] = MatcherTokenType::Skippable;
                        loopJumps[startParenIdx - 1] = i + 1;
                        addEdge(startParenIdx - 1, i + 1);
                        addEdge(startParenIdx - 1, startParenIdx);

                        // Iterate loop
                        tokenTypes[i] = MatcherTokenType::Iterate;
                        loopJumps[i] = startParenIdx + 1;
                        addEdge(i, startParenIdx + 1);

                        // End loop
                        tokenTypes[endParenIdx] = MatcherTokenType::LoopEnd;
                        loopJumps[endParenIdx] = i + 1;
                        addEdge(endParenIdx, i + 1);
                        addEdge(endParenIdx, endParenIdx + 1);
                    }
                    // Non-skippable loop
                    else if (check(i, TokenType::PLUS)){
                        // Start loop
                        tokenTypes[startParenIdx - 1] = MatcherTokenType::Ignore;
                        addEdge(startParenIdx - 1, startParenIdx);

                        // Iterate loop
                        tokenTypes[i] = MatcherTokenType::Iterate;
                        loopJumps[i] = startParenIdx + 1;
                        addEdge(i, startParenIdx + 1);

                        // End loop
                        tokenTypes[endParenIdx] = MatcherTokenType::LoopEnd;
                        loopJumps[endParenIdx] = i + 1;
                        addEdge(endParenIdx, i + 1);
                        addEdge(endParenIdx, endParenIdx + 1);
                    }
                    // Skippable sub-pattern
                    else if (check(i, TokenType::QUESTIONMARK)){
                        // Start / Skip loop
                        tokenTypes[startParenIdx - 1] = MatcherTokenType::Skippable;
                        loopJumps[startParenIdx - 1] = i + 1;
                        addEdge(startParenIdx - 1, startParenIdx);
                        addEdge(startParenIdx - 1, i + 1);

                        // End loop
                        tokenTypes[endParenIdx] = MatcherTokenType::LoopEnd;
                        loopJumps[endParenIdx] = i + 1;
                        addEdge(endParenIdx, i + 1);
                        addEdge(endParenIdx, endParenIdx + 1);
                    }
                    else {
                        throw parser->error(pattern[i], "Expected '*', '+' or '?' after macro loop");
                    }
                }
            }
            if (check(i, TokenType::RIGHT_BRACE) || check(i, TokenType::RIGHT_BRACKET)) {
                closers.pop_back();
                closerPositions.pop_back();
            }
        }
    }

    // Check if transitions which don't consume tokens form a DAG (and generate the topologic sort with Kahn's algorithm)
    std::queue<int> q;
    for (int i = 0; i < pattern.size(); i++){
        if (inDeg[i] == 0) q.push(i);
    }
    while (!q.empty()){
        int curNode = q.front();
        q.pop();
        topoSort.push_back(curNode);
        for (int neighbour : adj[curNode]){
            if (--inDeg[neighbour] == 0) { q.push(neighbour); }
        }
    }

    if (topoSort.size() != pattern.size()){
        // TODO: Make this better (highlight multiple tokens) and make the error more informative
        throw parser->error(pattern.front(), "Pattern can be ambiguously interpreted.");
    }
}
