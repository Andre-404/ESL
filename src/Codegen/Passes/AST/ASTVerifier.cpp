#include "ASTVerifier.h"
#include "../../../ErrorHandling/errorHandler.h"
#include "../../../Includes/fmt/format.h"
#include <unordered_map>

using namespace AST;

ASTVerifier::ASTVerifier(errorHandler::ErrorHandler& errHandler) : errHandler(errHandler) {
    loopDepth = 0;
    switchDepth = 0;
}

void ASTVerifier::process(vector<ASTModule> &units) {
    for(ASTModule& module : units){
        loopDepth = 0;
        switchDepth = 0;
        std::unordered_map<string, Token> declarations;
        for(auto stmt : module.stmts){
            // Check for name conflicts within the same module
            if(stmt->type == ASTType::CLASS || stmt->type == ASTType::FUNC || stmt->type == ASTType::VAR){
                shared_ptr<ASTDecl> decl = std::reinterpret_pointer_cast<ASTDecl>(stmt);
                Token declName = decl->getName();
                if(declarations.contains(declName.getLexeme())){
                    errHandler.reportError(fmt::format("Error, {} already defined.", declarations[declName.getLexeme()].getLexeme()),
                                           declarations[declName.getLexeme()]);
                    errHandler.reportError(fmt::format("Error, redefinition of {}.", declName.getLexeme()), declName);
                }
                declarations[declName.getLexeme()] = declName;
            }
            stmt->accept(this);
        }
        checkAliasConflict(module);
        checkNamingConflicts(module, units);
    }
}

void ASTVerifier::visitAssignmentExpr(AST::AssignmentExpr* expr) {
    expr->value->accept(this);
}
void ASTVerifier::visitSetExpr(AST::SetExpr* expr){
    expr->callee->accept(this);
    expr->field->accept(this);
    expr->value->accept(this);
}
void ASTVerifier::visitConditionalExpr(AST::ConditionalExpr* expr) {
    expr->condition->accept(this);
    expr->mhs->accept(this);
    expr->rhs->accept(this);
}
// Binary ops, module access(::) and macro invocation(!)
static bool isComparisonOp(const Token token){
    auto t = token.type;
    return (t == TokenType::LESS || t == TokenType::LESS_EQUAL ||
            t == TokenType::GREATER || t== TokenType::GREATER_EQUAL);
}
void ASTVerifier::visitBinaryExpr(AST::BinaryExpr* expr){
    // Chaining comparison ops is forbidden, here lhs is checked against op of this binary expr,
    // After parsing rhs, rhs is compared to op of this binary expr
    // TODO: move this to SemanticVerifer
    if(expr->left->type == ASTType::BINARY){
        auto op = std::static_pointer_cast<BinaryExpr>(expr->left)->op;
        if(isComparisonOp(op) && isComparisonOp(expr->op)){
            errHandler.reportWarning("Comparisons like 'X<=Y<=Z' don't have their mathematical meaning.", expr->op);
        }
    }
    expr->left->accept(this);
    expr->right->accept(this);
}
void ASTVerifier::visitUnaryExpr(AST::UnaryExpr* expr){
    expr->right->accept(this);
}
void ASTVerifier::visitCallExpr(AST::CallExpr* expr) {
    expr->callee->accept(this);
    for(auto arg : expr->args) arg->accept(this);
}
void ASTVerifier::visitNewExpr(AST::NewExpr* expr) {
    expr->call->accept(this);
}
void ASTVerifier::visitFieldAccessExpr(AST::FieldAccessExpr* expr) {
    expr->callee->accept(this);
    expr->field->accept(this);
}
void ASTVerifier::visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) {
    for(auto _expr : expr->members) _expr->accept(this);
}
void ASTVerifier::visitStructLiteralExpr(AST::StructLiteral* expr) {
    for(StructEntry& pair : expr->fields) pair.expr->accept(this);
}
void ASTVerifier::visitLiteralExpr(AST::LiteralExpr* expr) {
    // Nothing
}
void ASTVerifier::visitFuncLiteral(AST::FuncLiteral* expr) {
    int temploopdepth = loopDepth;
    int tempswitchdepth = switchDepth;
    expr->body->accept(this);
    loopDepth = temploopdepth;
    switchDepth = tempswitchdepth;
}
void ASTVerifier::visitModuleAccessExpr(AST::ModuleAccessExpr* expr) {
    // Nothing
}
void ASTVerifier::visitMacroExpr(AST::MacroExpr* expr) {
    // Nothing
}

void ASTVerifier::visitVarDecl(AST::VarDecl* decl) {
    if(decl->value) decl->value->accept(this);
}
void ASTVerifier::visitFuncDecl(AST::FuncDecl* decl) {
    int temploopdepth = loopDepth;
    int tempswitchdepth = switchDepth;
    decl->body->accept(this);
    loopDepth = temploopdepth;
    switchDepth = tempswitchdepth;
}

void ASTVerifier::visitClassDecl(AST::ClassDecl* decl) {
    std::unordered_map<string, Token> fieldNames;
    std::unordered_map<string, Token> methodNames;
    for(auto method : decl->methods) {
        int temploopdepth = loopDepth;
        int tempswitchdepth = switchDepth;
        method.method->accept(this);
        loopDepth = temploopdepth;
        switchDepth = tempswitchdepth;
        // Naming conflict check, can't check for field name conflicts because they haven't been added to the map yet
        // Field loop takes care of the method-field conflict
        Token methodName = method.method->name;
        if (methodNames.contains(methodName.getLexeme())) {
            errHandler.reportError("Redefinition of method.", methodName);
            errHandler.reportError("Method first defined here.", methodNames[methodName.getLexeme()]);
        }
        if(methodName.getLexeme() == decl->name.getLexeme()){
            errHandler.reportWarning("Constructor isn't declared pub, won't be called when creating class.", methodName);
        }
        methodNames[methodName.getLexeme()] = methodName;
    }
    // Naming conflict check
    for(auto field : decl->fields){
        Token fieldName = field.field;
        if(fieldNames.contains(fieldName.getLexeme())){
            errHandler.reportError("Field redefinition.", fieldName);
            errHandler.reportError("Field first defined here.", fieldNames[fieldName.getLexeme()]);
        }else if(methodNames.contains(fieldName.getLexeme())){
            Token methodName = methodNames[fieldName.getLexeme()];
            // Informs the user as to what was declared first, method or field
            string err1 = fieldName.str.start > methodName.str.start ? "Method redefined as a field." : "Field redefined as a method.";
            string err2 = fieldName.str.start > methodName.str.start ? "Method first defined here." : "Field first defined here.";
            errHandler.reportError(err1, fieldName);
            errHandler.reportError(err2, methodNames[fieldName.getLexeme()]);
        }
        fieldNames[fieldName.getLexeme()] = fieldName;
    }
}

void ASTVerifier::visitExprStmt(AST::ExprStmt* stmt) {
    stmt->expr->accept(this);
}
void ASTVerifier::visitSpawnStmt(AST::SpawnStmt* stmt) {
    stmt->callExpr->accept(this);
}
void ASTVerifier::visitBlockStmt(AST::BlockStmt* stmt) {
    for(auto _stmt : stmt->statements) _stmt->accept(this);
}
void ASTVerifier::visitIfStmt(AST::IfStmt* stmt) {
    stmt->condition->accept(this);
    stmt->thenBranch->accept(this);
    if(stmt->elseBranch) stmt->elseBranch->accept(this);
}
void ASTVerifier::visitWhileStmt(AST::WhileStmt* stmt) {
    loopDepth++;
    stmt->condition->accept(this);
    stmt->body->accept(this);
    loopDepth--;
}
void ASTVerifier::visitForStmt(AST::ForStmt* stmt) {
    loopDepth++;
    if(stmt->init) stmt->init->accept(this);
    if(stmt->condition) stmt->condition->accept(this);
    if(stmt->increment) stmt->increment->accept(this);
    stmt->body->accept(this);
    loopDepth--;
}
void ASTVerifier::visitBreakStmt(AST::BreakStmt* stmt) {
    if (loopDepth == 0 && switchDepth == 0)
        errHandler.reportError("Cannot use 'break' outside of loops or switch statements.", stmt->keyword);
}
void ASTVerifier::visitContinueStmt(AST::ContinueStmt* stmt) {
    if (loopDepth == 0)
        errHandler.reportError("Cannot use 'continue' outside of loops.", stmt->keyword);
}
// Can't use the visit method because we need to pass vector for switch constants
void ASTVerifier::handleCaseStmt(AST::CaseStmt& _case, vector<Token> allSwitchConstants){
    unordered_map<string, Token> constants;
    // Checks constant uniqueness
    for(auto c : _case.constants){
        int index = -1;
        for(int i = 0; i < allSwitchConstants.size(); i++){
            if(allSwitchConstants[i].equals(c)){
                index = i;
                break;
            }
        }
        if(index != -1){
            errHandler.reportError("Duplicate case value.", c);
            errHandler.reportError("Value already in a case here.", allSwitchConstants[index]);
        }else allSwitchConstants.push_back(c);
    }
    for(auto stmt : _case.stmts) stmt->accept(this);
}

void ASTVerifier::visitSwitchStmt(AST::SwitchStmt* stmt) {
    switchDepth++;
    stmt->expr->accept(this);
    bool hasDefault = false;
    vector<Token> allSwitchConstants;
    for(auto _case : stmt->cases) {
        if(_case->caseType.type == TokenType::DEFAULT) {
            if (hasDefault) {
                errHandler.reportError("Only 1 default case is allowed inside a switch statement.", _case->caseType);
            } else hasDefault = true;
        }
        handleCaseStmt(*_case, allSwitchConstants);
    }
    switchDepth--;
}
void ASTVerifier::visitCaseStmt(AST::CaseStmt* _case) {
    // Empty, handled in handleCaseStmt
}
void ASTVerifier::visitAdvanceStmt(AST::AdvanceStmt* stmt) {
    if (switchDepth == 0) errHandler.reportError("Cannot use 'advance' outside of switch statements.", stmt->keyword);
}
void ASTVerifier::visitReturnStmt(AST::ReturnStmt* stmt) {
    if (stmt->expr) stmt->expr->accept(this);
}
// Checks for duplicate aliases within the same module
void ASTVerifier::checkAliasConflict(ASTModule& module){
    unordered_map<string, Token> aliases;
    for(auto [alias, depIdx] : module.importedModules){
        if(alias.type == TokenType::NONE) continue;
        if(aliases.contains(alias.getLexeme())){
            errHandler.reportError("Alias already declared here.", aliases[alias.getLexeme()]);
            errHandler.reportError("Duplicate use of alias.", alias);
        }else aliases[alias.getLexeme()] = alias;
    }
}

void ASTVerifier::checkNamingConflicts(ASTModule& module, vector<ASTModule>& units){
    unordered_map<string, string> symbols;
    // Checks for duplicate symbols in imported files
    for(int i = 0; i < module.importedModules.size(); i++){
        auto [alias, depIdx] = module.importedModules[i];
        Token pathToken = module.importedModulesPath[i];
        if(alias.type != TokenType::NONE) continue;

        for(auto exportedDecl : units[depIdx].exports){
            string lexeme = exportedDecl->getName().getLexeme();
            string depPath = units[depIdx].file->path;
            if(symbols.contains(lexeme)){
                string str = fmt::format("Ambiguous definition, symbol '{}' defined in '{}' and redefined in '{}', consider using an alias when importing.",
                                         lexeme, symbols[lexeme], depPath);
                errHandler.reportError(str, pathToken);
            }else symbols[lexeme] = depPath;
        }
    }
    // Checks if there is a conflict between the symbols defined in this module and its dependencies
    for(auto exportedDecl : module.exports){
        string lexeme = exportedDecl->getName().getLexeme();
        if(symbols.contains(lexeme)){
            string str = fmt::format("Ambiguous definition, symbol '{}' defined in '{}' and redefined here, consider using an alias when importing.",
                                     lexeme, symbols[lexeme]);
            errHandler.reportError(str, exportedDecl->getName());
        }
    }
}
