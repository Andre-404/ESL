#pragma once
#include "../moduleDefs.h"

namespace AST {

    struct VarDeclDebugInfo {
        Token keyword;
        Token varName;

        VarDeclDebugInfo(const Token &keyword, const Token &varName) : keyword(keyword), varName(varName) {}

        // Default constructor just for it to stop complaining in TypedAST::VarDecl, the debug info is set after VarDecl's creation
        VarDeclDebugInfo() {}
    };

    struct VarStoreDebugInfo {
        Token varName;
        Token op;

        VarStoreDebugInfo(const Token &varName, const Token &op) : varName(varName), op(op) {}
    };

    struct VarReadDebugInfo {
        Token varName;

        explicit VarReadDebugInfo(const Token &varName) : varName(varName) {}
    };

    struct BinaryExprDebugInfo{
        Token op;

        explicit BinaryExprDebugInfo(const Token &op) : op(op) {}
    };

    struct UnaryExprDebugInfo{
        Token op;

        explicit UnaryExprDebugInfo(const Token &op) : op(op) {}
    };

    struct LiteralDebugInfo{
        Token literal;

        explicit LiteralDebugInfo(const Token &literal) : literal(literal) {}
    };

    struct RangeExprDebugInfo{
        Token op;

        explicit RangeExprDebugInfo(const Token &op) : op(op) {}
    };

    struct ArrayLiteralDebugInfo{
        Token bracket1;
        Token bracket2;

        ArrayLiteralDebugInfo(const Token &bracket1, const Token &bracket2) : bracket1(bracket1), bracket2(bracket2) {}
    };

    struct StructDbgInfoField{
        Token str;
        Token colon;

        StructDbgInfoField(const Token &str, const Token &colon) : str(str), colon(colon) {}
    };

    struct StructLiteralDebugInfo{
        Token brace1;
        vector<StructDbgInfoField> fields;
        Token brace2;

        StructLiteralDebugInfo(const Token &brace1, const vector<StructDbgInfoField> &fields, const Token &brace2)
        : brace1(brace1), fields(fields), brace2(brace2) {}
    };

    struct CollectionSetDebugInfo {
        Token accessor;
        Token op;

        CollectionSetDebugInfo(const Token &accessor, const Token &op) : accessor(accessor), op(op) {}
    };

    struct CollectionAccessDebugInfo{
        Token accessor;

        CollectionAccessDebugInfo(const Token &accessor) : accessor(accessor) {}
    };

    struct ConditionalExprDebugInfo{
        Token questionmark;
        Token colon;

        ConditionalExprDebugInfo(const Token &questionmark, const Token &colon) : questionmark(questionmark), colon(colon) {}
    };

    struct CallExprDebugInfo{
        Token paren1;
        Token paren2;

        CallExprDebugInfo(const Token &paren1, const Token &paren2) :
        paren1(paren1), paren2(paren2) {}
    };

    struct InvokeExprDebugInfo{
        Token accessor;
        Token method;
        Token paren1;
        Token paren2;

        InvokeExprDebugInfo(const Token &accessor, const Token &method, const Token &paren1, const Token &paren2) :
                            accessor(accessor), method(method), paren1(paren1), paren2(paren2) {}
    };

    struct NewExprDebugInfo{
        Token keyword;
        Token className;
        Token paren1;
        Token paren2;

        NewExprDebugInfo(const Token &keyword, const Token &className, const Token &paren1, const Token &paren2)
            : keyword(keyword), className(className), paren1(paren1), paren2(paren2) {}
    };

    struct AsyncExprDebugInfo{
        Token keyword;
        Token paren1;
        Token paren2;

        AsyncExprDebugInfo(const Token &keyword, const Token &paren1, const Token &paren2)
                : keyword(keyword), paren1(paren1), paren2(paren2) {}
    };

    struct AwaitExprDebugInfo{
        Token keyword;

        explicit AwaitExprDebugInfo(const Token &keyword) : keyword(keyword) {}
    };

    struct FuncLiteralDebugInfo{
        Token keyword;
        vector<Token> params;

        FuncLiteralDebugInfo(const Token &keyword, const vector<Token> &params) : keyword(keyword), params(params) {}
    };

    struct FuncDeclDebugInfo{
        Token keyword;
        Token name;
        vector<Token> params;

        FuncDeclDebugInfo(const Token &keyword, const Token &name, const vector<Token> &params) :
            keyword(keyword), name(name), params(params) {}
    };

    struct ReturnStmtDebugInfo{
        Token keyword;

        explicit ReturnStmtDebugInfo(const Token &keyword) : keyword(keyword) {}
    };

    struct UncondJmpDebugInfo{
        Token keyword;

        explicit UncondJmpDebugInfo(const Token &keyword) : keyword(keyword) {}
    };

    struct IfStmtDebugInfo{
        Token keyword;

        explicit IfStmtDebugInfo(const Token &keyword) : keyword(keyword) {}
    };

    struct WhileStmtDebugInfo{
        Token keyword;

        explicit WhileStmtDebugInfo(const Token &keyword) : keyword(keyword) {}
    };

    struct SwitchStmtDebugInfo{
        Token keyword;
        vector<Token> cases;

        explicit SwitchStmtDebugInfo(const Token &keyword, const vector<Token> &cases) : keyword(keyword), cases(cases) {}
    };

    struct ClassDeclDebugInfo{
        Token keyword;
        Token className;
        // Optional
        Token colon;

        ClassDeclDebugInfo(const Token &keyword, const Token &className) : keyword(keyword), className(className) {}
    };

    struct MethodDebugInfo{
        Token overrides; // Optional
        Token keyword;
        Token name;
        vector<Token> params;

        MethodDebugInfo(const Token &overrides, const Token &keyword, const Token &name, const vector<Token> &params)
                : overrides(overrides), keyword(keyword), name(name), params(params) {}

        MethodDebugInfo() {} // Default constructor to shut up errors
    };

    struct InstSetDebugInfo{
        Token field;
        Token accessor;
        Token op;

        InstSetDebugInfo(const Token &field, const Token &accessor, const Token &op) : field(field), accessor(accessor),
                                                                                       op(op) {}
    };

    struct InstGetDebugInfo{
        Token field;
        Token accessor;

        InstGetDebugInfo(const Token &field, const Token &accessor) : field(field), accessor(accessor) {}
    };
}