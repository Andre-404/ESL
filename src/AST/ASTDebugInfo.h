#pragma once
#include "../moduleDefs.h"

namespace AST {

    struct VarDeclDebugInfo {
        Token keyword;
        Token varName;
    };

    struct VarStoreDebugInfo {
        Token varName;
        Token op;
    };

    struct VarReadDebugInfo {
        Token varName;
    };

    struct BinaryExprDebugInfo{
        Token op;
    };

    struct UnaryExprDebugInfo{
        Token op;
    };

    struct LiteralDebugInfo{
        Token literal;
    };

    struct RangeExprDebugInfo{
        Token op;
    };

    struct ArrayLiteralDebugInfo{
        Token bracket1;
        Token bracket2;
    };

    struct StructField{
        Token str;
        Token colon;
        Token comma;
    };

    struct StructLiteralDebugInfo{
        Token brace1;
        vector<StructField> fields;
        Token brace2;
    };

    struct CollectionSetDebugInfo {
        Token field;
        Token accessor1;
        Token accessor2;
        Token op;
    };

    struct CollectionAccessDebugInfo{
        Token field;
        Token accessor1;
        Token accessor2;
    };

    struct ConditionalExprDebugInfo{
        Token questionmark;
        Token colon;
    };

    struct CallExprDebugInfo{
        Token paren1;
        vector<Token> commas;
        Token parent2;
    };

    struct InvokeExprDebugInfo{
        Token accessor;
        Token method;
        Token paren1;
        vector<Token> commas;
        Token parent2;
    };

    struct NewExprDebugInfo{
        Token keyword;
        Token className;
        Token paren1;
        vector<Token> commas;
        Token paren2;
    };

    struct AsyncExprDebugInfo{
        Token keyword;
        Token paren1;
        vector<Token> commas;
        Token paren2;
    };

    struct AwaitExprDebugInfo{
        Token keyword;
    };

    struct FuncLiteralDebugInfo{
        Token keyword;
        vector<Token> params;
    };

    struct FuncDeclDebugInfo{
        Token keyword;
        Token name;
        vector<Token> params;
    };

    struct ReturnStmtDebugInfo{
        Token keyword;
    };

    struct UncondJmpDebugInfo{
        Token keyword;
    };

    struct IfStmtDebugInfo{
        Token keyword;
    };

    struct WhileStmtDebugInfo{
        Token keyword;
    };

    struct SwitchStmtDebugInfo{
        Token keyword;
        vector<Token> cases;
    };

    struct ClassDeclDebugInfo{
        Token keyword;
        Token className;
        // Optional
        Token colon;
        Token parent;
    };

    struct MethodDebugInfo{
        Token overrides; // Optional
        Token keyword;
        Token name;
        vector<Token> params;
    };

    struct InstSetDebugInfo{
        Token field;
        Token accessor;
        Token op;
    };

    struct InstGetDebugInfo{
        Token field;
        Token accessor;
    };

    struct SuperExprDebugInfo{
        Token keyword;
        Token accessor;
        Token method;
    };
}