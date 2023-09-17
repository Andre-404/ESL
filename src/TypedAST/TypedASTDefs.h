#pragma once
#include "Types.h"
#include "../AST/ASTDebugInfo.h"
#include <variant>
#include <utility>


// Introduces types for all expressions to the AST
// Lowers some operations to make codegen easier(eg. for loops are transformed into while loops)
// Creates connections between variables reads/stores and variable declarations

namespace llvm{
    class Value;
}

namespace typedAST{
    using std::shared_ptr;
    enum class NodeType{
        VAR_DECL,
        VAR_STORE,
        VAR_READ,
        VAR_NATIVE_READ,

        ARITHEMTIC,
        COMPARISON, // Includes && and ||
        UNARY,

        LITERAL,
        RANGE,

        ARRAY,
        HASHMAP,
        COLLECTION_SET,
        COLLECTION_GET,

        CONDITIONAL,

        CALL,
        INVOKE,
        NEW,

        ASYNC,
        AWAIT,

        CLOSURE, // All local functions fall under this
        FUNC_DECL,
        RETURN,

        UNCOND_JMP,

        IF,
        WHILE,
        SWITCH,

        CLASS_DECL,
        INST_SET,
        INST_GET,
        INST_SUPER_GET,

        // Misc
        BLOCK_EDGE
    };
    class VarDecl;
    class VarRead;
    class VarStore;
    class VarReadNative;
    class ArithmeticExpr;
    class ComparisonExpr;
    class UnaryExpr;
    class LiteralExpr;
    class HashmapExpr;
    class ArrayExpr;
    class CollectionGet;
    class CollectionSet;
    class ConditionalExpr;
    class CallExpr;
    class InvokeExpr;
    class NewExpr;
    class AsyncExpr;
    class AwaitExpr;
    class CreateClosureExpr;
    class RangeExpr;
    class FuncDecl;
    class ReturnStmt;
    class UncondJump;
    class IfStmt;
    class WhileStmt;
    class SwitchStmt;
    class ClassDecl;
    class InstGet;
    class InstSet;
    class InstSuperGet;
    class ScopeEdge;

    class TypedASTVisitor{
    public:
        virtual void visitVarDecl(VarDecl* decl) = 0;
        virtual void visitVarRead(VarRead* expr) = 0;
        virtual void visitVarStore(VarStore* expr) = 0;
        virtual void visitVarReadNative(VarReadNative* expr) = 0;
        virtual void visitArithmeticExpr(ArithmeticExpr* expr) = 0;
        virtual void visitComparisonExpr(ComparisonExpr* expr) = 0;
        virtual void visitUnaryExpr(UnaryExpr* expr) = 0;
        virtual void visitLiteralExpr(LiteralExpr* expr) = 0;
        virtual void visitHashmapExpr(HashmapExpr* expr) = 0;
        virtual void visitArrayExpr(ArrayExpr* expr) = 0;
        virtual void visitCollectionGet(CollectionGet* expr) = 0;
        virtual void visitCollectionSet(CollectionSet* expr) = 0;
        virtual void visitConditionalExpr(ConditionalExpr* expr) = 0;
        virtual void visitCallExpr(CallExpr* expr) = 0;
        virtual void visitInvokeExpr(InvokeExpr* expr) = 0;
        virtual void visitNewExpr(NewExpr* expr) = 0;
        virtual void visitAsyncExpr(AsyncExpr* expr) = 0;
        virtual void visitAwaitExpr(AwaitExpr* expr) = 0;
        virtual void visitCreateClosureExpr(CreateClosureExpr* expr) = 0;
        virtual void visitRangeExpr(RangeExpr* expr) = 0;
        virtual void visitFuncDecl(FuncDecl* expr) = 0;
        virtual void visitReturnStmt(ReturnStmt* expr) = 0;
        virtual void visitUncondJump(UncondJump* expr) = 0;
        virtual void visitIfStmt(IfStmt* expr) = 0;
        virtual void visitWhileStmt(WhileStmt* expr) = 0;
        virtual void visitSwitchStmt(SwitchStmt* expr) = 0;
        virtual void visitClassDecl(ClassDecl* expr) = 0;
        virtual void visitInstGet(InstGet* expr) = 0;
        virtual void visitInstSuperGet(InstSuperGet* expr) = 0;
        virtual void visitInstSet(InstSet* expr) = 0;
        virtual void visitScopeBlock(ScopeEdge* expr) = 0;
    };

    class TypedASTCodegen{
    public:
        virtual llvm::Value* visitVarDecl(VarDecl* decl) = 0;
        virtual llvm::Value* visitVarRead(VarRead* expr) = 0;
        virtual llvm::Value* visitVarStore(VarStore* expr) = 0;
        virtual llvm::Value* visitVarReadNative(VarReadNative* expr) = 0;
        virtual llvm::Value* visitArithmeticExpr(ArithmeticExpr* expr) = 0;
        virtual llvm::Value* visitComparisonExpr(ComparisonExpr* expr) = 0;
        virtual llvm::Value* visitUnaryExpr(UnaryExpr* expr) = 0;
        virtual llvm::Value* visitLiteralExpr(LiteralExpr* expr) = 0;
        virtual llvm::Value* visitHashmapExpr(HashmapExpr* expr) = 0;
        virtual llvm::Value* visitArrayExpr(ArrayExpr* expr) = 0;
        virtual llvm::Value* visitCollectionGet(CollectionGet* expr) = 0;
        virtual llvm::Value* visitCollectionSet(CollectionSet* expr) = 0;
        virtual llvm::Value* visitConditionalExpr(ConditionalExpr* expr) = 0;
        virtual llvm::Value* visitCallExpr(CallExpr* expr) = 0;
        virtual llvm::Value* visitInvokeExpr(InvokeExpr* expr) = 0;
        virtual llvm::Value* visitNewExpr(NewExpr* expr) = 0;
        virtual llvm::Value* visitAsyncExpr(AsyncExpr* expr) = 0;
        virtual llvm::Value* visitAwaitExpr(AwaitExpr* expr) = 0;
        virtual llvm::Value* visitCreateClosureExpr(CreateClosureExpr* expr) = 0;
        virtual llvm::Value* visitRangeExpr(RangeExpr* expr) = 0;
        virtual llvm::Value* visitFuncDecl(FuncDecl* expr) = 0;
        virtual llvm::Value* visitReturnStmt(ReturnStmt* expr) = 0;
        virtual llvm::Value* visitUncondJump(UncondJump* expr) = 0;
        virtual llvm::Value* visitIfStmt(IfStmt* expr) = 0;
        virtual llvm::Value* visitWhileStmt(WhileStmt* expr) = 0;
        virtual llvm::Value* visitSwitchStmt(SwitchStmt* expr) = 0;
        virtual llvm::Value* visitClassDecl(ClassDecl* expr) = 0;
        virtual llvm::Value* visitInstGet(InstGet* expr) = 0;
        virtual llvm::Value* visitInstSuperGet(InstSuperGet* expr) = 0;
        virtual llvm::Value* visitInstSet(InstSet* expr) = 0;
        virtual llvm::Value* visitScopeBlock(ScopeEdge* expr) = 0;
    };

    class TypedASTNode {
    public:
        NodeType type;

        virtual ~TypedASTNode() {};
        virtual void accept(TypedASTVisitor* vis) = 0;
        virtual llvm::Value* codegen(TypedASTCodegen* vis) = 0;
    };
    using nodePtr = shared_ptr<TypedASTNode>;

    class TypedASTExpr : public TypedASTNode{
    public:
        types::tyVarIdx exprType;

        virtual ~TypedASTExpr() {};
        virtual void accept(TypedASTVisitor* vis) = 0;
        virtual llvm::Value* codegen(TypedASTCodegen* vis) = 0;
    };
    using exprPtr = shared_ptr<TypedASTExpr>;

    enum class VarType{
        LOCAL,
        FREEVAR,
        GLOBAL,
        // To optimize calling functions/instantiating classes
        GLOBAL_FUNC,
        GLOBAL_CLASS
    };
    class VarDecl : public TypedASTNode{
    public:
        VarType varType;
        // Constrained by the type given in constructor
        bool typeConstrained;
        types::tyVarIdx possibleTypes;
        AST::VarDeclDebugInfo dbgInfo;
        uInt64 uuid;
        static inline uInt64 instanceCount = 0;

        VarDecl(VarType _varType, types::tyVarIdx _possibleTypes, bool _typeConstrained = false){
            varType = _varType;
            uuid = instanceCount++;
            typeConstrained = _typeConstrained;
            possibleTypes = _possibleTypes;
            type = NodeType::VAR_DECL;
        }
        ~VarDecl() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitVarDecl(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitVarDecl(this);
        }
    };
    class VarRead : public TypedASTExpr{
    public:
        shared_ptr<VarDecl> varPtr;
        AST::VarReadDebugInfo dbgInfo;

        VarRead(shared_ptr<VarDecl> _varPtr, AST::VarReadDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            varPtr = _varPtr;
            exprType = varPtr->possibleTypes;
            type = NodeType::VAR_READ;
        }
        ~VarRead() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitVarRead(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitVarRead(this);
        }
    };
    class VarStore : public TypedASTExpr{
    public:
        shared_ptr<VarDecl> varPtr;
        exprPtr toStore;
        AST::VarStoreDebugInfo dbgInfo;

        VarStore(shared_ptr<VarDecl> _varPtr, exprPtr _toStore, AST::VarStoreDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            varPtr = _varPtr;
            toStore = _toStore;
            exprType = _toStore->exprType;
            type = NodeType::VAR_STORE;
        }
        ~VarStore() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitVarStore(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitVarStore(this);
        }
    };
    class VarReadNative : public TypedASTExpr{
    public:
        string nativeName;
        AST::VarReadDebugInfo dbgInfo;

        VarReadNative(string name, types::tyVarIdx _heldType, AST::VarReadDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            nativeName = name;
            exprType = _heldType;
            type = NodeType::VAR_NATIVE_READ;
        }
        ~VarReadNative() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitVarReadNative(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitVarReadNative(this);
        }
    };

    enum class ArithmeticOp{
        ADD,
        SUB,
        MUL,
        DIV,
        IDIV,
        MOD,
        AND,
        OR,
        XOR,
        BITSHIFT_L,
        BITSHIFT_R,
    };
    class ArithmeticExpr : public TypedASTExpr{
    public:
        ArithmeticOp opType;
        exprPtr lhs;
        exprPtr rhs;
        AST::BinaryExprDebugInfo dbgInfo;

        ArithmeticExpr(exprPtr _lhs, exprPtr _rhs, ArithmeticOp _op, types::tyVarIdx _exprTy, AST::BinaryExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            lhs = _lhs;
            rhs = _rhs;
            opType = _op;
            exprType = _exprTy;
            type = NodeType::ARITHEMTIC;
        }
        ~ArithmeticExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitArithmeticExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitArithmeticExpr(this);
        }
    };

    enum class ComparisonOp{
        LESS,
        LESSEQ,
        GREAT,
        GREATEQ,
        EQUAL,
        NOT_EQUAL,
        AND,
        OR,
        INSTANCEOF
    };
    class ComparisonExpr : public TypedASTExpr{
    public:
        ComparisonOp opType;
        exprPtr lhs;
        exprPtr rhs;
        AST::BinaryExprDebugInfo dbgInfo;

        ComparisonExpr(exprPtr _lhs, exprPtr _rhs, ComparisonOp _op, types::tyVarIdx _exprTy, AST::BinaryExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            lhs = _lhs;
            rhs = _rhs;
            opType = _op;
            exprType = _exprTy;
            type = NodeType::COMPARISON;
        }
        ~ComparisonExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitComparisonExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitComparisonExpr(this);
        }
    };

    enum class UnaryOp{
        INC_PRE,
        INC_POST,
        DEC_PRE,
        DEC_POST,
        NEG,
        FNEG,
        BIN_NEG,
    };
    class UnaryExpr : public TypedASTExpr{
    public:
        UnaryOp opType;
        exprPtr rhs;
        AST::UnaryExprDebugInfo dbgInfo;

        UnaryExpr(exprPtr _rhs, UnaryOp _op, types::tyVarIdx _exprTy, AST::UnaryExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            rhs = _rhs;
            opType = _op;
            type = NodeType::UNARY;
            exprType = _exprTy;
        }
        ~UnaryExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitUnaryExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitUnaryExpr(this);
        }
    };

    class LiteralExpr : public TypedASTExpr{
    public:
        // int is used to signal a nil value
        std::variant<double, bool, void*, string> val;
        AST::LiteralDebugInfo dbgInfo;

        LiteralExpr(std::variant<double, bool, void*, string> variant, types::tyVarIdx _exprTy, AST::LiteralDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            val = variant;
            exprType = _exprTy;
            type = NodeType::LITERAL;
        }
        ~LiteralExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitLiteralExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitLiteralExpr(this);
        }
    };

    class HashmapExpr : public TypedASTExpr{
    public:
        // Fields sorted in the order they appear in, important to eval them in that order
        vector<std::pair<string, exprPtr>> fields;
        AST::StructLiteralDebugInfo dbgInfo;

        HashmapExpr(vector<std::pair<string, exprPtr>> _fields, types::tyVarIdx _exprTy, AST::StructLiteralDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            fields = _fields;
            exprType = _exprTy;
            type = NodeType::HASHMAP;
        }
        ~HashmapExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitHashmapExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitHashmapExpr(this);
        }
    };
    class ArrayExpr : public TypedASTExpr{
    public:
        vector<exprPtr> fields;
        AST::ArrayLiteralDebugInfo dbgInfo;

        ArrayExpr(vector<exprPtr> _fields, types::tyVarIdx _exprTy, AST::ArrayLiteralDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            fields = _fields;
            exprType = _exprTy;
            type = NodeType::ARRAY;
        }
        ~ArrayExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitArrayExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitArrayExpr(this);
        }
    };

    class CollectionGet : public TypedASTExpr{
    public:
        exprPtr collection;
        exprPtr field;
        AST::CollectionAccessDebugInfo dbgInfo;

        CollectionGet(exprPtr _collection, exprPtr _field, types::tyVarIdx _exprTy, AST::CollectionAccessDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            collection = _collection;
            field = _field;
            exprType = _exprTy;
            type = NodeType::COLLECTION_GET;
        }
        ~CollectionGet() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitCollectionGet(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitCollectionGet(this);
        }
    };
    enum class SetType{
        SET,
        ADD_SET,
        SUB_SET,
        MUL_SET,
        DIV_SET,
        REM_SET,
        AND_SET,
        OR_SET,
        XOR_SET,
    };
    class CollectionSet : public TypedASTExpr{
    public:
        exprPtr collection;
        exprPtr field;
        exprPtr toStore;
        SetType operationType;
        AST::CollectionSetDebugInfo dbgInfo;

        CollectionSet(exprPtr _collection, exprPtr _field, exprPtr _toStore, SetType _operationType, AST::CollectionSetDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            collection = _collection;
            field = _field;
            toStore = _toStore;
            operationType = _operationType;
            exprType = toStore->exprType;
            type = NodeType::COLLECTION_SET;
        }
        ~CollectionSet() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitCollectionSet(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitCollectionSet(this);
        }
    };

    class ConditionalExpr : public TypedASTExpr{
    public:
        exprPtr cond;
        exprPtr thenExpr;
        exprPtr elseExpr;
        AST::ConditionalExprDebugInfo dbgInfo;

        ConditionalExpr(exprPtr _cond, exprPtr _thenExpr, exprPtr _elseExpr, types::tyVarIdx _exprTy, AST::ConditionalExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            cond = _cond;
            thenExpr = _thenExpr;
            elseExpr = _elseExpr;
            exprType = _exprTy;
            type = NodeType::CONDITIONAL;
        }
        ~ConditionalExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitConditionalExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitConditionalExpr(this);
        }
    };

    class CallExpr : public TypedASTExpr{
    public:
        exprPtr callee;
        vector<exprPtr> args;
        AST::CallExprDebugInfo dbgInfo;

        CallExpr(exprPtr _callee, vector<exprPtr> _args, types::tyVarIdx _callDeferred, AST::CallExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            callee = _callee;
            args = _args;
            exprType = _callDeferred;
            type = NodeType::CALL;
        }
        ~CallExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitCallExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitCallExpr(this);
        }
    };
    class InvokeExpr : public TypedASTExpr{
    public:
        exprPtr inst;
        vector<exprPtr> args;
        string field;
        types::tyVarIdx classTy;
        // Pointer to a variable which holds the class
        std::shared_ptr<typedAST::VarDecl> klass;
        AST::InvokeExprDebugInfo dbgInfo;

        InvokeExpr(exprPtr _inst, string _field, vector<exprPtr> _args, types::tyVarIdx _exprTy,
                   AST::InvokeExprDebugInfo _dbgInfo, std::shared_ptr<typedAST::VarDecl> _klass = nullptr) : dbgInfo(_dbgInfo){
            inst = _inst;
            field = _field;
            args = _args;
            // Used for super invokes
            klass = _klass;
            // TODO: actually do type inference on this
            exprType = _exprTy;
            type = NodeType::INVOKE;
        }
        ~InvokeExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitInvokeExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitInvokeExpr(this);
        }
    };
    class NewExpr : public TypedASTExpr{
    public:
        exprPtr callee;
        vector<exprPtr> args;
        AST::NewExprDebugInfo dbgInfo;

        NewExpr(exprPtr _callee, vector<exprPtr> _args, types::tyVarIdx _instType, AST::NewExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            callee = _callee;
            args = _args;
            exprType = _instType;
            type = NodeType::NEW;
        }
        ~NewExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitNewExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitNewExpr(this);
        }
    };

    class AsyncExpr : public TypedASTExpr{
    public:
        exprPtr callee;
        vector<exprPtr> args;
        AST::AsyncExprDebugInfo dbgInfo;

        AsyncExpr(exprPtr _callee, vector<exprPtr> _args, types::tyVarIdx _futTy, AST::AsyncExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            callee = _callee;
            args = _args;
            // Expr that possibly holds a function type of the called function
            exprType = _futTy;
            type = NodeType::ASYNC;
        }
        ~AsyncExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitAsyncExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitAsyncExpr(this);
        }
    };
    class AwaitExpr : public TypedASTExpr{
    public:
        exprPtr expr;
        AST::AwaitExprDebugInfo dbgInfo;

        AwaitExpr(exprPtr _expr, types::tyVarIdx _futAwaitTy, AST::AwaitExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            expr = _expr;
            // Expr type that possibly holds a future type that holds the possible return types of the called function
            exprType = _futAwaitTy;
            type = NodeType::AWAIT;
        }
        ~AwaitExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitAwaitExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitAwaitExpr(this);
        }
    };

    struct Block{
        vector<nodePtr> stmts;
        // If this block terminates no need to put a br instruction at the end of it when emitting IR
        bool terminates;
        Block(){
            terminates = false;
        }
    };
    struct Function{
        Block block;
        vector<shared_ptr<VarDecl>> args;
        shared_ptr<types::FunctionType> fnTy;
        string name;

        Function(){
            name = "";
            fnTy = nullptr;
        }
        Function(shared_ptr<types::FunctionType> _fnTy){
            name = "";
            fnTy = _fnTy;
        }
    };

    class CreateClosureExpr : public TypedASTExpr{
    public:
        std::shared_ptr<Function> fn;
        // First ptr is pointer to the VarDecl from an outer function to store to the closure(can be local/freevar),
        // second is to the VarDecl used inside this function
        vector<std::pair<shared_ptr<VarDecl>, shared_ptr<VarDecl>>> freevars;
        AST::FuncLiteralDebugInfo dbgInfo;

        CreateClosureExpr(std::shared_ptr<Function> _fn, vector<std::pair<shared_ptr<VarDecl>, shared_ptr<VarDecl>>> _freevars,
                          types::tyVarIdx ty, AST::FuncLiteralDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            fn = _fn;
            exprType = ty;
            freevars = _freevars;
            type = NodeType::CLOSURE;
        }
        ~CreateClosureExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitCreateClosureExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitCreateClosureExpr(this);
        }
    };
    class FuncDecl : public TypedASTNode{
    public:
        std::shared_ptr<Function> fn;
        AST::FuncDeclDebugInfo dbgInfo;
        // To store the function object to a variable
        uInt64 globalVarUuid;

        FuncDecl(std::shared_ptr<Function> _fn, AST::FuncDeclDebugInfo _dbgInfo, uInt64 _globalVarUuid) : dbgInfo(_dbgInfo){
            fn = _fn;
            globalVarUuid = _globalVarUuid;
            type = NodeType::FUNC_DECL;
        }
        ~FuncDecl() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitFuncDecl(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitFuncDecl(this);
        }
    };

    class ReturnStmt : public TypedASTNode{
    public:
        exprPtr expr;
        AST::ReturnStmtDebugInfo dbgInfo;

        ReturnStmt(exprPtr _expr, AST::ReturnStmtDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            expr = _expr;
            type = NodeType::RETURN;
        }
        ~ReturnStmt() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitReturnStmt(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitReturnStmt(this);
        }
    };

    class RangeExpr : public TypedASTExpr{
    public:
        exprPtr lhs;
        exprPtr rhs;
        bool isEndInclusive;
        AST::RangeExprDebugInfo dbgInfo;

        RangeExpr(exprPtr _lhs, exprPtr _rhs, bool _isEndInclusive, types::tyVarIdx _ty, AST::RangeExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            lhs = _lhs;
            rhs = _rhs;
            isEndInclusive = _isEndInclusive;
            exprType = _ty;
            type = NodeType::RANGE;
        }
        ~RangeExpr() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitRangeExpr(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitRangeExpr(this);
        }
    };

    enum class JumpType{
        BREAK,
        CONTINUE,
        ADVANCE
    };
    class UncondJump : public TypedASTNode{
    public:
        JumpType jmpType;
        AST::UncondJmpDebugInfo dbgInfo;

        UncondJump(JumpType _jmpType, AST::UncondJmpDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            jmpType = _jmpType;
            type = NodeType::UNCOND_JMP;
        }
        ~UncondJump() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitUncondJump(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitUncondJump(this);
        }
    };

    class IfStmt : public TypedASTNode{
    public:
        exprPtr cond;
        Block thenBlock;
        Block elseBlock;
        AST::IfStmtDebugInfo dbgInfo;

        IfStmt(exprPtr _cond, Block _thenBlock, Block _elseBlock, AST::IfStmtDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            cond = _cond;
            thenBlock = _thenBlock;
            elseBlock = _elseBlock;
            type = NodeType::IF;
        }
        ~IfStmt() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitIfStmt(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitIfStmt(this);
        }
    };
    class WhileStmt : public TypedASTNode{
    public:
        exprPtr cond;
        Block loopBody;
        // Used to implement for loop
        exprPtr afterLoopExpr;
        AST::WhileStmtDebugInfo dbgInfo;

        WhileStmt(exprPtr _cond, Block _loopBody, AST::WhileStmtDebugInfo _dbgInfo, exprPtr _afterLoopExpr = nullptr) : dbgInfo(_dbgInfo){
            cond = _cond;
            loopBody = _loopBody;
            afterLoopExpr = _afterLoopExpr;
            type = NodeType::WHILE;
        }
        ~WhileStmt() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitWhileStmt(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitWhileStmt(this);
        }
    };

    enum class SwitchConstantsType{
        ALL_INT = 0,
        ALL_NUM = 1,
        ALL_STRING = 2,
        MIXED = 3
    };
    class SwitchStmt : public TypedASTNode{
    public:
        exprPtr cond;
        // Each constant has a literal and the index of the case it leads to
        // For the default case ptr is null
        vector<std::pair<std::variant<double, void*, bool, string>, int>> constants;
        int defaultCaseBlockNum;
        vector<Block> cases;
        SwitchConstantsType constantsType;
        AST::SwitchStmtDebugInfo dbgInfo;

        SwitchStmt(exprPtr _cond, vector<std::pair<std::variant<double, void*, bool, string>, int>>& _constants,
                   vector<Block> _cases, SwitchConstantsType _ty, int _defaultCaseBlockNum, AST::SwitchStmtDebugInfo _dbgInfo)
            : dbgInfo(_dbgInfo){
            cond = _cond;
            constants = _constants;
            cases = _cases;
            constantsType = _ty;
            defaultCaseBlockNum = _defaultCaseBlockNum;
            type = NodeType::SWITCH;
        }
        ~SwitchStmt() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitSwitchStmt(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitSwitchStmt(this);
        }
    };

    struct ClassMethod {
        std::shared_ptr<Function> code;
        AST::MethodDebugInfo dbg;

        ClassMethod(){
            code = nullptr;
        }
        ClassMethod(std::shared_ptr<Function> _code, AST::MethodDebugInfo _dbg){
            code = _code;
            dbg = _dbg;
        }
    };

    class ClassDecl : public TypedASTNode{
    public:
        // Privates are prefixed with "priv."
        // Int is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, int> fields;
        std::unordered_map<string, std::pair<ClassMethod, int>> methods;
        std::shared_ptr<types::ClassType> classType;
        shared_ptr<VarDecl> parentClass; // Used for chaining classes to implement 'instanceof' operator
        // To store the class object to a variable
        uInt64 globalVarUuid;

        AST::ClassDeclDebugInfo dbgInfo;

        ClassDecl(std::shared_ptr<types::ClassType> ty, AST::ClassDeclDebugInfo _dbgInfo, uInt64 _globalVarUuid,
                  shared_ptr<VarDecl> _parentClass) : dbgInfo(_dbgInfo){
            classType = ty;
            parentClass = _parentClass;
            globalVarUuid = _globalVarUuid;
            type = NodeType::CLASS_DECL;
        }
        ~ClassDecl() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitClassDecl(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitClassDecl(this);
        }
        void inherit(std::shared_ptr<ClassDecl> parent){
            methods = parent->methods;
            fields = parent->fields;
        }
    };
    class InstGet : public TypedASTExpr{
    public:
        exprPtr instance;
        string field;
        AST::InstGetDebugInfo dbgInfo;

        InstGet(exprPtr _instance, string _field, types::tyVarIdx _instGetTy, AST::InstGetDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            instance = _instance;
            field = _field;
            exprType = _instGetTy;
            type = NodeType::INST_GET;
        }
        ~InstGet() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitInstGet(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitInstGet(this);
        }
    };
    class InstSuperGet : public TypedASTExpr{
    public:
        exprPtr instance;
        // Pointer to a variable which holds the class
        std::shared_ptr<typedAST::VarDecl> klass;
        string method;
        AST::SuperExprDebugInfo dbgInfo;

        InstSuperGet(exprPtr _instance, string _method, std::shared_ptr<typedAST::VarDecl> _klass, types::tyVarIdx methodTy,
                     AST::SuperExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            instance = _instance;
            method = _method;
            klass = _klass;
            // ASTToTypedAST checks if class ty contains method
            exprType = methodTy;
            type = NodeType::INST_SUPER_GET;
        }
        ~InstSuperGet() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitInstSuperGet(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitInstSuperGet(this);
        }
    };
    class InstSet : public TypedASTExpr{
    public:
        exprPtr instance;
        string field;
        exprPtr toStore;
        SetType operationType;
        AST::InstSetDebugInfo dbgInfo;

        InstSet(exprPtr _instance, string _field, exprPtr _toStore, SetType _operationType, AST::InstSetDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            instance = _instance;
            field = _field;
            toStore = _toStore;
            operationType = _operationType;
            exprType = toStore->exprType;
            type = NodeType::INST_SET;
        }
        ~InstSet() {};
        void accept(TypedASTVisitor* vis) override{
            vis->visitInstSet(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitInstSet(this);
        }

    };

    // Misc
    enum class ScopeEdgeType{
        START,
        END
    };
    // Used for better debug info and to know which variables to remove when exiting scope
    class ScopeEdge : public TypedASTNode{
    public:
        // Start or end edge of scope
        ScopeEdgeType edgeType;
        // Variables to pop from the variable map
        std::unordered_set<shared_ptr<VarDecl>> toPop;
        ScopeEdge(ScopeEdgeType _ty, std::unordered_set<shared_ptr<VarDecl>> _toPop){
            edgeType = _ty;
            toPop = _toPop;
            type = NodeType::BLOCK_EDGE;
        }
        ~ScopeEdge() {}
        void accept(TypedASTVisitor* vis) override{
            vis->visitScopeBlock(this);
        }
        llvm::Value* codegen(TypedASTCodegen* vis) override{
            return vis->visitScopeBlock(this);
        }
    };
}