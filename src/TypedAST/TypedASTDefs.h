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

namespace CFG{
    using std::shared_ptr;
    enum class NodeType{
        VAR_DECL,
        VAR_STORE,
        VAR_READ,
        VAR_NATIVE_READ,

        ARITHEMTIC,
        COMPARISON, // Includes && and ||
        INSTANCEOF,
        UNARY,

        LITERAL,

        ARRAY,
        HASHMAP,
        COLLECTION_SET,
        COLLECTION_GET,

        CONDITIONAL,

        CALL,
        INVOKE,
        NEW,

        SPAWN,

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

        // Misc
        BLOCK_EDGE
    };
    class VarDecl;
    class VarRead;
    class VarStore;
    class VarReadNative;
    class ArithmeticExpr;
    class ComparisonExpr;
    class InstanceofExpr;
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
    class SpawnStmt;
    class CreateClosureExpr;
    class FuncDecl;
    class ExprStmt;
    class ReturnStmt;
    class UncondJump;
    class IfStmt;
    class WhileStmt;
    class SwitchStmt;
    class ClassDecl;
    class InstGet;
    class InstSet;
    class ScopeEdge;

    class CFGVisitor{
    public:
        virtual ~CFGVisitor() = default;

        virtual void visitVarDecl(VarDecl* decl) = 0;
        virtual void visitVarRead(VarRead* expr) = 0;
        virtual void visitVarStore(VarStore* expr) = 0;
        virtual void visitVarReadNative(VarReadNative* expr) = 0;
        virtual void visitArithmeticExpr(ArithmeticExpr* expr) = 0;
        virtual void visitComparisonExpr(ComparisonExpr* expr) = 0;
        virtual void visitInstanceofExpr(InstanceofExpr* expr) = 0;
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
        virtual void visitSpawnStmt(SpawnStmt* stmt) = 0;
        virtual void visitCreateClosureExpr(CreateClosureExpr* expr) = 0;
        virtual void visitFuncDecl(FuncDecl* decl) = 0;
        virtual void visitExprStmt(ExprStmt* stmt) = 0;
        virtual void visitReturnStmt(ReturnStmt* stmt) = 0;
        virtual void visitUncondJump(UncondJump* stmt) = 0;
        virtual void visitIfStmt(IfStmt* stmt) = 0;
        virtual void visitWhileStmt(WhileStmt* stmt) = 0;
        virtual void visitSwitchStmt(SwitchStmt* stmt) = 0;
        virtual void visitClassDecl(ClassDecl* decl) = 0;
        virtual void visitInstGet(InstGet* expr) = 0;
        virtual void visitInstSet(InstSet* expr) = 0;
        virtual void visitScopeBlock(ScopeEdge* stmt) = 0;
    };

    class CFGCodeGen{
    public:
        virtual ~CFGCodeGen() = default;
        virtual llvm::Value* visitVarDecl(VarDecl* decl) = 0;
        virtual llvm::Value* visitVarRead(VarRead* expr) = 0;
        virtual llvm::Value* visitVarStore(VarStore* expr) = 0;
        virtual llvm::Value* visitVarReadNative(VarReadNative* expr) = 0;
        virtual llvm::Value* visitArithmeticExpr(ArithmeticExpr* expr) = 0;
        virtual llvm::Value* visitComparisonExpr(ComparisonExpr* expr) = 0;
        virtual llvm::Value* visitInstanceofExpr(InstanceofExpr* expr) = 0;
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
        virtual llvm::Value* visitSpawnStmt(SpawnStmt* stmt) = 0;
        virtual llvm::Value* visitCreateClosureExpr(CreateClosureExpr* expr) = 0;
        virtual llvm::Value* visitFuncDecl(FuncDecl* decl) = 0;
        virtual llvm::Value* visitExprStmt(ExprStmt* stmt) = 0;
        virtual llvm::Value* visitReturnStmt(ReturnStmt* stmt) = 0;
        virtual llvm::Value* visitUncondJump(UncondJump* stmt) = 0;
        virtual llvm::Value* visitIfStmt(IfStmt* stmt) = 0;
        virtual llvm::Value* visitWhileStmt(WhileStmt* stmt) = 0;
        virtual llvm::Value* visitSwitchStmt(SwitchStmt* stmt) = 0;
        virtual llvm::Value* visitClassDecl(ClassDecl* decl) = 0;
        virtual llvm::Value* visitInstGet(InstGet* stmt) = 0;
        virtual llvm::Value* visitInstSet(InstSet* stmt) = 0;
        virtual llvm::Value* visitScopeBlock(ScopeEdge* stmt) = 0;
    };

    class CFGNode {
    public:
        NodeType type;

        virtual ~CFGNode() {};
        virtual void accept(CFGVisitor* vis) = 0;
        virtual llvm::Value* codegen(CFGCodeGen* vis) = 0;
    };
    using nodePtr = shared_ptr<CFGNode>;

    class TypedExpr : public CFGNode{
    public:
        types::tyPtr exprType;

        virtual ~TypedExpr() {};
        virtual void accept(CFGVisitor* vis) = 0;
        virtual llvm::Value* codegen(CFGCodeGen* vis) = 0;
    };
    using exprPtr = shared_ptr<TypedExpr>;

    // Only keep track of antecedents to build a reverse CFG
    class CFGStmt : public CFGNode {
    public:
        std::vector<std::shared_ptr<CFGNode>> antecedents;
    };

    using stmtPtr = shared_ptr<CFGStmt>;

    enum class VarType{
        LOCAL,
        FREEVAR,
        GLOBAL,
        // To optimize calling functions/instantiating classes
        GLOBAL_FUNC
    };
    class VarDecl : public CFGStmt{
    public:
        VarType varType;
        AST::VarDeclDebugInfo dbgInfo;
        uInt64 uuid;
        static inline uInt64 instanceCount = 0;

        VarDecl(VarType _varType){
            varType = _varType;
            uuid = instanceCount++;
            type = NodeType::VAR_DECL;
        }
        ~VarDecl() {};
        void accept(CFGVisitor* vis) override{
            vis->visitVarDecl(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitVarDecl(this);
        }
    };
    class VarRead : public TypedExpr{
    public:
        shared_ptr<VarDecl> varPtr;
        AST::VarReadDebugInfo dbgInfo;

        VarRead(shared_ptr<VarDecl> _varPtr, AST::VarReadDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            varPtr = _varPtr;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::VAR_READ;
        }
        ~VarRead() {};
        void accept(CFGVisitor* vis) override{
            vis->visitVarRead(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitVarRead(this);
        }
    };
    class VarStore : public TypedExpr{
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
        void accept(CFGVisitor* vis) override{
            vis->visitVarStore(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitVarStore(this);
        }
    };
    class VarReadNative : public TypedExpr{
    public:
        string nativeName;
        AST::VarReadDebugInfo dbgInfo;

        VarReadNative(string name, types::tyPtr _heldType, AST::VarReadDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            nativeName = name;
            exprType = _heldType;
            type = NodeType::VAR_NATIVE_READ;
        }
        ~VarReadNative() {};
        void accept(CFGVisitor* vis) override{
            vis->visitVarReadNative(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
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
    class ArithmeticExpr : public TypedExpr{
    public:
        ArithmeticOp opType;
        exprPtr lhs;
        exprPtr rhs;
        AST::BinaryExprDebugInfo dbgInfo;

        ArithmeticExpr(exprPtr _lhs, exprPtr _rhs, ArithmeticOp _op, AST::BinaryExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            lhs = _lhs;
            rhs = _rhs;
            opType = _op;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::ARITHEMTIC;
        }
        ~ArithmeticExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitArithmeticExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
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
        OR
    };
    class ComparisonExpr : public TypedExpr{
    public:
        ComparisonOp opType;
        exprPtr lhs;
        exprPtr rhs;
        AST::BinaryExprDebugInfo dbgInfo;

        ComparisonExpr(exprPtr _lhs, exprPtr _rhs, ComparisonOp _op, AST::BinaryExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            lhs = _lhs;
            rhs = _rhs;
            opType = _op;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::COMPARISON;
        }
        ~ComparisonExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitComparisonExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitComparisonExpr(this);
        }
    };

    class InstanceofExpr : public TypedExpr{
    public:
        exprPtr lhs;
        string className;
        std::shared_ptr<types::ClassType> classType;
        AST::BinaryExprDebugInfo dbgInfo;

        InstanceofExpr(exprPtr _lhs, string _className, std::shared_ptr<types::ClassType> _classType,
            AST::BinaryExprDebugInfo _dbgInfo): dbgInfo(_dbgInfo) {
            classType = _classType;
            lhs = _lhs;
            className = _className;
            exprType = types::getBasicType(types::TypeFlag::BOOL);
            type = NodeType::INSTANCEOF;
        }
        ~InstanceofExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitInstanceofExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitInstanceofExpr(this);
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
    class UnaryExpr : public TypedExpr{
    public:
        UnaryOp opType;
        exprPtr rhs;
        AST::UnaryExprDebugInfo dbgInfo;

        UnaryExpr(exprPtr _rhs, UnaryOp _op, AST::UnaryExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            rhs = _rhs;
            opType = _op;
            type = NodeType::UNARY;
            exprType = types::getBasicType(types::TypeFlag::ANY);
        }
        ~UnaryExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitUnaryExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitUnaryExpr(this);
        }
    };

    class LiteralExpr : public TypedExpr{
    public:
        // int is used to signal a nil value
        std::variant<double, bool, void*, string> val;
        AST::LiteralDebugInfo dbgInfo;

        LiteralExpr(std::variant<double, bool, void*, string> variant, AST::LiteralDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            val = variant;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::LITERAL;
        }
        ~LiteralExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitLiteralExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitLiteralExpr(this);
        }
    };

    class HashmapExpr : public TypedExpr{
    public:
        // Fields sorted in the order they appear in, important to eval them in that order
        vector<std::pair<string, exprPtr>> fields;
        AST::StructLiteralDebugInfo dbgInfo;

        HashmapExpr(vector<std::pair<string, exprPtr>> _fields, AST::StructLiteralDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            fields = _fields;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::HASHMAP;
        }
        ~HashmapExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitHashmapExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitHashmapExpr(this);
        }
    };
    class ArrayExpr : public TypedExpr{
    public:
        vector<exprPtr> fields;
        AST::ArrayLiteralDebugInfo dbgInfo;

        ArrayExpr(vector<exprPtr> _fields, AST::ArrayLiteralDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            fields = _fields;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::ARRAY;
        }
        ~ArrayExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitArrayExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitArrayExpr(this);
        }
    };

    class CollectionGet : public TypedExpr{
    public:
        exprPtr collection;
        exprPtr field;
        AST::CollectionAccessDebugInfo dbgInfo;

        CollectionGet(exprPtr _collection, exprPtr _field, AST::CollectionAccessDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            collection = _collection;
            field = _field;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::COLLECTION_GET;
        }
        ~CollectionGet() {};
        void accept(CFGVisitor* vis) override{
            vis->visitCollectionGet(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
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
    class CollectionSet : public TypedExpr{
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
        void accept(CFGVisitor* vis) override{
            vis->visitCollectionSet(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitCollectionSet(this);
        }
    };

    class ConditionalExpr : public TypedExpr{
    public:
        exprPtr cond;
        exprPtr thenExpr;
        exprPtr elseExpr;
        AST::ConditionalExprDebugInfo dbgInfo;

        ConditionalExpr(exprPtr _cond, exprPtr _thenExpr, exprPtr _elseExpr, AST::ConditionalExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            cond = _cond;
            thenExpr = _thenExpr;
            elseExpr = _elseExpr;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::CONDITIONAL;
        }
        ~ConditionalExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitConditionalExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitConditionalExpr(this);
        }
    };

    class CallExpr : public TypedExpr{
    public:
        exprPtr callee;
        vector<exprPtr> args;
        AST::CallExprDebugInfo dbgInfo;

        CallExpr(exprPtr _callee, vector<exprPtr> _args, AST::CallExprDebugInfo _dbgInfo)
        : dbgInfo(_dbgInfo){
            callee = _callee;
            args = _args;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::CALL;
        }
        ~CallExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitCallExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitCallExpr(this);
        }
    };
    class InvokeExpr : public TypedExpr{
    public:
        exprPtr inst;
        vector<exprPtr> args;
        string field;
        AST::InvokeExprDebugInfo dbgInfo;

        InvokeExpr(exprPtr _inst, string _field, vector<exprPtr> _args, AST::InvokeExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            inst = _inst;
            field = _field;
            args = _args;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::INVOKE;
        }
        ~InvokeExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitInvokeExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitInvokeExpr(this);
        }
    };
    class NewExpr : public TypedExpr{
    public:
        string className;
        vector<exprPtr> args;
        AST::NewExprDebugInfo dbgInfo;

        NewExpr(string _className, vector<exprPtr> _args, AST::NewExprDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            className = _className;
            args = _args;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::NEW;
        }
        ~NewExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitNewExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitNewExpr(this);
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

    class CreateClosureExpr : public TypedExpr{
    public:
        std::shared_ptr<Function> fn;
        // First ptr is pointer to the VarDecl from an outer function to store to the closure(can be local/freevar),
        // second is to the VarDecl used inside this function
        vector<std::pair<shared_ptr<VarDecl>, shared_ptr<VarDecl>>> freevars;
        AST::FuncLiteralDebugInfo dbgInfo;

        CreateClosureExpr(std::shared_ptr<Function> _fn, vector<std::pair<shared_ptr<VarDecl>, shared_ptr<VarDecl>>> _freevars,
                          AST::FuncLiteralDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            fn = _fn;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            freevars = _freevars;
            type = NodeType::CLOSURE;
        }
        ~CreateClosureExpr() {};
        void accept(CFGVisitor* vis) override{
            vis->visitCreateClosureExpr(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitCreateClosureExpr(this);
        }
    };
    class FuncDecl : public CFGNode{
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
        void accept(CFGVisitor* vis) override{
            vis->visitFuncDecl(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitFuncDecl(this);
        }
    };

    class ExprStmt : public CFGStmt {
    public:
        exprPtr expr;

        ExprStmt(exprPtr _expr) : expr(_expr) {}
        void accept(CFGVisitor* vis) override{
            vis->visitExprStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitExprStmt(this);
        }
    };

    class SpawnStmt : public CFGStmt{
    public:
        bool isInvoke;
        exprPtr call;
        AST::SpawnStmtDebugInfo dbgInfo;

        SpawnStmt(exprPtr _call, bool _isInvoke, AST::SpawnStmtDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            call = _call;
            isInvoke = _isInvoke;
            type = NodeType::SPAWN;
        }
        ~SpawnStmt() {};
        void accept(CFGVisitor* vis){
            vis->visitSpawnStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) {
            return vis->visitSpawnStmt(this);
        }
    };

    class ReturnStmt : public CFGStmt{
    public:
        exprPtr expr;
        AST::ReturnStmtDebugInfo dbgInfo;

        ReturnStmt(exprPtr _expr, AST::ReturnStmtDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            expr = _expr;
            type = NodeType::RETURN;
        }
        ~ReturnStmt() {};
        void accept(CFGVisitor* vis) override{
            vis->visitReturnStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitReturnStmt(this);
        }
    };

    enum class JumpType{
        BREAK,
        CONTINUE,
        ADVANCE
    };
    class UncondJump : public CFGNode{
    public:
        JumpType jmpType;
        AST::UncondJmpDebugInfo dbgInfo;

        UncondJump(JumpType _jmpType, AST::UncondJmpDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            jmpType = _jmpType;
            type = NodeType::UNCOND_JMP;
        }
        ~UncondJump() {};
        void accept(CFGVisitor* vis) override{
            vis->visitUncondJump(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitUncondJump(this);
        }
    };

    class IfStmt : public CFGStmt{
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
        void accept(CFGVisitor* vis) override{
            vis->visitIfStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitIfStmt(this);
        }
    };
    class WhileStmt : public CFGStmt{
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
        void accept(CFGVisitor* vis) override{
            vis->visitWhileStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitWhileStmt(this);
        }
    };

    class SwitchStmt : public CFGStmt{
    public:
        exprPtr cond;
        // Each constant has a literal and the index of the case it leads to
        // No constant leads to defaultCase
        vector<std::pair<std::variant<double, bool, void*, string>, int>> constants;
        int defaultCaseBlockNum;
        vector<Block> cases;
        AST::SwitchStmtDebugInfo dbgInfo;
        bool containsStrings;

        SwitchStmt(exprPtr _cond, vector<std::pair<std::variant<double, bool, void*, string>, int>> _constants,
                   vector<Block> _cases, int _defaultCaseBlockNum, bool _containsStrings, AST::SwitchStmtDebugInfo _dbgInfo)
            : dbgInfo(_dbgInfo){
            cond = _cond;
            constants = _constants;
            cases = _cases;
            defaultCaseBlockNum = _defaultCaseBlockNum;
            containsStrings = _containsStrings;
            type = NodeType::SWITCH;
        }
        ~SwitchStmt() {};
        void accept(CFGVisitor* vis) override{
            vis->visitSwitchStmt(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
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

    class ClassDecl : public CFGNode{
    public:
        // Privates are prefixed with "priv."
        // Int is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, int> fields;
        std::unordered_map<string, std::pair<ClassMethod, int>> methods;
        std::shared_ptr<types::ClassType> classType;
        string parentClassName; // Used for chaining classes to implement 'instanceof' operator
        // To store the class object to a variable
        string fullName;

        // Name is contained in dbgInfo
        AST::ClassDeclDebugInfo dbgInfo;

        ClassDecl(std::shared_ptr<types::ClassType> ty, AST::ClassDeclDebugInfo _dbgInfo, string _fullName,
                  string _parentClassName, std::unordered_map<string, int>& _fields,
                  std::unordered_map<string, std::pair<ClassMethod, int>>& _methods) : dbgInfo(_dbgInfo){
            classType = ty;
            parentClassName = _parentClassName;
            fields = _fields;
            methods = _methods;
            fullName = _fullName;
            type = NodeType::CLASS_DECL;
        }
        ~ClassDecl() {};
        void accept(CFGVisitor* vis) override{
            vis->visitClassDecl(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitClassDecl(this);
        }
        void inherit(std::shared_ptr<ClassDecl> parent){
            methods = parent->methods;
            fields = parent->fields;
        }
    };
    class InstGet : public TypedExpr{
    public:
        exprPtr instance;
        string field;
        AST::InstGetDebugInfo dbgInfo;

        InstGet(exprPtr _instance, string _field, AST::InstGetDebugInfo _dbgInfo) : dbgInfo(_dbgInfo){
            instance = _instance;
            field = _field;
            exprType = types::getBasicType(types::TypeFlag::ANY);
            type = NodeType::INST_GET;
        }
        ~InstGet() {};
        void accept(CFGVisitor* vis) override{
            vis->visitInstGet(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitInstGet(this);
        }
    };
    class InstSet : public TypedExpr{
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
        void accept(CFGVisitor* vis) override{
            vis->visitInstSet(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitInstSet(this);
        }

    };

    // Misc
    enum class ScopeEdgeType{
        START,
        END
    };
    // Used for better debug info and to know which variables to remove when exiting scope and for generating runtime debug info
    class ScopeEdge : public CFGNode{
    public:
        ScopeEdgeType edgeType;
        Token location;
        // Variables to pop from the variable map
        vector<uInt64> toPop;
        ScopeEdge(ScopeEdgeType _ty, Token& _location, vector<uInt64> _toPop){
            edgeType = _ty;
            location = _location;
            toPop = _toPop;
            type = NodeType::BLOCK_EDGE;
        }
        ~ScopeEdge() {}
        void accept(CFGVisitor* vis) override{
            vis->visitScopeBlock(this);
        }
        llvm::Value* codegen(CFGCodeGen* vis) override{
            return vis->visitScopeBlock(this);
        }
    };
}