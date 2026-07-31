#pragma once
#include "Passes/AST/ASTToTypedAST.h"
#include "DebugEmitter.h"
#include "CompilerParts/ComptimeValues.h"
#include "CompilerParts/RuntimeTypecheck.h"
#include "CompilerParts/InstBuilder.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include <stack>

namespace errorHandler{
    class ErrorHandler;
}

namespace llvm{
    class Type;
    class Function;
}

template<class T, class K>
using fastMap = ankerl::unordered_dense::map<T, K>;

namespace compileCore {

    enum class CompileType{
        JIT,
        OBJECT_CODE
    };

    using typedExprPtr = std::shared_ptr<CFG::TypedExpr>;

    class CompilerError{
    public:
        string reason;
        CompilerError(string _reason) : reason(_reason) {}
    };

    class Class{
    public:
        llvm::Constant* classPtr;
        // Nulled [N x Value] that create_inst copies into a fresh instance. Just the fields: the
        // header and class pointer are the runtime's job now.
        llvm::GlobalVariable* fieldTemplatePtr;
        std::shared_ptr<types::ClassType> ty;
        vector<llvm::Constant*> methodArr;
        llvm::Constant* methodArrPtr;

        Class(llvm::Constant* classPtr, llvm::GlobalVariable* fieldTemplatePtr, std::shared_ptr<types::ClassType> ty,
              vector<llvm::Constant*> methodArr, llvm::Constant* methodArrPtr) :
            classPtr(classPtr), fieldTemplatePtr(fieldTemplatePtr), ty(ty), methodArr(methodArr), methodArrPtr(methodArrPtr) {}
        Class(){
            classPtr = nullptr;
            fieldTemplatePtr = nullptr;
            ty = nullptr;
            methodArrPtr = nullptr;
        }
    };

    class Function{
    public:
        llvm::Function* fn;
        Function(){
            fn = nullptr;
        }
        Function(llvm::Function* fn) : fn(fn){}
    };

class Compiler : public CFG::CFGCodeGen {
	public:
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(vector<File*>& _srcFiles, fastMap<string, std::pair<int, int>>& _classHierarchy,
                 fastMap<string, types::tyPtr>& natives, const llvm::DataLayout& DL, errorHandler::ErrorHandler& errHandler);
        llvm::orc::ThreadSafeModule compile(std::shared_ptr<CFG::Function> _code, string mainFnName);

		#pragma region Visitor pattern
        llvm::Value* visitVarDecl(CFG::VarDecl* decl) override;
        llvm::Value* visitVarRead(CFG::VarRead* expr) override;
        llvm::Value* visitVarStore(CFG::VarStore* expr) override;
        llvm::Value* visitVarReadNative(CFG::VarReadNative* expr) override;
        llvm::Value* visitArithmeticExpr(CFG::ArithmeticExpr* expr) override;
        llvm::Value* visitComparisonExpr(CFG::ComparisonExpr* expr) override;
        llvm::Value* visitInstanceofExpr(CFG::InstanceofExpr* expr) override;
        llvm::Value* visitUnaryExpr(CFG::UnaryExpr* expr) override;
        llvm::Value* visitLiteralExpr(CFG::LiteralExpr* expr) override;
        llvm::Value* visitHashmapExpr(CFG::HashmapExpr* expr) override;
        llvm::Value* visitArrayExpr(CFG::ArrayExpr* expr) override;
        llvm::Value* visitCollectionGet(CFG::CollectionGet* expr) override;
        llvm::Value* visitCollectionSet(CFG::CollectionSet* expr) override;
        llvm::Value* visitConditionalExpr(CFG::ConditionalExpr* expr) override;
        llvm::Value* visitCallExpr(CFG::CallExpr* expr) override;
        llvm::Value* visitInvokeExpr(CFG::InvokeExpr* expr) override;
        llvm::Value* visitNewExpr(CFG::NewExpr* expr) override;
        llvm::Value* visitSpawnStmt(CFG::SpawnStmt* stmt) override;
        llvm::Value* visitCreateClosureExpr(CFG::CreateClosureExpr* expr) override;
        llvm::Value* visitFuncDecl(CFG::FuncDecl* stmt) override;
        llvm::Value* visitExprStmt(CFG::ExprStmt* stmt) override;
        llvm::Value* visitReturnStmt(CFG::ReturnStmt* stmt) override;
        llvm::Value* visitUncondJump(CFG::UncondJump* stmt) override;
        llvm::Value* visitIfStmt(CFG::IfStmt* stmt) override;
        llvm::Value* visitWhileStmt(CFG::WhileStmt* stmt) override;
        llvm::Value* visitSwitchStmt(CFG::SwitchStmt* stmt) override;
        llvm::Value* visitClassDecl(CFG::ClassDecl* stmt) override;
        llvm::Value* visitInstGet(CFG::InstGet* expr) override;
        llvm::Value* visitInstSet(CFG::InstSet* expr) override;
        llvm::Value* visitScopeBlock(CFG::ScopeEdge* stmt) override;
		#pragma endregion 
	private:
        // Integers are uuids of VarDecl instances
        fastMap<uInt64, llvm::Value*> variables;

        // Maps uuids of classes to global variable holding the class struct
        fastMap<string, Class> classes;
        fastMap<string, llvm::Value*> nativeFunctions;
        DebugEmitter debugEmitter;
        errorHandler::ErrorHandler& errHandler;


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;

        std::stack<Function> inProgressFuncs;
        std::stack<llvm::BasicBlock*> continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        std::stack<llvm::BasicBlock*> breakJumpDest;
        std::stack<llvm::BasicBlock*> advanceJumpDest;

        // Must stay first: it creates the layouts and function declarations everything else uses
        runtime_bridge _bridge;
        TypeHelper _tyhelp;
        ComptimeValues _ct;
        RuntimeTypecheck _rt;
        InstBuilder _inst_builder;


        // LLVM stuff
        void setupModule(const llvm::DataLayout& DL);
        void optimizeModule(llvm::Module& module);
        // Declares both user and native functions
        llvm::Function* declareFunction(const std::shared_ptr<types::FunctionType> fnTy);
        void createMainEntrypoint(string entrypointName);
        void pastAllocas(std::function<void(llvm::IRBuilder<>&)> func);

        #pragma region Helpers
        // Compile time type checking
        bool exprIsType(const typedExprPtr expr, const types::tyPtr ty);
        bool exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);
        bool exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag);

        // Branching
        void create_if(llvm::Value* cond, std::function<void()> then, std::function<void()> _else);

        // Codegen functions(take in a CFG expression and transform into LLVM IR)
        // Made to avoid monolithic functions that contain a bunch of builder calls
        llvm::Value* codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, Token op);
        llvm::Value* codegenLogicOps(const typedExprPtr lhs, const typedExprPtr rhs, const CFG::ComparisonOp op);
        llvm::Value* codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg);
        llvm::Value* codegenNeg(llvm::Value* rhs, const types::tyPtr type, const CFG::UnaryOp op, const Token dbg);
        void codegenBlock(const CFG::Block& block);
        llvm::Value *codegenIncrement(const CFG::UnaryOp op, const typedExprPtr expr, const Token dbg);
        llvm::Value *codegenVarIncrement(const CFG::UnaryOp op, const std::shared_ptr<CFG::VarRead> expr, Token dbg);
        llvm::Value *codegenInstIncrement(const CFG::UnaryOp op, const std::shared_ptr<CFG::InstGet> expr, Token dbg);
        llvm::Value* codegenVarRead(std::shared_ptr<CFG::VarDecl> varPtr);
        llvm::Value* codegenVarStore(std::shared_ptr<CFG::VarDecl> varPtr, llvm::Value* toStore);

        // Functions helpers
        llvm::Function* startFuncDef(const string &name, const std::shared_ptr<types::FunctionType> fnTy, Token& loc);
        void declareFuncArgs(const vector<std::shared_ptr<CFG::VarDecl>>& args);

        // Array optimization
        llvm::Value* decoupleSetOperation(llvm::Value* storedVal, llvm::Value* newVal, CFG::SetType opTy, Token dbg);
        llvm::Value* getArrElement(llvm::Value* arr, llvm::Value* index, bool opt, Token dbg);
        llvm::Value* getMapElement(llvm::Value* map, llvm::Value* field, bool opt, Token dbg);
        llvm::Value* setArrElement(llvm::Value* arr, llvm::Value* index, llvm::Value* val, bool optIdx, bool optVal, CFG::SetType opTy, Token dbg);
        llvm::Value* setMapElement(llvm::Value* map, llvm::Value* field, llvm::Value* val, bool optIdx, bool optVal, CFG::SetType opTy, Token dbg);
        // Switch stmt stuff
        llvm::ConstantInt* createSwitchConstantInt(std::variant<double, bool, void*, string>& constant);
        vector<llvm::BasicBlock*> createNCaseBlocks(int n);
        llvm::Value* createSeqCmp(llvm::Value* compVal, vector<std::pair<std::variant<double, bool, void*, string>, int>>& constants);

        // Class helpers
        llvm::Function* createStrToIdxFunc(std::shared_ptr<types::ClassType> classType, bool isMethod);
        void codegenMethod(string classname, CFG::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd);

        // Multithreading
        // Unfortunately right now we need to create a threadWrapper for each spawn statement
        llvm::Function* createThreadWrapper(llvm::FunctionType* funcType, int numArgs);
        void setupThreadCreation(llvm::FunctionCallee callee, vector<llvm::Value*>& args);

        // Misc
        llvm::Function* safeGetFunc(const string& name);
        void generateNativeFuncs(fastMap<string, types::tyPtr>& natives);

        #pragma endregion
	};
}