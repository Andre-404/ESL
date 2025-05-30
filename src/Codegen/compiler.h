#pragma once
#include "Passes/ASTToTypedAST.h"
#include "DebugEmitter.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include <array>
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

    using typedExprPtr = std::shared_ptr<typedAST::TypedASTExpr>;

    class CompilerError{
    public:
        string reason;
        CompilerError(string _reason) : reason(_reason) {}
    };

    class Class{
    public:
        llvm::Constant* classPtr;
        llvm::GlobalVariable* instTemplatePtr;
        std::shared_ptr<types::ClassType> ty;
        vector<llvm::Constant*> methodArr;
        llvm::Constant* methodArrPtr;

        Class(llvm::Constant* classPtr, llvm::GlobalVariable* instTemplatePtr, std::shared_ptr<types::ClassType> ty,
              vector<llvm::Constant*> methodArr, llvm::Constant* methodArrPtr) :
            classPtr(classPtr), instTemplatePtr(instTemplatePtr), ty(ty), methodArr(methodArr), methodArrPtr(methodArrPtr) {}
        Class(){
            classPtr = nullptr;
            instTemplatePtr = nullptr;
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

class Compiler : public typedAST::TypedASTCodegen {
	public:
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(vector<File*>& _srcFiles, vector<types::tyPtr>& _tyEnv,
                 fastMap<string, std::pair<int, int>>& _classHierarchy, fastMap<string, types::tyVarIdx>& natives, const llvm::DataLayout& DL,
                 errorHandler::ErrorHandler& errHandler);
        llvm::orc::ThreadSafeModule compile(std::shared_ptr<typedAST::Function> _code, string mainFnName);

		#pragma region Visitor pattern
        llvm::Value* visitVarDecl(typedAST::VarDecl* decl) override;
        llvm::Value* visitVarRead(typedAST::VarRead* expr) override;
        llvm::Value* visitVarStore(typedAST::VarStore* expr) override;
        llvm::Value* visitVarReadNative(typedAST::VarReadNative* expr) override;
        llvm::Value* visitArithmeticExpr(typedAST::ArithmeticExpr* expr) override;
        llvm::Value* visitComparisonExpr(typedAST::ComparisonExpr* expr) override;
        llvm::Value* visitInstanceofExpr(typedAST::InstanceofExpr* expr) override;
        llvm::Value* visitUnaryExpr(typedAST::UnaryExpr* expr) override;
        llvm::Value* visitLiteralExpr(typedAST::LiteralExpr* expr) override;
        llvm::Value* visitHashmapExpr(typedAST::HashmapExpr* expr) override;
        llvm::Value* visitArrayExpr(typedAST::ArrayExpr* expr) override;
        llvm::Value* visitCollectionGet(typedAST::CollectionGet* expr) override;
        llvm::Value* visitCollectionSet(typedAST::CollectionSet* expr) override;
        llvm::Value* visitConditionalExpr(typedAST::ConditionalExpr* expr) override;
        llvm::Value* visitCallExpr(typedAST::CallExpr* expr) override;
        llvm::Value* visitInvokeExpr(typedAST::InvokeExpr* expr) override;
        llvm::Value* visitNewExpr(typedAST::NewExpr* expr) override;
        llvm::Value* visitSpawnStmt(typedAST::SpawnStmt* stmt) override;
        llvm::Value* visitCreateClosureExpr(typedAST::CreateClosureExpr* expr) override;
        llvm::Value* visitFuncDecl(typedAST::FuncDecl* stmt) override;
        llvm::Value* visitReturnStmt(typedAST::ReturnStmt* stmt) override;
        llvm::Value* visitUncondJump(typedAST::UncondJump* stmt) override;
        llvm::Value* visitIfStmt(typedAST::IfStmt* stmt) override;
        llvm::Value* visitWhileStmt(typedAST::WhileStmt* stmt) override;
        llvm::Value* visitSwitchStmt(typedAST::SwitchStmt* stmt) override;
        llvm::Value* visitClassDecl(typedAST::ClassDecl* stmt) override;
        llvm::Value* visitInstGet(typedAST::InstGet* expr) override;
        llvm::Value* visitInstSet(typedAST::InstSet* expr) override;
        llvm::Value* visitScopeBlock(typedAST::ScopeEdge* stmt) override;
		#pragma endregion 
	private:
        // Collapsed type environment, "exprType" inside of expressions is an index into a list of types in the environment
        vector<types::tyPtr> typeEnv;

        // Integers are uuids of VarDecl instances
        fastMap<uInt64, llvm::Value*> variables;

        // Maps uuids of classes to global variable holding the class struct
        fastMap<string, Class> classes;
        fastMap<string, std::pair<int, int>> classHierarchy;
        // Connects function types(unique for each function) and the LLVM IR representation of that function
        fastMap<types::tyPtr, llvm::Function*> functions;
        fastMap<string, llvm::Value*> nativeFunctions;
        fastMap<string, llvm::Constant*> CStrings;
        fastMap<string, llvm::Constant*> ESLStrings;
        fastMap<string, llvm::Type*> namedTypes;
        DebugEmitter debugEmitter;
        errorHandler::ErrorHandler& errHandler;


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        vector<llvm::Attribute> ESLFuncAttrs;

        std::stack<Function> inProgressFuncs;
        std::stack<llvm::BasicBlock*> continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        std::stack<llvm::BasicBlock*> breakJumpDest;
        std::stack<llvm::BasicBlock*> advanceJumpDest;

        // LLVM stuff
        void setupModule(const llvm::DataLayout& DL);
        void optimizeModule(llvm::Module& module);
        // Declares both user and native functions
        void declareFunctions();
        void createMainEntrypoint(string entrypointName);

        #pragma region Helpers
        // Compile time type checking
        bool exprIsType(const typedExprPtr expr, const types::tyPtr ty);
        bool exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);
        bool exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag);

        // Runtime type checking
        void createTypeCheckUnary(const string err, llvm::Value* const val, std::tuple<uint64_t, uint64_t, bool> masks);
        void createTypeCheckBinary(const string err, llvm::Value* const lhs, llvm::Value* const rhs, std::tuple<uint64_t, uint64_t, bool> masks);
        void createArgCountCheck(const string err, llvm::Value* closure, uint8_t expectedArity);
        void createArrBoundsCheck(const string err, llvm::Value* arr, llvm::Value* index);
        void createInstNoField(const string err, const string field, llvm::Value* inst);
        void createInstClassCheck(const string err, llvm::Value* inst, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd);

        // Codegen functions(take in a typedAST expression and transform into LLVM IR)
        // Made to avoid monolithic functions that contain a bunch of builder calls
        llvm::Value* codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, Token op);
        llvm::Value* codegenLogicOps(const typedExprPtr lhs, const typedExprPtr rhs, const typedAST::ComparisonOp op);
        llvm::Value* codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg);
        llvm::Value* codegenNeg(llvm::Value* rhs, const types::tyVarIdx type, const typedAST::UnaryOp op, const Token dbg);
        void codegenBlock(const typedAST::Block& block);
        llvm::Value *codegenIncrement(const typedAST::UnaryOp op, const typedExprPtr expr, const Token dbg);
        llvm::Value *codegenVarIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::VarRead> expr, Token dbg);
        llvm::Value *codegenInstIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::InstGet> expr, Token dbg);
        llvm::Value* codegenVarRead(std::shared_ptr<typedAST::VarDecl> varPtr);
        llvm::Value* codegenVarStore(std::shared_ptr<typedAST::VarDecl> varPtr, llvm::Value* toStore);

        // Functions helpers
        llvm::Function* startFuncDef(const string name, const std::shared_ptr<types::FunctionType> fnTy, Token& loc);
        llvm::FunctionType* getFuncType(int argnum);
        void declareFuncArgs(const vector<std::shared_ptr<typedAST::VarDecl>>& args);
        llvm::FunctionCallee setupUnoptCall(llvm::Value* closureVal, int argc, Token dbg);
        void createRuntimeFuncArgCheck(llvm::Value* objClosurePtr, size_t argSize, Token dbg);
        llvm::FunctionCallee getBitcastFunc(llvm::Value* closurePtr, const int argc);

        // Array optimization
        llvm::Value* decoupleSetOperation(llvm::Value* storedVal, llvm::Value* newVal, typedAST::SetType opTy, Token dbg);
        llvm::Value* getArrElement(llvm::Value* arr, llvm::Value* index, bool opt, Token dbg);
        llvm::Value* getMapElement(llvm::Value* map, llvm::Value* field, bool opt, Token dbg);
        llvm::Value* setArrElement(llvm::Value* arr, llvm::Value* index, llvm::Value* val, bool optIdx, bool optVal, typedAST::SetType opTy, Token dbg);
        llvm::Value* setMapElement(llvm::Value* map, llvm::Value* field, llvm::Value* val, bool optIdx, bool optVal, typedAST::SetType opTy, Token dbg);
        // Switch stmt stuff
        llvm::ConstantInt* createSwitchConstantInt(std::variant<double, bool, void*, string>& constant);
        vector<llvm::BasicBlock*> createNCaseBlocks(int n);
        llvm::Value* createSeqCmp(llvm::Value* compVal, vector<std::pair<std::variant<double, bool, void*, string>, int>>& constants);

        // Class helpers
        llvm::Function* createStrToIdxFunc(std::shared_ptr<types::ClassType> classType, bool isMethod);
        void codegenMethod(string classname, typedAST::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd);
        llvm::Constant* createMethodObj(typedAST::ClassMethod& method, llvm::Function* methodPtr);

        llvm::GlobalVariable* createInstanceTemplate(llvm::Constant* klass, int fieldN);
        llvm::Value* optimizeInstGet(llvm::Value* inst, string field, Class& klass);
        llvm::Value* instGetUnoptimized(llvm::Value* inst, string fieldName, Token dbg);
        std::pair<llvm::Value*, llvm::Value*> instGetUnoptIdx(llvm::Value* klass, string field);
        std::pair<llvm::Value*, llvm::Value*> instGetClassPtr(llvm::Value* inst, Token dbg);
        llvm::Value* instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx);

        llvm::Value* getOptInstFieldPtr(llvm::Value* inst, Class& klass, string field);
        llvm::Value* getUnoptInstFieldPtr(llvm::Value* inst, string field, Token dbg);

        llvm::FunctionCallee optimizeInvoke(llvm::Value* inst, string field, Class& klass, vector<llvm::Value*>& args, Token dbg);
        llvm::Value* unoptimizedInvoke(llvm::Value* inst, string field, vector<llvm::Value*> args, Token dbg);

        // Multithreading
        // Unfortunately right now we need to create a threadWrapper for each spawn statement
        llvm::Function* createThreadWrapper(llvm::FunctionType* funcType, int numArgs);
        void setupThreadCreation(llvm::FunctionCallee callee, vector<llvm::Value*>& args);

        // Const objects
        llvm::Constant* createESLString(const string& str);
        llvm::Constant* createConstObjHeader(int type);
        llvm::Constant* constObjToVal(llvm::Constant* obj, uint8_t type);
        llvm::GlobalVariable* storeConstObj(llvm::Constant* obj);

        // ESL val casting
        llvm::Value* ESLValTo(llvm::Value* val, llvm::Type* ty);
        llvm::Constant* ESLConstTo(llvm::Constant* constant, llvm::Type* ty);
        llvm::Value* CastToESLVal(llvm::Value* val);
        llvm::Constant* ConstCastToESLVal(llvm::Constant* constant);
        llvm::Type* getESLValType();

        // Misc
        llvm::Constant* createConstStr(const string& str);
        llvm::Function* safeGetFunc(const string& name);
        void argCntError(Token token, llvm::Value* expected, const int got);
        llvm::Constant* createConstant(std::variant<double, bool, void*,string>& constant);
        void generateNativeFuncs(fastMap<string, types::tyVarIdx>& natives);
        void createWeightedSwitch(llvm::Value* cond, vector<std::pair<int, llvm::BasicBlock*>> cases, llvm::BasicBlock* defaultBB, vector<int> weights);;

        #pragma endregion
	};
}