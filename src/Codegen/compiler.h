#pragma once
#include "Passes/ASTToTypedAST.h"
#include "JIT.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <array>
#include <stack>

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
        llvm::Constant* methodArrPtr;

        Class(llvm::Constant* classPtr, llvm::GlobalVariable* instTemplatePtr, std::shared_ptr<types::ClassType> ty, llvm::Constant* methodArrPtr) :
            classPtr(classPtr), instTemplatePtr(instTemplatePtr), ty(ty), methodArrPtr(methodArrPtr) {}
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
        llvm::AllocaInst* frameStack;
        int stackSize;
        int maxStackSize;
        Function(){
            fn = nullptr;
            frameStack = nullptr;
            stackSize = 0;
            maxStackSize = 0;
        }
        Function(llvm::Function* fn) : fn(fn), frameStack(nullptr), stackSize(0), maxStackSize(0){}
    };

class Compiler : public typedAST::TypedASTCodegen {
	public:
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(CompileType compileFlag, std::shared_ptr<typedAST::Function> _code, vector<File*>& _srcFiles, vector<types::tyPtr>& _tyEnv,
                 fastMap<string, std::pair<int, int>>& _classHierarchy, fastMap<string, types::tyVarIdx>& natives);
        void compile(std::shared_ptr<typedAST::Function> _code, string mainFnName);

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


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT;
        std::unique_ptr<llvm::TargetMachine> targetMachine;

        std::stack<Function> inProgressFuncs;
        std::stack<llvm::BasicBlock*> continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        std::stack<llvm::BasicBlock*> breakJumpDest;
        std::stack<llvm::BasicBlock*> advanceJumpDest;

        // LLVM stuff
        void setupModule(CompileType compileFlag);

        #pragma region Helpers
        // Compile time type checking
        bool exprIsType(const typedExprPtr expr, const types::tyPtr ty);
        bool exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);
        bool exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag);

        // Runtime type checking
        void createTyErr(const string err, llvm::Value* const val, Token token);
        void createTyErr(const string err, llvm::Value* const lhs, llvm::Value* const rhs, Token token);
        void createRuntimeTypeCheck(llvm::Function* predicate, vector<llvm::Value*> val,
                string executeBBName, string errMsg, Token dbg);
        void createRuntimeTypeCheck(llvm::Function* predicate, llvm::Value* lhs, llvm::Value* rhs,
                                    string executeBBName, string errMsg, Token dbg);

        // Codegen functions(take in a typedAST expression and transform into LLVM IR)
        // Made to avoid monolithic functions that contain a bunch of builder calls
        llvm::Value* codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, Token op);
        llvm::Value* codegenLogicOps(const typedExprPtr lhs, const typedExprPtr rhs, const typedAST::ComparisonOp op);
        llvm::Value* codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg);
        llvm::Value* codegenNeg(const typedExprPtr expr1, const typedAST::UnaryOp op, const Token dbg);
        void codegenBlock(const typedAST::Block& block);
        llvm::Value *codegenIncrement(const typedAST::UnaryOp op, const typedExprPtr expr);
        llvm::Value *codegenVarIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::VarRead> expr);
        llvm::Value *codegenInstIncrement(const typedAST::UnaryOp op, const std::shared_ptr<typedAST::InstGet> expr);
        llvm::Value* codegenVarRead(std::shared_ptr<typedAST::VarDecl> varPtr);
        llvm::Value* codegenVarStore(std::shared_ptr<typedAST::VarDecl> varPtr, llvm::Value* toStore);

        // Functions helpers
        llvm::Function* createNewFunc(const string name, const std::shared_ptr<types::FunctionType> fnTy);
        llvm::FunctionType* getFuncType(int argnum);
        void declareFuncArgs(const vector<std::shared_ptr<typedAST::VarDecl>>& args);
        llvm::Value* createFuncCall(llvm::Value* closureVal, vector<llvm::Value*> args, Token dbg);
        void createRuntimeFuncArgCheck(llvm::Value* objClosurePtr, size_t argSize, Token dbg);
        std::pair<llvm::Value*, llvm::FunctionType*> getBitcastFunc(llvm::Value* closurePtr, const int argc);

        // Array optimization
        void createArrBoundsCheck(llvm::Value* arr, llvm::Value* index, string errMsg, Token dbg);
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
        llvm::Function* createFieldChooseFunc(string className, std::unordered_map<string, int>& fields);
        llvm::Function* createMethodChooseFunc(string className, std::unordered_map<string, std::pair<typedAST::ClassMethod, int>>& methods);
        llvm::Function* forwardDeclMethod(typedAST::ClassMethod& method);
        void codegenMethod(string classname, typedAST::ClassMethod& method, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd, llvm::Function* methodFn);
        llvm::Constant* createMethodObj(typedAST::ClassMethod& method, llvm::Function* methodPtr);

        llvm::GlobalVariable* createInstanceTemplate(llvm::Constant* klass, int fieldN);
        llvm::Value* optimizeInstGet(llvm::Value* inst, string field, Class& klass);
        llvm::Value* instGetUnoptimized(llvm::Value* inst, llvm::Value* fieldIdx, llvm::Value* methodIdx, llvm::Value* klass, string field);
        std::pair<llvm::Value*, llvm::Value*> instGetUnoptIdx(llvm::Value* klass, llvm::Constant* field);

        llvm::Value* getOptInstFieldPtr(llvm::Value* inst, Class& klass, string field);
        llvm::Value* getUnoptInstFieldPtr(llvm::Value* inst, string field, Token dbg);

        llvm::Value* optimizeInvoke(llvm::Value* inst, string field, Class& klass, vector<llvm::Value*>& args, Token dbg);
        llvm::Value* unoptimizedInvoke(llvm::Value* inst, llvm::Value* fieldIdx, llvm::Value* methodIdx, llvm::Value* klass,
                                       string field, vector<llvm::Value*> args, Token dbg);

        // Const objects
        llvm::Constant* createESLString(const string& str);
        llvm::Constant* createConstObjHeader(int type);
        llvm::Constant* constObjToVal(llvm::Constant* obj);
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
        void implementNativeFunctions(fastMap<string, types::tyVarIdx>& natives);


        #pragma endregion
	};
}