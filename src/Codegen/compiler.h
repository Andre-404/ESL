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

        Class(llvm::Constant* classPtr, llvm::GlobalVariable* instTemplatePtr, std::shared_ptr<types::ClassType> ty) :
            classPtr(classPtr), instTemplatePtr(instTemplatePtr), ty(ty) {}
        Class(){
            classPtr = nullptr;
            instTemplatePtr = nullptr;
            ty = nullptr;
        }
    };

class Compiler : public typedAST::TypedASTCodegen {
	public:
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(std::shared_ptr<typedAST::Function> _code, vector<File*>& _srcFiles, vector<types::tyPtr>& _tyEnv);
        void compile(std::shared_ptr<typedAST::Function> _code);

		#pragma region Visitor pattern
        llvm::Value* visitVarDecl(typedAST::VarDecl* decl) override;
        llvm::Value* visitVarRead(typedAST::VarRead* expr) override;
        llvm::Value* visitVarStore(typedAST::VarStore* expr) override;
        llvm::Value* visitVarReadNative(typedAST::VarReadNative* expr) override;
        llvm::Value* visitArithmeticExpr(typedAST::ArithmeticExpr* expr) override;
        llvm::Value* visitComparisonExpr(typedAST::ComparisonExpr* expr) override;
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
        llvm::Value* visitAsyncExpr(typedAST::AsyncExpr* expr) override;
        llvm::Value* visitAwaitExpr(typedAST::AwaitExpr* expr) override;
        llvm::Value* visitCreateClosureExpr(typedAST::CreateClosureExpr* expr) override;
        llvm::Value* visitRangeExpr(typedAST::RangeExpr* expr) override;
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
        // Connects function types(unique for each function) and the LLVM IR representation of that function
        fastMap<types::tyPtr, llvm::Function*> functions;
        fastMap<string, llvm::Function*> nativeFunctions;
        fastMap<string, llvm::Constant*> CStrings;
        fastMap<string, llvm::Constant*> ESLStrings;
        fastMap<string, llvm::Type*> namedTypes;


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT;
        llvm::Function* currentFunction;

        std::stack<llvm::BasicBlock*> continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        std::stack<llvm::BasicBlock*> breakJumpDest;
        std::stack<llvm::BasicBlock*> advanceJumpDest;

        #pragma region Helpers
        // Compile time type checking
        bool exprIsType(const typedExprPtr expr, const types::tyPtr ty);
        bool exprIsType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);
        bool exprIsComplexType(const typedExprPtr expr, const types::TypeFlag flag);

        // Runtime type checking
        void createTyErr(const string err, llvm::Value* const val, const Token token);
        void createTyErr(const string err, llvm::Value* const lhs, llvm::Value* const rhs, const Token token);
        void createRuntimeTypeCheck(llvm::Function* typeCheckFunc, llvm::Value* val, string executeBBName, string errMsg, Token dbg);
        void createRuntimeTypeCheck(llvm::Function* typeCheckFunc, llvm::Value* lhs, llvm::Value* rhs,
                                    string executeBBName, string errMsg, Token dbg);

        // Codegen functions(take in a typedAST expression and transform into LLVM IR)
        // Made to avoid monolithic functions that contain a bunch of builder calls
        llvm::Value* codegenBinaryAdd(llvm::Value* lhs, llvm::Value* rhs, const Token op);
        llvm::Value* codegenLogicOps(const typedExprPtr lhs, const typedExprPtr rhs, const typedAST::ComparisonOp op);
        llvm::Value* codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg);
        llvm::Value* codegenNeg(const typedExprPtr expr1, const typedAST::UnaryOp op, const Token dbg);
        void codegenBlock(const typedAST::Block& block);

        // Functions helpers
        llvm::Function* createNewFunc(const string name, const std::shared_ptr<types::FunctionType> fnTy);
        llvm::FunctionType* getFuncType(int argnum);
        void declareFuncArgs(const vector<std::shared_ptr<typedAST::VarDecl>>& args);
        // Tries to optimize a function call if possible, otherwise returns nullptr
        llvm::Value* optimizedFuncCall(const typedAST::CallExpr* expr);
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
        llvm::Constant* codegenMethod(typedAST::ClassMethod& method);
        llvm::GlobalVariable* createInstanceTemplate(llvm::Constant* klass, int fieldN);

        // Misc
        llvm::Constant* createConstStr(const string& str);
        llvm::Constant* createESLString(const string& str);
        llvm::Value* castToVal(llvm::Value* val);
        llvm::Function* safeGetFunc(const string& name);
        void argCntError(Token token, llvm::Value* expected, const int got);
        llvm::Constant* createConstant(std::variant<double, bool, void*,string>& constant);
        llvm::ilist_iterator<llvm::ilist_detail::node_options<llvm::Instruction, false, false, void>, false, false> getIP();
        llvm::GlobalVariable* storeConstObj(llvm::Constant* obj);
        llvm::Constant* createConstObjHeader(int type);
        llvm::Constant* constObjToVal(llvm::Constant* obj);
        void replaceGV(uInt64 uuid, llvm::Constant* newInit);
        #pragma endregion
	};
}