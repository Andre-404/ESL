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

namespace compileCore {

    using typedExprPtr = std::shared_ptr<typedAST::TypedASTExpr>;

class Compiler : public typedAST::TypedASTCodegen {
	public:
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(std::shared_ptr<typedAST::Function> _code, vector<File*>& _srcFiles, vector<vector<types::tyPtr>>& _tyEnv);
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
        llvm::Value* visitInstSuperGet(typedAST::InstSuperGet* expr) override;
        llvm::Value* visitInstSet(typedAST::InstSet* expr) override;
        llvm::Value* visitScopeBlock(typedAST::ScopeEdge* stmt) override;
		#pragma endregion 
	private:
        // Collapsed type environment, "exprType" inside of expressions is an index into a list of types in the environment
        vector<vector<types::tyPtr>> typeEnv;

        // Integers are uuids of VarDecl instances
        ankerl::unordered_dense::map<uInt64, llvm::Value*> variables;

        ankerl::unordered_dense::map<std::shared_ptr<types::ClassType>, std::shared_ptr<typedAST::ClassDecl>> classes;
        // Connects function types(unique for each function) and the LLVM IR representation of that function
        ankerl::unordered_dense::map<types::tyPtr, llvm::Function*> functions;
        ankerl::unordered_dense::map<string, llvm::Function*> nativeFunctions;
        ankerl::unordered_dense::map<string, llvm::Constant*> stringConstants;
        ankerl::unordered_dense::map<string, llvm::Type*> namedTypes;


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT;
        llvm::Function* currentFunction;

        llvm::BasicBlock* continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        llvm::BasicBlock* breakJumpDest;
        llvm::BasicBlock* advanceJumpDest;

        void createRuntimeErrCall(string fmtErr, std::vector<llvm::Value*> args, int exitCode);
        void createTyErr(const string err, llvm::Value* const val, const Token token);
        void createTyErr(const string err, llvm::Value* const lhs, llvm::Value* const rhs, const Token token);
        llvm::Constant* createConstStr(const string& str);
        llvm::Value* castToVal(llvm::Value* val);

        #pragma region Helpers
        bool exprConstrainedToType(const typedExprPtr expr, const types::tyPtr ty);

        bool exprConstrainedToType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);

        bool exprWithoutType(const typedExprPtr expr, const types::tyPtr ty);

        bool exprWithoutType(const typedExprPtr expr1, const typedExprPtr expr2, const types::tyPtr ty);

        int getClassFieldIndex(const types::tyVarIdx exprTyIdx, const types::tyPtr ty);

        llvm::Value* codegenBinaryAdd(const typedExprPtr lhs, const typedExprPtr rhs, const Token op);

        llvm::Value* codegenLogicOps(const typedExprPtr lhs, const typedExprPtr rhs, const typedAST::ComparisonOp op);

        llvm::Value* codegenCmp(const typedExprPtr expr1, const typedExprPtr expr2, const bool neg);

        llvm::Value* codegenNeg(const typedExprPtr expr1, const typedAST::UnaryOp op, const Token dbg);

        void codegenBlock(const typedAST::Block& block);
        // Misc
        void error(const Token token, const string& msg) noexcept(false);
        void error(const string& message) noexcept(false);

        llvm::Function* createNewFunc(const int argCount, const string name, const std::shared_ptr<types::FunctionType> fnTy);

        std::shared_ptr<types::FunctionType> getFuncFromType(const types::tyVarIdx ty);
        #pragma endregion
	};
}