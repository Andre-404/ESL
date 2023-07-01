#pragma once
#include "codegenDefs.h"
#include "../Objects/objects.h"
#include "../Parsing/ASTDefs.h"
#include "../Parsing/parser.h"
#include "upvalueFinder.h"
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

	enum class FuncType {
		TYPE_FUNC,
		TYPE_METHOD,
		TYPE_CONSTRUCTOR,
		TYPE_SCRIPT,
	};

	struct Local {
		string name;
		int depth;
        llvm::Value* val;// Value that this local holds
        // Whether this local variable has been captured as an upvalue and should be accessed through ObjUpval
        bool isUpval;

        Local(string _name, int _depth, bool _isUpval) : name(_name), depth(_depth), isUpval(_isUpval) {}
        Local(){
            name = "";
            depth = -1;
            val = nullptr;
            isUpval = false;
        }
	};

    // Represents an Upvalue held in ObjClosure, at function entry all
	struct Upvalue {
		string name = "";
        llvm::Value* val = nullptr;

        Upvalue(string _name, llvm::Value* _val) : name(_name), val(_val) {}
	};


	// Information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
	struct CurrentChunkInfo {
		// For closures
		CurrentChunkInfo* enclosing;
        llvm::Function* func;
        FuncType type;
		bool hasReturnStmt;

		int line;
        int scopeDepth;
		// Stack can grow an arbitrary amount
        vector<Local> locals;
        vector<Upvalue> upvalues;
		bool hasCapturedLocals;
		CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type, llvm::Function* _func);
	};

	struct ClassChunkInfo {
    // For statics
    ankerl::unordered_dense::set<string> fields;
    ClassChunkInfo* parent;
    ClassChunkInfo(ClassChunkInfo* _parent) : parent(_parent) {};
	};

	struct CompilerException {

	};

    struct Globalvar {
        llvm::Value* val;
        bool isDefined;
        // If type is FUNC or CLASS then it cannot be assigned to
        AST::ASTDeclType type;

        Globalvar(AST::ASTDeclType _type, llvm::Value* _val) {
            val = _val;
            isDefined = false;
            type = _type;
        }
    };


    class Compiler : public AST::Visitor {
	public:
		// Compiler only ever emits the code for a single function, top level code is considered a function
		CurrentChunkInfo* current;
		std::unique_ptr<ClassChunkInfo> currentClass;
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;

		Compiler(vector<ESLModule*>& units);
        void compile();
		Chunk* getChunk();
        llvm::Value* endFuncDecl(int arity, string name);

		#pragma region Visitor pattern
		void visitAssignmentExpr(AST::AssignmentExpr* expr) override;
		void visitSetExpr(AST::SetExpr* expr) override;
		void visitConditionalExpr(AST::ConditionalExpr* expr) override;
        void visitRangeExpr(AST::RangeExpr* expr) override;
		void visitBinaryExpr(AST::BinaryExpr* expr) override;
		void visitUnaryExpr(AST::UnaryExpr* expr) override;
		void visitCallExpr(AST::CallExpr* expr) override;
        void visitNewExpr(AST::NewExpr* expr) override;
		void visitFieldAccessExpr(AST::FieldAccessExpr* expr) override;
		void visitAsyncExpr(AST::AsyncExpr* expr) override;
		void visitAwaitExpr(AST::AwaitExpr* expr) override;
		void visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr) override;
		void visitStructLiteralExpr(AST::StructLiteral* expr) override;
		void visitLiteralExpr(AST::LiteralExpr* expr) override;
		void visitSuperExpr(AST::SuperExpr* expr) override;
		void visitFuncLiteral(AST::FuncLiteral* expr) override;
		void visitModuleAccessExpr(AST::ModuleAccessExpr* expr) override;
        void visitMacroExpr(AST::MacroExpr* expr) override;

		void visitVarDecl(AST::VarDecl* decl) override;
		void visitFuncDecl(AST::FuncDecl* decl) override;
		void visitClassDecl(AST::ClassDecl* decl) override;

		void visitExprStmt(AST::ExprStmt* stmt) override;
		void visitBlockStmt(AST::BlockStmt* stmt) override;
		void visitIfStmt(AST::IfStmt* stmt) override;
		void visitWhileStmt(AST::WhileStmt* stmt) override;
		void visitForStmt(AST::ForStmt* stmt) override;
		void visitBreakStmt(AST::BreakStmt* stmt) override;
		void visitContinueStmt(AST::ContinueStmt* stmt) override;
		void visitSwitchStmt(AST::SwitchStmt* stmt) override;
		void visitCaseStmt(AST::CaseStmt* _case) override;
		void visitAdvanceStmt(AST::AdvanceStmt* stmt) override;
		void visitReturnStmt(AST::ReturnStmt* stmt) override;
		#pragma endregion 
	private:
		ESLModule* curUnit;
		int curUnitIndex;
		vector<ESLModule*> units;
        std::unordered_map<AST::FuncLiteral*, vector<upvalueFinder::Upvalue>> upvalueMap;
        ankerl::unordered_dense::map<string, Globalvar> globals;
        ankerl::unordered_dense::map<string, ClassChunkInfo*> globalClasses;
        ankerl::unordered_dense::map<string, llvm::Value*> nativeFunctions;


        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        llvm::Value* returnValue;
        std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT;

        ankerl::unordered_dense::map<string, llvm::Constant*> stringConstants;
        ankerl::unordered_dense::map<string, llvm::Type*> namedTypes;
        vector<llvm::BasicBlock*> continueJumpDest;
        // Both loops and switch statement push to breakJump stack
        vector<llvm::BasicBlock*> breakJumpDest;
        vector<llvm::BasicBlock*> advanceJumpDest;
        // Flag to know if a break/continue/advance jump was performed in this basic block,
        // if this is true don't emit the final unconditional break in if/for/while/switch constructs
        bool BBTerminated;

        llvm::Value* evalASTExpr(std::shared_ptr<AST::ASTNode> node);
        void retVal(llvm::Value* val);
        void createRuntimeErrCall(string fmtErr, std::vector<llvm::Value*> args, int exitCode);
        void createTyErr(string err, llvm::Value* val);
        void createTyErr(string err, llvm::Value* lhs, llvm::Value* rhs);
        llvm::Constant* createConstStr(string str);
        llvm::Value* castToVal(llvm::Value* val);
        void createGcSafepoint();

        #pragma region Helpers
        void emitReturn();
        // Helper that's used in if/for/while/switch to terminate a basic block
        // checks BBTerminated to see if the final br instruction is needed
        void endControlFlowBB(llvm::BasicBlock* dest);

        string declareGlobalVar(Token name);
        void defineGlobalVar(string name, llvm::Value* val);

        llvm::Value* readVar(Token name);
        void storeToVar(Token name, llvm::Value* val);
        // Locals
        void declareLocalVar(AST::ASTVar& name);
        void defineLocalVar(llvm::Value* val);

        void addLocal(AST::ASTVar name);
        int resolveLocal(Token name);

        int resolveUpvalue(Token name);

        void beginScope();
        void endScope();
        // Classes and methods
        object::ObjClosure* method(AST::FuncDecl* _method, Token className);
        bool invoke(AST::CallExpr* expr);
        llvm::Value* resolveClassField(Token name, bool canAssign);
        ClassChunkInfo* getClassFromExpr(AST::ASTNodePtr expr);
        // Resolve public/private fields when this.object_field in encountered
        bool resolveThis(AST::FieldAccessExpr* expr);
        bool resolveThis(AST::SetExpr* expr);
        bool resolveImplicitObjectField(AST::CallExpr* expr);
        // Misc
        Token syntheticToken(string str);
        void updateLine(Token token);
        void error(Token token, const string& msg) noexcept(false);
        void error(const string& message) noexcept(false);
        // Checks all imports to see if the symbol 'token' is imported
        llvm::Value* checkSymbol(Token token);
        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        llvm::Value* resolveGlobal(Token token, bool canAssign);
        // Given a token for module alias and a token for variable name, returns correct symbol to use
        llvm::Value* resolveModuleVariable(Token moduleAlias, Token variable);

        void createNewFunc(int argCount, string name, FuncType type);
        #pragma endregion
	};
}