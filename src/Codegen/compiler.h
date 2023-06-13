#pragma once
#include "codegenDefs.h"
#include "../Objects/objects.h"
#include "../Parsing/ASTDefs.h"
#include "../Parsing/parser.h"
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
		string name = "";
		int depth = -1;
		bool isLocalUpvalue = false;//whether this local variable has been captured as an upvalue
	};

	struct Upvalue {
		uint8_t index = 0;
		bool isLocal = false;
	};
	enum class ScopeJumpType {
		BREAK,
		CONTINUE,
		ADVANCE
	};
	//conversion from enum to 1 byte number
	inline constexpr unsigned operator+ (ScopeJumpType const val) { return static_cast<byte>(val); }


	//information about the parserCurrent code chunk we're compiling, contains a reference to the enclosing code chunk which created this one
	struct CurrentChunkInfo {
		//for closures
		CurrentChunkInfo* enclosing;
		//function whose information is contained within this chunk info
		object::ObjFunc* func;
		Chunk chunk;
		FuncType type;
		bool hasReturnStmt;

		uInt line;
		//information about unpatched 'continue' and 'break' statements
		vector<uInt> scopeJumps;
		//locals
		Local locals[LOCAL_MAX];
		uInt localCount;
		uInt scopeDepth;
		vector<int> scopeWithLoop;
		vector<int> scopeWithSwitch;
		std::array<Upvalue, UPVAL_MAX> upvalues;
		bool hasCapturedLocals;
		CurrentChunkInfo(CurrentChunkInfo* _enclosing, FuncType _type);
	};

	struct ClassChunkInfo {
    // For statics
    object::ObjClass* klass;
    ClassChunkInfo(object::ObjClass* _klass) : klass(_klass) {};
	};

	struct CompilerException {

	};

	class Compiler : public AST::Visitor {
	public:
		// Compiler only ever emits the code for a single function, top level code is considered a function
		CurrentChunkInfo* current;
		std::unique_ptr<ClassChunkInfo> currentClass;
		// Passed to the VM, used for highlighting runtime errors, managed by the VM
		vector<File*> sourceFiles;
		// Passed to the VM
		vector<Globalvar> globals;
		Chunk mainCodeBlock;
        object::ObjFunc* mainBlockFunc;
        // Here to do name checking at compile time
        vector<object::ObjNativeFunc*> nativeFuncs;
        //Base class which implements toString
        object::ObjClass* baseClass;

		Compiler(vector<ESLModule*>& units);
        void compile();
		Chunk* getChunk();
		object::ObjFunc* endFuncDecl();

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
		int curGlobalIndex;
		vector<ESLModule*> units;
        // Every slot corresponds to a global variable in globals at the same index, used by compiler to detect if
        // a undefined global variable is being used
        vector<bool> definedGlobals;
        ankerl::unordered_dense::map<string, uInt> nativeFuncNames;

        std::unique_ptr<llvm::LLVMContext> ctx;
        llvm::IRBuilder<> builder;
        std::unique_ptr<llvm::Module> curModule;
        llvm::Value* returnValue;
        std::unique_ptr<llvm::orc::KaleidoscopeJIT> JIT;

        llvm::Value* visitASTNode(AST::ASTNode* node);
        void retVal(llvm::Value* val);

        #pragma region Helpers
        // Emitters
        void emitByte(byte byte);
        void emitBytes(byte byte1, byte byte2);
        void emit16Bit(uInt16 number);
        void emitByteAnd16Bit(byte byte, uInt16 num);
        void emitConstant(Value value);
        void emitReturn();
        // Control flow
        int emitJump(byte jumpType);
        void patchJump(int offset);
        void emitLoop(int start);

        void patchScopeJumps(ScopeJumpType type);

        uInt16 makeConstant(Value value);
        // Variables
        uInt16 identifierConstant(Token name);

        uint16_t declareGlobalVar(Token name);
        void defineGlobalVar(uInt16 name);

        void namedVar(Token name, bool canAssign);
        // Locals
        void declareLocalVar(AST::ASTVar& name);
        void defineLocalVar();

        void addLocal(AST::ASTVar name);
        int resolveLocal(Token name);
        int resolveLocal(CurrentChunkInfo* func, Token name);

        int resolveUpvalue(CurrentChunkInfo* func, Token name);
        int addUpvalue(CurrentChunkInfo* func, byte index, bool isLocal);

        void beginScope();
        void endScope();
        // Classes and methods
        object::ObjClosure* method(AST::FuncDecl* _method, Token className);
        bool invoke(AST::CallExpr* expr);
        int resolveClassField(Token name, bool canAssign);
        object::ObjClass* getClassFromExpr(AST::ASTNodePtr expr);
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
        int checkSymbol(Token token);
        // Given a token and whether the operation is assigning or reading a variable, determines the correct symbol to use
        int resolveGlobal(Token token, bool canAssign);
        // Given a token for module alias and a token for variable name, returns correct symbol to use
        uInt resolveModuleVariable(Token moduleAlias, Token variable);
        #pragma endregion
	};
}