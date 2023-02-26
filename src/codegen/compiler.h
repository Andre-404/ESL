#pragma once
#include "codegenDefs.h"
#include "../Objects/objects.h"
#include "../Parsing/ASTDefs.h"
#include "../Parsing/parser.h"
#include <array>

namespace compileCore {
	#define LOCAL_MAX 256
	#define UPVAL_MAX 256

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
		ClassChunkInfo* currentClass;
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

		Compiler(vector<CSLModule*>& units);
		Chunk* getChunk();
		object::ObjFunc* endFuncDecl();

		#pragma region Visitor pattern
		void visitAssignmentExpr(AST::AssignmentExpr* expr);
		void visitSetExpr(AST::SetExpr* expr);
		void visitConditionalExpr(AST::ConditionalExpr* expr);
		void visitBinaryExpr(AST::BinaryExpr* expr);
		void visitUnaryExpr(AST::UnaryExpr* expr);
		void visitCallExpr(AST::CallExpr* expr);
		void visitFieldAccessExpr(AST::FieldAccessExpr* expr);
		void visitAsyncExpr(AST::AsyncExpr* expr);
		void visitAwaitExpr(AST::AwaitExpr* expr);
		void visitArrayLiteralExpr(AST::ArrayLiteralExpr* expr);
		void visitStructLiteralExpr(AST::StructLiteral* expr);
		void visitLiteralExpr(AST::LiteralExpr* expr);
		void visitSuperExpr(AST::SuperExpr* expr);
		void visitFuncLiteral(AST::FuncLiteral* expr);
		void visitModuleAccessExpr(AST::ModuleAccessExpr* expr);
        void visitMacroExpr(AST::MacroExpr* expr);

		void visitVarDecl(AST::VarDecl* decl);
		void visitFuncDecl(AST::FuncDecl* decl);
		void visitClassDecl(AST::ClassDecl* decl);

		void visitExprStmt(AST::ExprStmt* stmt);
		void visitBlockStmt(AST::BlockStmt* stmt);
		void visitIfStmt(AST::IfStmt* stmt);
		void visitWhileStmt(AST::WhileStmt* stmt);
		void visitForStmt(AST::ForStmt* stmt);
		void visitBreakStmt(AST::BreakStmt* stmt);
		void visitContinueStmt(AST::ContinueStmt* stmt);
		void visitSwitchStmt(AST::SwitchStmt* stmt);
		void visitCaseStmt(AST::CaseStmt* _case);
		void visitAdvanceStmt(AST::AdvanceStmt* stmt);
		void visitReturnStmt(AST::ReturnStmt* stmt);
		#pragma endregion 
	private:
		CSLModule* curUnit;
		int curUnitIndex;
		int curGlobalIndex;
		vector<CSLModule*> units;
        // Every slot corresponds to a global variable in globals at the same index, used by compiler to detect if
        // a undefined global variable is being used
        vector<bool> definedGlobals;
        ankerl::unordered_dense::map<string, uInt> nativeFuncNames;

		#pragma region Helpers
		//emitters
		void emitByte(byte byte);
		void emitBytes(byte byte1, byte byte2);
		void emit16Bit(uInt16 number);
		void emitByteAnd16Bit(byte byte, uInt16 num);
		void emitConstant(Value value);
		void emitReturn();
		//control flow
		int emitJump(byte jumpType);
		void patchJump(int offset);
		void emitLoop(int start);

		void patchScopeJumps(ScopeJumpType type);

		uInt16 makeConstant(Value value);
		//variables
		uInt16 identifierConstant(Token name);

        uint16_t declareGlobalVar(Token name);
		void defineGlobalVar(uInt16 name);

		void namedVar(Token name, bool canAssign);
		//locals
		void declareLocalVar(AST::ASTVar& name);
        void defineLocalVar();

		void addLocal(AST::ASTVar name);
		int resolveLocal(Token name);
		int resolveLocal(CurrentChunkInfo* func, Token name);

		int resolveUpvalue(CurrentChunkInfo* func, Token name);
		int addUpvalue(CurrentChunkInfo* func, byte index, bool isLocal);

		void beginScope();
		void endScope();
		//classes and methods
		object::ObjClosure* method(AST::FuncDecl* _method, Token className);
		bool invoke(AST::CallExpr* expr);
        int resolveClassField(Token name, bool canAssign);
        object::ObjClass* getClassFromExpr(AST::ASTNodePtr expr);
        // Resolve public/private fields when this.object_field in encountered
        bool resolveThis(AST::FieldAccessExpr* expr);
        bool resolveThis(AST::SetExpr* expr);
        bool resolveImplicitObjectField(AST::CallExpr* expr);
		//misc
        Token syntheticToken(string str);
		void updateLine(Token token);
		void error(Token token, const string& msg) noexcept(false);
		void error(const string& message) noexcept(false);
		// Checks all imports to see if the symbol 'token' is imported
		int checkSymbol(Token token);
		// Given a symbol and whether the operation is assigning or reading a variable, determines the correct symbol to use
		int resolveGlobal(Token symbol, bool canAssign);
		// Given a token for module alias and a token for variable name, returns correct symbol to use
		uInt resolveModuleVariable(Token moduleAlias, Token variable);
		#pragma endregion
	};
}