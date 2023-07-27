#pragma once
#pragma once
#include "../moduleDefs.h"

namespace AST {
	using std::shared_ptr;

	enum class ASTType {
		ASSIGNMENT,
		SET,
		CONDITIONAL,
		BINARY,
		UNARY,
		ARRAY_LITERAL,
		CALL,
        NEW,
		FIELD_ACCESS,
		ASYNC,
		AWAIT,
		STRUCT,
		LITERAL,
		SUPER,
		FUNC_LITERAL,
		MODULE_ACCESS,
        MACRO,
        RANGE,

		VAR,
		FUNC,
		CLASS,

		EXPR_STMT,
		BLOCK,
		IF,
		WHILE,
		FOR,
		BREAK,
		CONTINUE,
		SWITCH,
		CASE,
		ADVANCE,
		RETURN
	};
	class AssignmentExpr;
	class SetExpr;
	class ConditionalExpr;
	class BinaryExpr;
	class UnaryExpr;
	class ArrayLiteralExpr;
	class CallExpr;
    class NewExpr;
	class FieldAccessExpr;
	class AsyncExpr;
	class AwaitExpr;
	class StructLiteral;
	class LiteralExpr;
	class SuperExpr;
	class FuncLiteral;
	class ModuleAccessExpr;
    class MacroExpr;
    class RangeExpr;

	class VarDecl;
	class FuncDecl;
	class ClassDecl;

	class ExprStmt;
	class BlockStmt;
	class IfStmt;
	class WhileStmt;
	class ForStmt;
	class BreakStmt;
	class ContinueStmt;
	class SwitchStmt;
	class CaseStmt;
	class AdvanceStmt;
	class ReturnStmt;

	//visitor pattern
	class Visitor {
	public:
		virtual void visitAssignmentExpr(AssignmentExpr* expr) = 0;
		virtual void visitSetExpr(SetExpr* expr) = 0;
		virtual void visitConditionalExpr(ConditionalExpr* expr) = 0;
        virtual void visitRangeExpr(RangeExpr* expr) = 0;
		virtual void visitBinaryExpr(BinaryExpr* expr) = 0;
		virtual void visitUnaryExpr(UnaryExpr* expr) = 0;
		virtual void visitCallExpr(CallExpr* expr) = 0;
        virtual void visitNewExpr(NewExpr* expr) = 0;
		virtual void visitFieldAccessExpr(FieldAccessExpr* expr) = 0;
		virtual void visitAsyncExpr(AsyncExpr* expr) = 0;
		virtual void visitAwaitExpr(AwaitExpr* expr) = 0;
		virtual void visitArrayLiteralExpr(ArrayLiteralExpr* expr) = 0;
		virtual void visitStructLiteralExpr(StructLiteral* expr) = 0;
		virtual void visitLiteralExpr(LiteralExpr* expr) = 0;
		virtual void visitSuperExpr(SuperExpr* expr) = 0;
		virtual void visitFuncLiteral(FuncLiteral* expr) = 0;
		virtual void visitModuleAccessExpr(ModuleAccessExpr* expr) = 0;
        virtual void visitMacroExpr(MacroExpr* expr) = 0;

		virtual void visitVarDecl(VarDecl* decl) = 0;
		virtual void visitFuncDecl(FuncDecl* decl) = 0;
		virtual void visitClassDecl(ClassDecl* decl) = 0;

		virtual void visitExprStmt(ExprStmt* stmt) = 0;
		virtual void visitBlockStmt(BlockStmt* stmt) = 0;
		virtual void visitIfStmt(IfStmt* stmt) = 0;
		virtual void visitWhileStmt(WhileStmt* stmt) = 0;
		virtual void visitForStmt(ForStmt* stmt) = 0;
		virtual void visitBreakStmt(BreakStmt* stmt) = 0;
		virtual void visitContinueStmt(ContinueStmt* stmt) = 0;
		virtual void visitSwitchStmt(SwitchStmt* stmt) = 0;
		virtual void visitCaseStmt(CaseStmt* _case) = 0;
		virtual void visitAdvanceStmt(AdvanceStmt* stmt) = 0;
		virtual void visitReturnStmt(ReturnStmt* stmt) = 0;
	};

	class ASTNode {
	public:
		ASTType type;

		virtual ~ASTNode() {};
		virtual void accept(Visitor* vis) = 0;
	};
	using ASTNodePtr = shared_ptr<ASTNode>;

    enum class ASTDeclType{
        VAR,
        FUNCTION,
        CLASS
    };

	class ASTDecl : public ASTNode {
	public:
		virtual Token getName() = 0;
        virtual ASTDeclType getType() = 0;
	};

    enum class ASTVarType{
        LOCAL,
        UPVALUE,
        GLOBAL,
        NONE
    };
    struct ASTVar{
        Token name;
        ASTVarType type;

        ASTVar(){
            type = ASTVarType::NONE;
        }
        ASTVar(Token _name){
            name = _name;
            type = ASTVarType::NONE;
        }
    };

	#pragma region Expressions

	class AssignmentExpr : public ASTNode {
	public:
		Token name;
		ASTNodePtr value;

		AssignmentExpr(Token _name, ASTNodePtr _value) {
			name = _name;
			value = _value;
			type = ASTType::ASSIGNMENT;
		}
		void accept(Visitor* vis) {
			vis->visitAssignmentExpr(this);
		}
	};

	//used for assigning values to struct, class and array fields
	class SetExpr : public ASTNode {
	public:
		ASTNodePtr callee;
		ASTNodePtr field;
		Token accessor;
		ASTNodePtr value;

		SetExpr(ASTNodePtr _callee, ASTNodePtr _field, Token _accessor, ASTNodePtr _val) {
			callee = _callee;
			field = _field;
			accessor = _accessor;
			value = _val;
			type = ASTType::SET;
		}
		void accept(Visitor* vis) override {
			vis->visitSetExpr(this);
		}
	};

	class ConditionalExpr : public ASTNode {
	public:
		ASTNodePtr condition;
		ASTNodePtr mhs;
		ASTNodePtr rhs;

		ConditionalExpr(ASTNodePtr _condition, ASTNodePtr _thenBranch, ASTNodePtr _elseBranch) {
			condition = _condition;
            mhs = _thenBranch;
            rhs = _elseBranch;
			type = ASTType::CONDITIONAL;
		}
		void accept(Visitor* vis) override {
			vis->visitConditionalExpr(this);
		}
	};

	class BinaryExpr : public ASTNode {
	public:
		Token op;
		ASTNodePtr left;
		ASTNodePtr right;

		BinaryExpr(ASTNodePtr _left, Token _op, ASTNodePtr _right) {
			left = _left;
			op = _op;
			right = _right;
			type = ASTType::BINARY;
		}
		void accept(Visitor* vis) override {
			vis->visitBinaryExpr(this);
		}
	};

	class UnaryExpr : public ASTNode {
	public:
		Token op;
		ASTNodePtr right;
		bool isPrefix;

		UnaryExpr(Token _op, ASTNodePtr _right, bool _isPrefix) {
			op = _op;
			right = _right;
			isPrefix = _isPrefix;
			type = ASTType::UNARY;
		}
		void accept(Visitor* vis) override {
			vis->visitUnaryExpr(this);
		}
	};

	class ArrayLiteralExpr : public ASTNode {
	public:
		vector<ASTNodePtr> members;

		ArrayLiteralExpr(vector<ASTNodePtr>& _members) {
			members = _members;
			type = ASTType::ARRAY_LITERAL;
		}
		void accept(Visitor* vis) override {
			vis->visitArrayLiteralExpr(this);
		}
	};

	class CallExpr : public ASTNode {
	public:
		ASTNodePtr callee;
		vector<ASTNodePtr> args;

		CallExpr(ASTNodePtr _callee, vector<ASTNodePtr>& _args) {
			callee = _callee;
			args = _args;
			type = ASTType::CALL;
		}
		void accept(Visitor* vis) override {
			vis->visitCallExpr(this);
		}
	};

    class NewExpr : public ASTNode{
    public:
        shared_ptr<CallExpr> call;
        Token token;

        NewExpr(shared_ptr<CallExpr> _call, Token _token) {
            call = _call;
            token = _token;
            type = ASTType::NEW;
        }
        void accept(Visitor* vis) override {
            vis->visitNewExpr(this);
        }
    };

	//getting values from compound types using '.' or '[]'
	class FieldAccessExpr : public ASTNode {
	public:
		ASTNodePtr callee;
		Token accessor;
		ASTNodePtr field;

		FieldAccessExpr(ASTNodePtr _callee, Token _accessor, ASTNodePtr _field) {
			callee = _callee;
			accessor = _accessor;
			field = _field;
			type = ASTType::FIELD_ACCESS;
		}
		void accept(Visitor* vis) override {
			vis->visitFieldAccessExpr(this);
		}
	};

	class SuperExpr : public ASTNode {
	public:
		Token methodName;

		SuperExpr(Token _methodName) {
			methodName = _methodName;
			type = ASTType::SUPER;
		}
		void accept(Visitor* vis) override {
			vis->visitSuperExpr(this);
		}
	};

	class AsyncExpr : public ASTNode {
	public:
		ASTNodePtr callee;
		vector<ASTNodePtr> args;
		Token token;

		AsyncExpr(Token _token, ASTNodePtr _callee, vector<ASTNodePtr>& _args) {
			callee = _callee;
			args = _args;
			type = ASTType::ASYNC;
			token = _token;
		}
		void accept(Visitor* vis) override {
			vis->visitAsyncExpr(this);
		}
	};

	class AwaitExpr : public ASTNode {
	public:
		ASTNodePtr expr;
		Token token;

		AwaitExpr(Token _token, ASTNodePtr _expr) {
			expr = _expr;
			type = ASTType::AWAIT;
			token = _token;
		}
		void accept(Visitor* vis) override {
			vis->visitAwaitExpr(this);
		}
	};

	class LiteralExpr : public ASTNode {
	public:
		Token token;

		LiteralExpr(Token _token) {
			token = _token;
			type = ASTType::LITERAL;
		}
		void accept(Visitor* vis) override {
			vis->visitLiteralExpr(this);
		}
	};

	struct StructEntry {
		ASTNodePtr expr;
		Token name;
		StructEntry(Token _name, ASTNodePtr _expr) {
            name = _name;
            expr = _expr;
        };
	};

	class StructLiteral : public ASTNode {
	public:
		vector<StructEntry> fields;

		StructLiteral(vector<StructEntry> _fields) {
			fields = _fields;
			type = ASTType::STRUCT;
		}
		void accept(Visitor* vis) {
			vis->visitStructLiteralExpr(this);
		}
	};

	class FuncLiteral : public ASTNode {
	public:
		vector<ASTVar> args;
        int arity;
		shared_ptr<BlockStmt> body;

		FuncLiteral(vector<ASTVar> _args, shared_ptr<BlockStmt> _body) {
			args = _args;
			arity = _args.size();
			body = _body;
			type = ASTType::FUNC_LITERAL;
		}
		void accept(Visitor* vis) {
			vis->visitFuncLiteral(this);
		}
	};

	class ModuleAccessExpr : public ASTNode {
	public:
		Token moduleName;
		Token ident;

		ModuleAccessExpr(Token _moduleName, Token _ident) {
			moduleName = _moduleName;
			ident = _ident;
			type = ASTType::MODULE_ACCESS;
		}

		void accept(Visitor* vis) {
			vis->visitModuleAccessExpr(this);
		}
	};

    class MacroExpr : public ASTNode {
    public:
        Token macroName;
        vector<Token> args;

        MacroExpr(Token _macroName, vector<Token> _args) {
            macroName = _macroName;
            args = _args;
            type = ASTType::MACRO;
        }

        void accept(Visitor* vis) {
            vis->visitMacroExpr(this);
        }
    };

    class RangeExpr : public ASTNode{
    public:
        Token token;
        ASTNodePtr start;
        ASTNodePtr end;
        bool endInclusive;

        RangeExpr(Token _token, ASTNodePtr _start, ASTNodePtr _end, bool _endInclusive) {
            token = _token;
            start = _start;
            end = _end;
            endInclusive = _endInclusive;
            type = ASTType::RANGE;
        }

        void accept(Visitor* vis) {
            vis->visitRangeExpr(this);
        }
    };

    #pragma endregion

	#pragma region Statements

	class ExprStmt : public ASTNode {
	public:
		ASTNodePtr expr;

		ExprStmt(ASTNodePtr _expr) {
			expr = _expr;
			type = ASTType::EXPR_STMT;
		}
		void accept(Visitor* vis) {
			vis->visitExprStmt(this);
		}
	};

	class VarDecl : public ASTDecl {
	public:
		ASTNodePtr value;
        ASTVar var;

		VarDecl(Token _name, ASTNodePtr _value) {
			var.name = _name;
			value = _value;
			type = ASTType::VAR;
		}
		void accept(Visitor* vis) {
			vis->visitVarDecl(this);
		}
		Token getName() { return var.name; }
        ASTDeclType getType() { return ASTDeclType::VAR; }
	};

	class BlockStmt : public ASTNode {
	public:
		vector<ASTNodePtr> statements;

		BlockStmt(vector<ASTNodePtr> _statements) {
			statements = _statements;
			type = ASTType::BLOCK;
		}
		void accept(Visitor* vis) {
			vis->visitBlockStmt(this);
		}
	};

	class IfStmt : public ASTNode {
	public:
		ASTNodePtr thenBranch;
		ASTNodePtr elseBranch;
		ASTNodePtr condition;

		IfStmt(ASTNodePtr _then, ASTNodePtr _else, ASTNodePtr _condition) {
			condition = _condition;
			thenBranch = _then;
			elseBranch = _else;
			type = ASTType::IF;
		}
		void accept(Visitor* vis) {
			vis->visitIfStmt(this);
		}
	};

	class WhileStmt : public ASTNode {
	public:
		ASTNodePtr body;
		ASTNodePtr condition;

		WhileStmt(ASTNodePtr _body, ASTNodePtr _condition) {
			body = _body;
			condition = _condition;
			type = ASTType::WHILE;
		}
		void accept(Visitor* vis) {
			vis->visitWhileStmt(this);
		}
	};

	class ForStmt : public ASTNode {
	public:
		ASTNodePtr body;
		ASTNodePtr init;
		ASTNodePtr condition;
		ASTNodePtr increment;

		ForStmt(ASTNodePtr _init, ASTNodePtr _condition, ASTNodePtr _increment, ASTNodePtr _body) {
			init = _init;
			condition = _condition;
			increment = _increment;
			body = _body;
			type = ASTType::FOR;
		}
		void accept(Visitor* vis) {
			vis->visitForStmt(this);
		}
	};

	class BreakStmt : public ASTNode {
	public:
        // Parser created breaks will never be part of an error, don't need token for error reporting
        BreakStmt() {
            type = ASTType::BREAK;
        }
		void accept(Visitor* vis) {
			vis->visitBreakStmt(this);
		}
	};

	class ContinueStmt : public ASTNode {
	public:
		ContinueStmt() {
			type = ASTType::CONTINUE;
		}
		void accept(Visitor* vis) {
			vis->visitContinueStmt(this);
		}
	};

	class SwitchStmt : public ASTNode {
	public:
		ASTNodePtr expr;
		vector<shared_ptr<CaseStmt>> cases;
		bool hasDefault;

		SwitchStmt(ASTNodePtr _expr, vector<shared_ptr<CaseStmt>> _cases, bool _hasDefault) {
			expr = _expr;
			cases = _cases;
			hasDefault = _hasDefault;
			type = ASTType::SWITCH;
		}
		void accept(Visitor* vis) {
			vis->visitSwitchStmt(this);
		}
	};

	class CaseStmt : public ASTNode {
	public:
		vector<Token> constants;
		vector<ASTNodePtr> stmts;
		Token caseType;//case or default

		CaseStmt(vector<Token> _constants, vector<ASTNodePtr>& _stmts) {
			constants = _constants;
			stmts = _stmts;
			type = ASTType::CASE;
		}
		void accept(Visitor* vis) {
			vis->visitCaseStmt(this);
		}
	};

	class AdvanceStmt : public ASTNode {
	public:
		AdvanceStmt() {
			type = ASTType::ADVANCE;
		}
		void accept(Visitor* vis) {
			vis->visitAdvanceStmt(this);
		}
	};

	class FuncDecl : public ASTDecl {
	public:
		vector<ASTVar> args;
        int arity;
		shared_ptr<BlockStmt> body;
		Token name;

		FuncDecl(Token _name, vector<ASTVar> _args, shared_ptr<BlockStmt> _body) {
			name = _name;
			args = _args;
			arity = _args.size();
			body = _body;
			type = ASTType::FUNC;
		}
		void accept(Visitor* vis) {
			vis->visitFuncDecl(this);
		}
		Token getName() { return name; }
        ASTDeclType getType() { return ASTDeclType::FUNCTION; }
	};

	class ReturnStmt : public ASTNode {
	public:
		ASTNodePtr expr;
		//for error reporting
		Token keyword;

		ReturnStmt(ASTNodePtr _expr, Token _keyword) {
			expr = _expr;
			keyword = _keyword;
			type = ASTType::RETURN;
		}
		void accept(Visitor* vis) {
			vis->visitReturnStmt(this);
		}
	};

    struct ClassMethod{
        bool isPublic;
        bool overrides;
        shared_ptr<FuncDecl> method;

        ClassMethod(bool _isPublic, shared_ptr<FuncDecl> _method, bool _overrides) : isPublic(_isPublic), method(_method), overrides(_overrides) {}
    };

    struct ClassField{
        bool isPublic;
        Token field;

        ClassField(bool _isPublic, Token _field) : isPublic(_isPublic), field(_field) {}
    };

	class ClassDecl : public ASTDecl {
	public:
		Token name;
		ASTNodePtr inheritedClass;
		vector<ClassMethod> methods;
        vector<ClassField> fields;

		ClassDecl(Token _name, vector<ClassMethod> _methods,
                  vector<ClassField> _fields,
                  ASTNodePtr _inheritedClass) {

			name = _name;
			methods = _methods;
            fields = _fields;
			inheritedClass = _inheritedClass;
			type = ASTType::CLASS;
		}
		void accept(Visitor* vis) {
			vis->visitClassDecl(this);
		}
		Token getName() { return name; }
        ASTDeclType getType() { return ASTDeclType::CLASS; }
	};
#pragma endregion

}