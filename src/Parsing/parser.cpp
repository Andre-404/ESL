#include "parser.h"
#include "../ErrorHandling/errorHandler.h"
#include "../DebugPrinting/ASTPrinter.h"
#include "../Includes/fmt/format.h"

using std::make_shared;
using namespace AST;

// Have to define this in the AST namespace because parselets are c++ friend classes
namespace AST {
	//!, -, ~, ++a, --a, async and await
	class UnaryPrefixParselet : public PrefixParselet {
	public:
		UnaryPrefixParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(Token token) {
			// Macro meta variables
            if (token.type == TokenType::DOLLAR){
                Token metaVar = cur->consume(TokenType::IDENTIFIER, "Expected identifier after '$'.");
                return cur->exprMetaVars[metaVar.getLexeme()]->get();
            }


			switch (token.type) {
			case TokenType::AWAIT: {
                // Parse a new expression with 0 precedence
                ASTNodePtr expr = cur->expression();
                return make_shared<AwaitExpr>(token, expr);
            }
			case TokenType::ASYNC: {
                ASTNodePtr expr = cur->expression(prec);
				if (expr->type != ASTType::CALL) throw cur->error(token, "Expected a call after 'thread'.");
				CallExpr* call = dynamic_cast<CallExpr*>(expr.get());
				return make_shared<AsyncExpr>(token, call->callee, call->args);
			}
			default: {
                ASTNodePtr expr = cur->expression(prec);
                return make_shared<UnaryExpr>(token, expr, true);
            }
			}
		}
	};

	//numbers, string, boolean, nil, array and struct literals, grouping as well as super calls and anonymous functions
	class LiteralParselet : public PrefixParselet {
	public:
		LiteralParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(Token token) {
			switch (token.type) {
				//only thing that gets inherited is methods
			case TokenType::SUPER: {
				cur->consume(TokenType::DOT, "Expected '.' after super.");
				Token ident = cur->consume(TokenType::IDENTIFIER, "Expect superclass method name.");
				return make_shared<SuperExpr>(ident);
			}
			case TokenType::LEFT_PAREN: {
				//grouping can contain a expr of any precedence
				ASTNodePtr expr = cur->expression();
				cur->consume(TokenType::RIGHT_PAREN, "Expected ')' at the end of grouping expression.");
				return expr;
			}
			//Array literal
			case TokenType::LEFT_BRACKET: {
				vector<ASTNodePtr> members;
				if (cur->peek().type != TokenType::RIGHT_BRACKET) {
					do {
						members.push_back(cur->expression());
					} while (cur->match(TokenType::COMMA));
				}
				cur->consume(TokenType::RIGHT_BRACKET, "Expect ']' at the end of an array literal.");
				return make_shared<ArrayLiteralExpr>(members);
			}
			//Struct literal
			case TokenType::LEFT_BRACE: {
				vector<StructEntry> entries;
				if (cur->peek().type != TokenType::RIGHT_BRACE) {
					//a struct literal looks like this: {var1 : expr1, var2 : expr2}
					do {
						Token identifier = cur->consume(TokenType::STRING, "Expected a string identifier.");
						cur->consume(TokenType::COLON, "Expected a ':' after string identifier");
						ASTNodePtr expr = cur->expression();
						entries.emplace_back(identifier, expr);
					} while (cur->match(TokenType::COMMA));
				}
				cur->consume(TokenType::RIGHT_BRACE, "Expect '}' after struct literal.");
				return make_shared<StructLiteral>(entries);
			}
			//function literal
			case TokenType::FN: {
				//the depths are used for throwing errors for switch and loops stmts, 
				//and since a function can be declared inside a loop we need to account for that
				int tempLoopDepth = cur->loopDepth;
				int tempSwitchDepth = cur->switchDepth;
				cur->loopDepth = 0;
				cur->switchDepth = 0;

				cur->consume(TokenType::LEFT_PAREN, "Expect '(' for arguments.");
				vector<ASTVar> args;
				//parse args
				if (!cur->check(TokenType::RIGHT_PAREN)) {
					do {
						Token arg = cur->consume(TokenType::IDENTIFIER, "Expect argument name");
						args.emplace_back(arg);
						if (args.size() > 127) {
							throw cur->error(arg, "Functions can't have more than 128 arguments");
						}
					} while (cur->match(TokenType::COMMA));
				}
				cur->consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments");
				cur->consume(TokenType::LEFT_BRACE, "Expect '{' after arguments.");
                shared_ptr<BlockStmt> body = cur->blockStmt();

				cur->loopDepth = tempLoopDepth;
				cur->switchDepth = tempSwitchDepth;
				return make_shared<FuncLiteral>(args, body);
			}
            case TokenType::NEW:{
                // new keyword is followed by a call to the class that is being instantiated, class must be an identifier
                // or module access to identifier
                auto call = cur->expression(+Precedence::CALL - 1);
                if(call->type != ASTType::CALL) throw cur->error(token, "Expected a call to class.");
                auto castCall = std::static_pointer_cast<CallExpr>(call);
                auto type = castCall->callee->type;
                if(!(type == AST::ASTType::LITERAL || type == AST::ASTType::MODULE_ACCESS)) {
                    throw cur->error(token, "Expected a class identifier or module access to class identifier.");
                }
                return make_shared<NewExpr>(castCall, token);
            }
			//number, string, boolean or nil
			default:
				return make_shared<LiteralExpr>(token);
			}
		}
	};

	//variable assignment
	class AssignmentParselet : public InfixParselet {
	public:
		AssignmentParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) {

			if (left->type != ASTType::LITERAL) throw cur->error(token, "Left side is not assignable");

			left->accept(cur->probe);
            Token temp = cur->probe->getProbedToken();
			if (temp.type != TokenType::IDENTIFIER) throw cur->error(token, "Left side is not assignable");
			//makes it right associative
			ASTNodePtr right = parseAssign(left, token);
			return make_shared<AssignmentExpr>(temp, right);
		}

		//used for parsing assignment tokens(eg. =, +=, *=...)
		ASTNodePtr parseAssign(ASTNodePtr left, Token op) {
			ASTNodePtr right = cur->expression();
			switch (op.type) {
			case TokenType::EQUAL: {
				break;
			}
			case TokenType::PLUS_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::PLUS, op), right);
				break;
			}
			case TokenType::MINUS_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::MINUS, op), right);
				break;
			}
			case TokenType::SLASH_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::SLASH, op), right);
				break;
			}
			case TokenType::STAR_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::STAR, op), right);
				break;
			}
			case TokenType::BITWISE_XOR_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_XOR, op), right);
				break;
			}
			case TokenType::BITWISE_AND_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_AND, op), right);
				break;
			}
			case TokenType::BITWISE_OR_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_OR, op), right);
				break;
			}
			case TokenType::PERCENTAGE_EQUAL: {
				right = make_shared<BinaryExpr>(left, Token(TokenType::PERCENTAGE, op), right);
				break;
			}
			}
			return right;
		}
	};

	//?: operator
	class ConditionalParselet : public InfixParselet {
	public:
		ConditionalParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) {
			ASTNodePtr thenBranch = cur->expression(prec - 1);
			cur->consume(TokenType::COLON, "Expected ':' after then branch.");
			ASTNodePtr elseBranch = cur->expression(prec - 1);
			return make_shared<ConditionalExpr>(left, thenBranch, elseBranch);
		}
	};

	//any binary operation + module alias access operator(::) + macro invocation (!)
	class BinaryParselet : public InfixParselet {
	public:
		BinaryParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) {

            switch(token.type){
                case TokenType::DOUBLE_COLON:{
                    if(left->type != ASTType::LITERAL) throw cur->error(token, "Expected module name identifier.");
                    left->accept(cur->probe);
                    Token ident = cur->consume(TokenType::IDENTIFIER, "Expected variable name.");
                    return make_shared<ModuleAccessExpr>(cur->probe->getProbedToken(), ident);
                }
                case TokenType::BANG:{
                    if(left->type != ASTType::LITERAL) throw cur->error(token, "Expected macro name to be an identifier.");
                    left->accept(cur->probe);
                    Token macroName = cur->probe->getProbedToken();
                    if (macroName.type != TokenType::IDENTIFIER) {
                        throw cur->error(macroName, "Expected macro name to be an identifier.");
                    }
                    if (!cur->macros.contains(macroName.getLexeme())) {
                        throw cur->error(macroName, "Invoked macro isn't defined");
                    }
                    return make_shared<MacroExpr>(macroName, cur->readTokenTree());
                }
                case TokenType::INSTANCEOF:{
                    auto right = cur->expression(+Precedence::PRIMARY - 1);
                    if(!(right->type == ASTType::LITERAL || right->type == ASTType::MODULE_ACCESS)){
                        throw cur->error(token, "Right side of the 'instanceof' operator can only be an identifier.");
                    }
                    return make_shared<BinaryExpr>(left, token, right);
                }
                default:{
                    ASTNodePtr right = cur->expression(prec);
                    return make_shared<BinaryExpr>(left, token, right);
                }
            }
		}
	};

	//a++, a--
	class UnaryPostfixParselet : public InfixParselet {
	public:
		UnaryPostfixParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr var, Token op, int surroundingPrec) {
			return make_shared<UnaryExpr>(op, var, false);
		}
	};

	//function calling
	class CallParselet : public InfixParselet {
	public:
		CallParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) {
			vector<ASTNodePtr> args;
			if (!cur->check(TokenType::RIGHT_PAREN)) {
				do {
					args.push_back(cur->expression());
				} while (cur->match(TokenType::COMMA));
			}
			cur->consume(TokenType::RIGHT_PAREN, "Expect ')' after call expression.");
			return make_shared<CallExpr>(left, args);
		}
	};

	//accessing struct, class or array fields
	class FieldAccessParselet : public InfixParselet {
	public:
		FieldAccessParselet(Parser* _cur, int _prec) {
			cur = _cur;
			prec = _prec;
		}
		ASTNodePtr parse(ASTNodePtr left, Token token, int surroundingPrec) {
			ASTNodePtr field = nullptr;
			Token newToken = token;
			if (token.type == TokenType::LEFT_BRACKET) {//array/struct with string access
				field = cur->expression();
				//object["field"] gets optimized to object.field
				if (field->type == ASTType::LITERAL) {
					field->accept(cur->probe);
					if (cur->probe->getProbedToken().type == TokenType::STRING) newToken.type = TokenType::DOT;
				}
				cur->consume(TokenType::RIGHT_BRACKET, "Expect ']' after array/map access.");
			}
			else if (token.type == TokenType::DOT) {//struct/object access
				Token fieldName = cur->consume(TokenType::IDENTIFIER, "Expected a field identifier.");
				field = make_shared<LiteralExpr>(fieldName);
			}
			//if we have something like arr[0] = 1 or struct.field = 1 we can't parse it with the assignment expr
			//this handles that case and produces a special set expr
			//we also check the precedence level of the surrounding expression, so "a + b.c = 3" doesn't get parsed
			//the match() covers every possible type of assignment
			if (surroundingPrec <= (int)Precedence::ASSIGNMENT
				&& cur->match({ TokenType::EQUAL, TokenType::PLUS_EQUAL, TokenType::MINUS_EQUAL, TokenType::SLASH_EQUAL,
					TokenType::STAR_EQUAL, TokenType::BITWISE_XOR_EQUAL, TokenType::BITWISE_AND_EQUAL,
					TokenType::BITWISE_OR_EQUAL, TokenType::PERCENTAGE_EQUAL })) {
				Token op = cur->previous();
				ASTNodePtr val = parseAssign(make_shared<FieldAccessExpr>(left, newToken, field), op);
				return make_shared<SetExpr>(left, field, newToken, op, val);
			}
			return make_shared<FieldAccessExpr>(left, newToken, field);
		}

        //used for parsing assignment tokens(eg. =, +=, *=...)
        ASTNodePtr parseAssign(ASTNodePtr left, Token op) {
            ASTNodePtr right = cur->expression();
            switch (op.type) {
                case TokenType::EQUAL: {
                    break;
                }
                case TokenType::PLUS_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::PLUS, op), right);
                    break;
                }
                case TokenType::MINUS_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::MINUS, op), right);
                    break;
                }
                case TokenType::SLASH_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::SLASH, op), right);
                    break;
                }
                case TokenType::STAR_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::STAR, op), right);
                    break;
                }
                case TokenType::BITWISE_XOR_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_XOR, op), right);
                    break;
                }
                case TokenType::BITWISE_AND_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_AND, op), right);
                    break;
                }
                case TokenType::BITWISE_OR_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::BITWISE_OR, op), right);
                    break;
                }
                case TokenType::PERCENTAGE_EQUAL: {
                    right = make_shared<BinaryExpr>(left, Token(TokenType::PERCENTAGE, op), right);
                    break;
                }
            }
            return right;
        }
	};

}

Parser::Parser() {
	probe = new ASTProbe;
    macroExpander = new MacroExpander(this);

	loopDepth = 0;
	switchDepth = 0;
    parsedUnit = nullptr;

    currentContainer = nullptr;
    currentPtr = 0;

	#pragma region Parselets
	// Prefix
	addPrefix<LiteralParselet>(TokenType::THIS, Precedence::NONE);

	addPrefix<UnaryPrefixParselet>(TokenType::BANG, Precedence::NOT);
	addPrefix<UnaryPrefixParselet>(TokenType::MINUS, Precedence::NOT);
	addPrefix<UnaryPrefixParselet>(TokenType::TILDA, Precedence::NOT);
    // Only for macros
    addPrefix<UnaryPrefixParselet>(TokenType::DOLLAR, Precedence::NOT);

	addPrefix<UnaryPrefixParselet>(TokenType::INCREMENT, Precedence::ALTER);
	addPrefix<UnaryPrefixParselet>(TokenType::DECREMENT, Precedence::ALTER);

	addPrefix<UnaryPrefixParselet>(TokenType::ASYNC, Precedence::ASYNC);
	addPrefix<UnaryPrefixParselet>(TokenType::AWAIT, Precedence::ASYNC);

	addPrefix<LiteralParselet>(TokenType::IDENTIFIER, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::STRING, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::NUMBER, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::TRUE, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::FALSE, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::NIL, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::LEFT_PAREN, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::LEFT_BRACKET, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::LEFT_BRACE, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::SUPER, Precedence::PRIMARY);
	addPrefix<LiteralParselet>(TokenType::FN, Precedence::PRIMARY);
    addPrefix<LiteralParselet>(TokenType::NEW, Precedence::PRIMARY);

	// Infix
	addInfix<AssignmentParselet>(TokenType::EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::PLUS_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::MINUS_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::SLASH_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::STAR_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::PERCENTAGE_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::BITWISE_XOR_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::BITWISE_OR_EQUAL, Precedence::ASSIGNMENT);
	addInfix<AssignmentParselet>(TokenType::BITWISE_AND_EQUAL, Precedence::ASSIGNMENT);

	addInfix<ConditionalParselet>(TokenType::QUESTIONMARK, Precedence::CONDITIONAL);

	addInfix<BinaryParselet>(TokenType::OR, Precedence::OR);
	addInfix<BinaryParselet>(TokenType::AND, Precedence::AND);

	addInfix<BinaryParselet>(TokenType::BITWISE_OR, Precedence::BIN_OR);
	addInfix<BinaryParselet>(TokenType::BITWISE_XOR, Precedence::BIN_XOR);
	addInfix<BinaryParselet>(TokenType::BITWISE_AND, Precedence::BIN_AND);

	addInfix<BinaryParselet>(TokenType::EQUAL_EQUAL, Precedence::EQUALITY);
	addInfix<BinaryParselet>(TokenType::BANG_EQUAL, Precedence::EQUALITY);

	addInfix<BinaryParselet>(TokenType::LESS, Precedence::COMPARISON);
	addInfix<BinaryParselet>(TokenType::LESS_EQUAL, Precedence::COMPARISON);
	addInfix<BinaryParselet>(TokenType::GREATER, Precedence::COMPARISON);
	addInfix<BinaryParselet>(TokenType::GREATER_EQUAL, Precedence::COMPARISON);

	addInfix<BinaryParselet>(TokenType::BITSHIFT_LEFT, Precedence::BITSHIFT);
	addInfix<BinaryParselet>(TokenType::BITSHIFT_RIGHT, Precedence::BITSHIFT);

	addInfix<BinaryParselet>(TokenType::PLUS, Precedence::SUM);
	addInfix<BinaryParselet>(TokenType::MINUS, Precedence::SUM);

	addInfix<BinaryParselet>(TokenType::SLASH, Precedence::FACTOR);
	addInfix<BinaryParselet>(TokenType::STAR, Precedence::FACTOR);
	addInfix<BinaryParselet>(TokenType::PERCENTAGE, Precedence::FACTOR);
    addInfix<BinaryParselet>(TokenType::BANG, Precedence::PRIMARY);

	addInfix<CallParselet>(TokenType::LEFT_PAREN, Precedence::CALL);
	addInfix<FieldAccessParselet>(TokenType::LEFT_BRACKET, Precedence::CALL);
	addInfix<FieldAccessParselet>(TokenType::DOT, Precedence::CALL);

	addInfix<BinaryParselet>(TokenType::DOUBLE_COLON, Precedence::PRIMARY);
    addInfix<BinaryParselet>(TokenType::INSTANCEOF, Precedence::PRIMARY);

	//postfix and mix-fix operators get parsed with the infix parselets
	addInfix<UnaryPostfixParselet>(TokenType::INCREMENT, Precedence::ALTER);
	addInfix<UnaryPostfixParselet>(TokenType::DECREMENT, Precedence::ALTER);
#pragma endregion
}

void Parser::parse(vector<CSLModule*>& modules) {
	#ifdef AST_DEBUG
	ASTPrinter* astPrinter = new ASTPrinter;
	#endif
	//modules are already sorted using toposort
	for (CSLModule* unit : modules) {
        parsedUnit = unit;

        // Parse tokenized source into AST
		loopDepth = 0;
		switchDepth = 0;
        currentContainer = &parsedUnit->tokens;
        currentPtr = 0;
		while (!isAtEnd()) {
			try {
                if (match(TokenType::ADDMACRO)) {
                    defineMacro();
                    continue;
                }
				unit->stmts.push_back(topLevelDeclaration());
				#ifdef AST_DEBUG
				//prints statement
				unit->stmts[unit->stmts.size() - 1]->accept(astPrinter);
				#endif
			}
			catch (ParserException& e) {
				sync();
			}
		}

        expandMacros();
	}
	// 2 units being imported using the same alias is illegal
    // Units imported without an alias must abide by the rule that every symbol must be unique
	for (CSLModule* unit : modules) {
		std::unordered_map<string, Dependency*> symbols;
        // Symbols of this unit are also taken into account when checking uniqueness
        for(auto decl : unit->topDeclarations){
            symbols[decl->getName().getLexeme()] = nullptr;
        }
		std::unordered_map<string, Dependency*> importAliases;

		for (Dependency& dep : unit->deps) {
			if (dep.alias.type == TokenType::NONE) {
				for (const auto decl : dep.module->exports) {
					string lexeme = decl->getName().getLexeme();

					if (symbols.count(lexeme) == 0) {
                        symbols[lexeme] = &dep;
						continue;
					}
					// If there are 2 or more declaration which use the same symbol,
					// throw an error and tell the user exactly which dependencies caused the error

					string str = fmt::format("Ambiguous definition, symbol '{}' defined in {} and {}.",
						lexeme, symbols[lexeme] ? symbols[lexeme]->pathString.getLexeme() : "this file", dep.pathString.getLexeme());
                    if(!symbols[lexeme]){
                        for(auto thisFileDecl : unit->topDeclarations){
                            if(thisFileDecl->getName().getLexeme() != lexeme) continue;
                            error(thisFileDecl->getName(), str);
                        }
                    }else error(dep.pathString, str);
				}
			}
			else {
				// Check if any imported dependencies share the same alias
				if (importAliases.count(dep.alias.getLexeme()) > 0) {
					error(importAliases[dep.alias.getLexeme()]->alias, "Cannot use the same alias for 2 module imports.");
					error(dep.alias, "Cannot use the same alias for 2 module imports.");
				}
				importAliases[dep.alias.getLexeme()] = &dep;
			}
		}
	}
}

void Parser::defineMacro() {
    consume(TokenType::BANG, "Expected '!' after 'addMacro' token.");
    Token macroName = consume(TokenType::IDENTIFIER, "Expected macro name to be an identifier.");
    consume(TokenType::LEFT_BRACE, "Expected '{' initiating macro definition.");

    macros[macroName.getLexeme()] = std::make_unique<Macro>(macroName, this);
    auto& macro = macros[macroName.getLexeme()];

    while (!isAtEnd() && !check(TokenType::RIGHT_BRACE)) {
        MatchPattern matcher(readTokenTree(), this);
        consume(TokenType::ARROW, "Expected '=>' after matcher expression.");
        if (!check(TokenType::LEFT_BRACE)) { throw error(peek(), "Expected '{' initiating transcriber expression."); }
        vector<Token> transcriber = readTokenTree();
        // erase '{' and '}' from transcriber
        transcriber.erase(transcriber.begin());
        transcriber.pop_back();

        consume(TokenType::SEMICOLON, "Expected ';' after transcriber expression.");

        macro->matchers.push_back(matcher);
        macro->transcribers.push_back(transcriber);
    }
    consume(TokenType::RIGHT_BRACE, "Unexpected incomplete macro definition.");
}

ASTNodePtr Parser::expression(int prec) {
	Token token = advance();
	//check if the token has a prefix function associated with it, and if it does, parse with it
	if (prefixParselets.count(token.type) == 0) {
        // TODO: Fix hackyness
        if (token.str.length == 0) token = currentContainer->at(currentPtr - 2);
		throw error(token, "Expected expression.");
	}
    if (token.type == TokenType::DOLLAR && parseMode != ParseMode::Macro){
        throw error(token, "Unexpected '$' found outside of macro transcriber.");
    }
	unique_ptr<PrefixParselet>& prefix = prefixParselets[token.type];
	shared_ptr<ASTNode> left = prefix->parse(token);

	//advances only if the next token has a higher precedence than the parserCurrent one
	//e.g. 1 + 2 compiles because the base precedence is 0, and '+' has a precedence of 11
	//loop runs as long as the next operator has a higher precedence than the one that called this function
	while (prec < getPrec()) {
		token = advance();
		if (infixParselets.count(token.type) == 0) {
			throw error(token, "Expected expression.");
		}
		unique_ptr<InfixParselet>& infix = infixParselets[token.type];
		left = infix->parse(left, token, prec);
	}
	return left;
}

ASTNodePtr Parser::expression() {
	return expression(0);
}

#pragma region Statements and declarations
//module level variables are put in a list to help with error reporting in compiler
ASTNodePtr Parser::topLevelDeclaration() {
	//export is only allowed in global scope
    shared_ptr<ASTDecl> node = nullptr;
    bool isExported = false;
    if (match(TokenType::PUB)) isExported = true;
    if (match(TokenType::LET)) node = varDecl();
    else if (match(TokenType::CLASS)) node = classDecl();
    else if (match(TokenType::FN)) node = funcDecl();
    else if(isExported) throw error(previous(), "Only declarations are allowed after 'export'");
    if(node){
        for(const auto decl : parsedUnit->topDeclarations){
            if (node->getName().equals(decl->getName())) {
                error(node->getName(), fmt::format("Error, {} already defined.", node->getName().getLexeme()));
                throw error(decl->getName(), fmt::format("Error, redefinition of {}.", decl->getName().getLexeme()));
            }
        }
        // Passing in the actual AST node, not just the name, because it also contains info about declaration type(var, func, class)
        parsedUnit->topDeclarations.push_back(node);
        if(isExported) parsedUnit->exports.push_back(node);
        return node;
    }
	return statement();
}

ASTNodePtr Parser::localDeclaration() {
	if (match(TokenType::LET)) return varDecl();
	return statement();
}

shared_ptr<VarDecl> Parser::varDecl() {
	Token name = consume(TokenType::IDENTIFIER, "Expected a variable identifier.");
	ASTNodePtr expr = nullptr;
	//if no initializer is present the variable is initialized to null
	if (match(TokenType::EQUAL)) {
		expr = expression();
	}
	consume(TokenType::SEMICOLON, "Expected a ';' after variable declaration.");
	return make_shared<VarDecl>(name, expr);
}

shared_ptr<FuncDecl> Parser::funcDecl() {
	//the depths are used for throwing errors for switch and loops stmts, 
	//and since a function can be declared inside a loop we need to account for that
	int tempLoopDepth = loopDepth;
	int tempSwitchDepth = switchDepth;
	loopDepth = 0;
	switchDepth = 0;

	Token name = consume(TokenType::IDENTIFIER, "Expected a function name.");
	consume(TokenType::LEFT_PAREN, "Expect '(' after function name.");
	vector<ASTVar> args;
	//parse args
	if (!check(TokenType::RIGHT_PAREN)) {
		do {
			Token arg = consume(TokenType::IDENTIFIER, "Expect argument name");
			args.emplace_back(arg);
			if (args.size() > 127) {
				throw error(arg, "Functions can't have more than 127 arguments");
			}
		} while (match(TokenType::COMMA));
	}
	consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments");
	consume(TokenType::LEFT_BRACE, "Expect '{' after arguments.");
    shared_ptr<BlockStmt> body = blockStmt();

	loopDepth = tempLoopDepth;
	switchDepth = tempSwitchDepth;
	return make_shared<FuncDecl>(name, args, body);
}

shared_ptr<ClassDecl> Parser::classDecl() {
	Token name = consume(TokenType::IDENTIFIER, "Expected a class name.");
	ASTNodePtr inherited = nullptr;
	// Inheritance is optional
	if (match(TokenType::COLON)) {
		Token token = previous();
		// Only accept identifiers and module access
		inherited = expression(+Precedence::PRIMARY - 1);
		if (!((inherited->type == ASTType::LITERAL && dynamic_cast<LiteralExpr*>(inherited.get())->token.type == TokenType::IDENTIFIER)
			|| inherited->type == ASTType::MODULE_ACCESS)) {
			error(token, "Superclass can only be an identifier.");
		}
	}

	consume(TokenType::LEFT_BRACE, "Expect '{' before class body.");

    vector<ClassMethod> methods;
    vector<ClassField> fields;

    auto checkName = [&](Token token, bool isMethod, bool isPublic){
        if(isMethod){
            for(auto& m : methods){
                if(token.equals(m.method->name)) {
                    throw error(token, "Re-declaration of method.");
                    throw error(m.method->name, "Method first defined here.");
                    return;
                }
            }
            return;
        }
        for(auto& field : fields){
            if(token.equals(field.field)) {
                throw error(token, "Re-declaration of field.");
                throw error(field.field, "Field first defined here.");
                return;
            }
        }
        return;
    };

	while (!check(TokenType::RIGHT_BRACE) && !isAtEnd()) {
        try {
            bool isPublic = false;
            if (match(TokenType::PUB)) {
                isPublic = true;
            }
            if (match(TokenType::LET)) {
                Token field = consume(TokenType::IDENTIFIER, "Expected a field identifier.");

                checkName(field, false, isPublic);
                fields.emplace_back(isPublic, field);

                while (!check(TokenType::SEMICOLON) && !check(TokenType::RIGHT_BRACE) && !isAtEnd()) {
                    if (!match(TokenType::COMMA)) break;
                    field = consume(TokenType::IDENTIFIER, "Expected a field identifier.");
                    checkName(field, false, isPublic);
                    fields.emplace_back(isPublic, field);
                }

                consume(TokenType::SEMICOLON, "Expected ';' after field name");
            } else if (match(TokenType::FN)) {
                auto decl = funcDecl();
                checkName(decl->name, true, isPublic);
                // Implicitly declare "this"
                decl->args.insert(decl->args.begin(), ASTVar(Token(TokenType::IDENTIFIER, "this")));
                methods.emplace_back(isPublic, decl);
            } else {
                throw error(peek(), "Expected let or fn keywords.");
            }
        }catch(ParserException& e){
            sync();
        }
	}
	consume(TokenType::RIGHT_BRACE, "Expect '}' after class body.");
	return make_shared<ClassDecl>(name, methods, fields, inherited);
}

ASTNodePtr Parser::statement() {
	if (match({ TokenType::LEFT_BRACE, TokenType::IF, TokenType::WHILE,
		TokenType::FOR, TokenType::BREAK, TokenType::SWITCH,
		TokenType::RETURN, TokenType::CONTINUE, TokenType::ADVANCE })) {

		switch (previous().type) {
		case TokenType::LEFT_BRACE: return blockStmt();
		case TokenType::IF: return ifStmt();
		case TokenType::WHILE: return whileStmt();
		case TokenType::FOR: return forStmt();
		case TokenType::BREAK: return breakStmt();
		case TokenType::CONTINUE: return continueStmt();
		case TokenType::ADVANCE: return advanceStmt();
		case TokenType::SWITCH: return switchStmt();
		case TokenType::RETURN: return returnStmt();
		}
	}
	return exprStmt();
}

shared_ptr<ExprStmt> Parser::exprStmt() {
	ASTNodePtr expr = expression();
	consume(TokenType::SEMICOLON, "Expected ';' after expression.");
	return make_shared<ExprStmt>(expr);
}

shared_ptr<BlockStmt> Parser::blockStmt() {
	vector<ASTNodePtr> stmts;
	//TokenType::LEFT_BRACE is already consumed
	while (!check(TokenType::RIGHT_BRACE)) {
		stmts.push_back(localDeclaration());
	}
	consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
	return make_shared<BlockStmt>(stmts);
}

shared_ptr<IfStmt> Parser::ifStmt() {
	consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.");
	ASTNodePtr condition = expression();
	consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");
	//using statement() instead of declaration() disallows declarations directly in a control flow body
	//declarations are still allowed in block statement
	ASTNodePtr thenBranch = statement();
	ASTNodePtr elseBranch = nullptr;
	if (match(TokenType::ELSE)) {
		elseBranch = statement();
	}
	return make_shared<IfStmt>(thenBranch, elseBranch, condition);
}

shared_ptr<WhileStmt> Parser::whileStmt() {
	//loopDepth is used to see if a 'continue' or 'break' statement is allowed within the body
	loopDepth++;
	consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.");
	ASTNodePtr condition = expression();
	consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");
	ASTNodePtr body = statement();
	loopDepth--;
	return make_shared<WhileStmt>(body, condition);
}

shared_ptr<ForStmt> Parser::forStmt() {
	loopDepth++;
	consume(TokenType::LEFT_PAREN, "Expect '(' after 'for'.");
	//initializer can either be: empty, a new variable declaration, or any expression
	ASTNodePtr init = nullptr;
	if (match(TokenType::SEMICOLON)) {
		//do nothing
	}
	else if (match(TokenType::LET)) init = varDecl();
	else init = exprStmt();

	ASTNodePtr condition = nullptr;
	//we don't want to use exprStmt() because it emits OP_POP, and we'll need the value to determine whether to jump
	if (!check(TokenType::SEMICOLON)) condition = expression();
	consume(TokenType::SEMICOLON, "Expect ';' after loop condition");

	ASTNodePtr increment = nullptr;
	//using expression() here instead of exprStmt() because there is no trailing ';'
	if (!check(TokenType::RIGHT_PAREN)) increment = expression();
	consume(TokenType::RIGHT_PAREN, "Expect ')' after 'for' clauses.");
	//disallows declarations unless they're in a block
	ASTNodePtr body = statement();
	loopDepth--;
	return make_shared<ForStmt>(init, condition, increment, body);
}

shared_ptr<BreakStmt> Parser::breakStmt() {
	if (loopDepth == 0 && switchDepth == 0) throw error(previous(), "Cannot use 'break' outside of loops or switch statements.");
	consume(TokenType::SEMICOLON, "Expect ';' after break.");
	return make_shared<BreakStmt>(previous());
}

shared_ptr<ContinueStmt> Parser::continueStmt() {
	if (loopDepth == 0) throw error(previous(), "Cannot use 'continue' outside of loops.");
	consume(TokenType::SEMICOLON, "Expect ';' after continue.");
	return make_shared<ContinueStmt>(previous());
}

shared_ptr<SwitchStmt> Parser::switchStmt() {
	//structure:
	//switch(<expression>){
	//case <expression>: <statements>
	//}
	consume(TokenType::LEFT_PAREN, "Expect '(' after 'switch'.");
	ASTNodePtr expr = expression();
	consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
	consume(TokenType::LEFT_BRACE, "Expect '{' after switch expression.");
	switchDepth++;
	vector<shared_ptr<CaseStmt>> cases;
	bool hasDefault = false;

	while (!check(TokenType::RIGHT_BRACE) && match({ TokenType::CASE, TokenType::DEFAULT })) {
		Token prev = previous();//to see if it's a default statement
		shared_ptr<CaseStmt> curCase = caseStmt();
		curCase->caseType = prev;
		if (prev.type == TokenType::DEFAULT) {
			//don't throw, it isn't a breaking error
			if (hasDefault) error(prev, "Only 1 default case is allowed inside a switch statement.");
			hasDefault = true;
		}
		cases.push_back(curCase);
	}
	consume(TokenType::RIGHT_BRACE, "Expect '}' after switch body.");
	switchDepth--;
	return make_shared<SwitchStmt>(expr, cases, hasDefault);
}

shared_ptr<CaseStmt> Parser::caseStmt() {
	vector<Token> matchConstants;
	//default cases don't have a match expression
	if (previous().type != TokenType::DEFAULT) {
		while (match({ TokenType::NIL, TokenType::NUMBER, TokenType::STRING, TokenType::TRUE, TokenType::FALSE })) {
			matchConstants.push_back(previous());
			if (!match(TokenType::BITWISE_OR)) break;
		}
		if (!match({ TokenType::NIL, TokenType::NUMBER, TokenType::STRING, TokenType::TRUE, TokenType::FALSE }) && peek().type != TokenType::COLON) {
			throw error(peek(), "Expression must be a constant literal(string, number, boolean or nil).");
		}
	}
	consume(TokenType::COLON, "Expect ':' after 'case' or 'default'.");
	vector<ASTNodePtr> stmts;
	while (!check(TokenType::CASE) && !check(TokenType::RIGHT_BRACE) && !check(TokenType::DEFAULT)) {
		stmts.push_back(localDeclaration());
	}
	return make_shared<CaseStmt>(matchConstants, stmts);
}

shared_ptr<AdvanceStmt> Parser::advanceStmt() {
	if (switchDepth == 0) throw error(previous(), "Cannot use 'advance' outside of switch statements.");
	consume(TokenType::SEMICOLON, "Expect ';' after 'advance'.");
	return make_shared<AdvanceStmt>(previous());
}

shared_ptr<ReturnStmt> Parser::returnStmt() {
	ASTNodePtr expr = nullptr;
	Token keyword = previous();
	if (!match(TokenType::SEMICOLON)) {
		expr = expression();
		consume(TokenType::SEMICOLON, "Expect ';' at the end of 'return'.");
	}
	return make_shared<ReturnStmt>(expr, keyword);
}

#pragma endregion

#pragma region Helpers
//if the parserCurrent token type matches any of the provided tokenTypes it's consumed, if not false is returned
bool Parser::match(const std::initializer_list<TokenType>& tokenTypes) {
	if (check(tokenTypes)) {
        advance();
        return true;
    }
	return false;
}

bool Parser::match(const TokenType type) {
	return match({ type });
}

bool Parser::isAtEnd() {
	return currentContainer->size() <= currentPtr;
}

bool Parser::check(const std::initializer_list<TokenType>& tokenTypes){
    if (isAtEnd()) return false;
    for (const TokenType& type : tokenTypes){
        if (type == peek().type){
            return true;
        }
    }
    return false;
}

bool Parser::check(const TokenType type) {
	return check({ type });
}

//returns parserCurrent token and increments to the next one
Token Parser::advance() {
	if (isAtEnd()) throw error(currentContainer->back(), "Expected token.");
    currentPtr++;
	return previous();
}

//gets parserCurrent token
Token Parser::peek() {
	if (isAtEnd()) throw error(currentContainer->back(), "Expected token.");
	return currentContainer->at(currentPtr);
}

//gets next token
Token Parser::peekNext() {
	if (currentContainer->size() <= currentPtr + 1) throw error(currentContainer->back(), "Expected token.");
	return currentContainer->at(currentPtr + 1);
}

Token Parser::previous() {
	if (currentPtr - 1 < 0) throw error(currentContainer->at(0), "Expected token.");
	return currentContainer->at(currentPtr - 1);
}

//if the parserCurrent token is of the correct type, it's consumed, if not an error is thrown
Token Parser::consume(TokenType type, string msg) {
	if (check(type)) return advance();

	throw error(peek(), msg);
}

ParserException Parser::error(Token token, string msg) {
    if (parseMode != ParseMode::Matcher) {
        errorHandler::addCompileError(msg, token);
    }
	return ParserException();
}

vector<Token> Parser::readTokenTree(bool isNonLeaf)
{
    if (!isNonLeaf && !check({TokenType::LEFT_PAREN, TokenType::LEFT_BRACE, TokenType::LEFT_BRACKET, TokenType::RIGHT_PAREN, TokenType::RIGHT_BRACE, TokenType::RIGHT_BRACKET})) { return { advance() }; }

    if (!check({TokenType::LEFT_PAREN, TokenType::LEFT_BRACE, TokenType::LEFT_BRACKET})) { throw error(peek(), "Expected '(', '{' or '[' initiating token tree."); }

    vector<Token> tokenTree;
    vector<TokenType> closerStack;

    do {
        if (isAtEnd()) {
            throw error(previous(), "Unexpected end of file.");
        }

        // Update closers
        if (check({TokenType::LEFT_PAREN, TokenType::LEFT_BRACE, TokenType::LEFT_BRACKET})) { closerStack.push_back(peek().type); }
        if (check(TokenType::RIGHT_PAREN)) {
            if (closerStack.back() != TokenType::LEFT_PAREN) { throw error(peek(), "Unexpected ')' in token tree."); }
            closerStack.pop_back();
        }
        if (check(TokenType::RIGHT_BRACE)){
            if (closerStack.back() != TokenType::LEFT_BRACE) { throw error(peek(), "Unexpected '}' in token tree."); }
            closerStack.pop_back();
        }
        if (check(TokenType::RIGHT_BRACKET)){
            if (closerStack.back() != TokenType::LEFT_BRACKET) { throw error(peek(), "Unexpected ']' in token tree."); }
            closerStack.pop_back();
        }

        tokenTree.push_back(advance());
    } while (!closerStack.empty());

    return tokenTree;
}

void Parser::expandMacros()
{
    for (ASTNodePtr stmt : parsedUnit->stmts) {
        try {
            macroExpander->expand(stmt);
        }
        catch (ParserException& e) {}
    }
}

//syncs when we find a ';' or one of the keywords
void Parser::sync() {
	while (!isAtEnd()) {
		if (peek().type == TokenType::SEMICOLON){
            advance();
            return;
        }

		switch (peek().type) {
		case TokenType::CLASS:
		case TokenType::FN:
		case TokenType::LET:
		case TokenType::FOR:
		case TokenType::IF:
		case TokenType::ELSE:
		case TokenType::WHILE:
		case TokenType::RETURN:
		case TokenType::SWITCH:
		case TokenType::CASE:
		case TokenType::DEFAULT:
		case TokenType::RIGHT_BRACE:
        case TokenType::STATIC:
        case TokenType::PUB:
			return;
        default: break;
		}

		advance();
	}
}

template<typename ParseletType>
void Parser::addPrefix(TokenType type, Precedence prec) {
	prefixParselets[type] = std::make_unique<ParseletType>(this, +prec);
}

template<typename ParseletType>
void Parser::addInfix(TokenType type, Precedence prec) {
	infixParselets[type] = std::make_unique<ParseletType>(this, +prec);
}

//checks if the parserCurrent token has any infix parselet associated with it, and if so the precedence of that operation is returned
int Parser::getPrec() {
	Token token = peek();
	if (infixParselets.count(token.type) == 0) {
		return 0;
	}
	return infixParselets[token.type]->prec;
}
#pragma endregion
