#include <ranges>

#include "TypeInference.h"

using namespace types;

template<typename... Args>
requires (std::same_as<Args, tyPtr> && ...)
static tyPtr typeUnion(tyPtr first, Args... types) {
    tyPtr res = first;
    auto lam = [&](auto&& arg) {
        if (res->type == TypeFlag::UNKNOWN) res = arg;
        if (!types_equal(res, arg) && arg->type != TypeFlag::UNKNOWN) res = getBasicType(TypeFlag::ANY);
    };
    (lam(types), ...);
    return res;
}

template<typename... Args>
requires (std::same_as<Args, tyPtr> && ...)
static tyPtr last_evaluated(tyPtr first, Args... types) {
    tyPtr res = first;
    auto lam = [&](auto&& arg) {
        if (res->type == TypeFlag::UNKNOWN) res = arg;
        if (!types_equal(res, arg) && arg->type != TypeFlag::UNKNOWN) res = arg;
    };
    (lam(types), ...);
    return res;
}


class VariableTypeFinder : CFG::CFGVisitor{
    uint64_t _target = 0;
    tyPtr _target_type = nullptr;
    std::unordered_set<CFG::WhileStmt*> _loop_labels;

    void ret_target(tyPtr ty) {
        _target_type = ty;
    }
    tyPtr extract_type(const std::shared_ptr<CFG::CFGNode>& node) {
        node->accept(this);
        return _target_type;
    }
    tyPtr visit_antecedents(const CFG::CFGStmt* stmt) {
        tyPtr tmp = getBasicType(TypeFlag::UNKNOWN);
        for (const auto& antecedent : stmt->antecedents)
            tmp = typeUnion(tmp, extract_type(antecedent));
        return tmp;
    }
    bool is_target(std::shared_ptr<CFG::TypedExpr> expr) {
        if (expr->type != CFG::NodeType::VAR_READ) return false;
        return std::reinterpret_pointer_cast<CFG::VarRead>(expr)->varPtr->uuid == _target;
    }
    public:
    tyPtr run(uint64_t var, CFG::CFGStmt* cur_stmt) {
        _target = var;
        return visit_antecedents(cur_stmt);
    }
    // Returning UNKNOWN in any method here means we explored everything and found no constraints on the target
    // When there are multiple subexpressions to explore we use the last evaluated type
    // IMPORTANT: this means that the order in which we pass subexpressions to the last_evaluated function
    // must be the same order in which things happen
    void visitVarDecl(CFG::VarDecl* decl) override {
        if (decl->uuid == _target) return ret_target(getBasicType(TypeFlag::NIL));
        return ret_target(visit_antecedents(decl));
    }
    void visitVarRead(CFG::VarRead* expr) override {
        // TODO: maybe optimize if var read has type set? but we can get false positives with it
        return ret_target(getBasicType(TypeFlag::UNKNOWN));
    }
    void visitVarStore(CFG::VarStore* expr) override {
        if (expr->varPtr->uuid == _target) return ret_target(expr->toStore->exprType);
        return ret_target(extract_type(expr->toStore));
    }
    void visitVarReadNative(CFG::VarReadNative* expr) override {
        return ret_target(getBasicType(TypeFlag::UNKNOWN));
    }
    void visitArithmeticExpr(CFG::ArithmeticExpr* expr) override {
        if (expr->opType == CFG::ArithmeticOp::ADD) {
            // TODO: what if type of the other side is ANY? we can probably do better
            if (is_target(expr->lhs) && !is_target(expr->rhs)) return ret_target(expr->rhs->exprType);
            if (!is_target(expr->lhs) && is_target(expr->rhs)) return ret_target(expr->lhs->exprType);
        }
        // Every op except add is number | number
        if (is_target(expr->lhs) || is_target(expr->rhs)) {
            return ret_target(getBasicType(TypeFlag::NUMBER));
        }
        return ret_target(last_evaluated(extract_type(expr->lhs), extract_type(expr->rhs)));
    }
    void visitComparisonExpr(CFG::ComparisonExpr* expr) override {
        switch (expr->opType) {
            case CFG::ComparisonOp::LESS:
            case CFG::ComparisonOp::LESSEQ:
            case CFG::ComparisonOp::GREAT:
            case CFG::ComparisonOp::GREATEQ:
                if (is_target(expr->lhs) || is_target(expr->rhs)) return ret_target(getBasicType(TypeFlag::NUMBER));
                break;
            case CFG::ComparisonOp::EQUAL:
                // TODO: what if type of the other side is ANY? we can probably do better
                if (is_target(expr->lhs) && !is_target(expr->rhs)) return ret_target(expr->rhs->exprType);
                if (!is_target(expr->lhs) && is_target(expr->rhs)) return ret_target(expr->lhs->exprType);
            case CFG::ComparisonOp::NOT_EQUAL:
            case CFG::ComparisonOp::AND:
            case CFG::ComparisonOp::OR:
                break;// These ops don't constraint the type in any way since booleans are evaluated with isTruthy
        }
        return ret_target(last_evaluated(extract_type(expr->lhs), extract_type(expr->rhs)));
    }
    void visitInstanceofExpr(CFG::InstanceofExpr* expr) override {
        if (is_target(expr->lhs)) return ret_target(std::make_shared<InstanceType>(expr->classType));
        return ret_target(extract_type(expr->lhs));
    }
    void visitUnaryExpr(CFG::UnaryExpr* expr) override {
        if (is_target(expr->rhs)) {
            if (expr->opType == CFG::UnaryOp::NEG) return ret_target(getBasicType(TypeFlag::BOOL));
            return ret_target(getBasicType(TypeFlag::NUMBER));
        }
        return ret_target(extract_type(expr->rhs));
    }
    void visitLiteralExpr(CFG::LiteralExpr* expr) override {
        return ret_target(getBasicType(TypeFlag::UNKNOWN));
    }
    void visitHashmapExpr(CFG::HashmapExpr* expr) override {
        tyPtr tmp = getBasicType(TypeFlag::UNKNOWN);
        for (const auto& field : expr->fields | std::views::values)
            tmp = last_evaluated(tmp, extract_type(field));
        return ret_target(tmp);
    }
    void visitArrayExpr(CFG::ArrayExpr* expr) override {
        tyPtr tmp = getBasicType(TypeFlag::UNKNOWN);
        for (const auto& field : expr->fields)
            tmp = last_evaluated(tmp, extract_type(field));
        return ret_target(tmp);
    }
    void visitCollectionGet(CFG::CollectionGet* expr) override {
        // Implication of array[number] and hashmap[string] goes both ways
        if (is_target(expr->collection)) {
            if (expr->field->exprType->type == TypeFlag::NUMBER) return ret_target(getBasicType(TypeFlag::ARRAY));
            if (expr->field->exprType->type == TypeFlag::STRING) return ret_target(getBasicType(TypeFlag::HASHMAP));
        } else if (is_target(expr->field)) {
            if (expr->field->exprType->type == TypeFlag::ARRAY) return ret_target(getBasicType(TypeFlag::NUMBER));
            if (expr->field->exprType->type == TypeFlag::HASHMAP) return ret_target(getBasicType(TypeFlag::STRING));
        }
        return ret_target(last_evaluated(extract_type(expr->collection), extract_type(expr->field)));
    }
    void visitCollectionSet(CFG::CollectionSet* expr) override {
        // Implication of array[number] and hashmap[string] goes both ways
        if (is_target(expr->collection)) {
            if (expr->field->exprType->type == TypeFlag::NUMBER) return ret_target(getBasicType(TypeFlag::ARRAY));
            if (expr->field->exprType->type == TypeFlag::STRING) return ret_target(getBasicType(TypeFlag::HASHMAP));
        } else if (is_target(expr->field)) {
            if (expr->field->exprType->type == TypeFlag::ARRAY) return ret_target(getBasicType(TypeFlag::NUMBER));
            if (expr->field->exprType->type == TypeFlag::HASHMAP) return ret_target(getBasicType(TypeFlag::STRING));
        }
        return ret_target(last_evaluated(extract_type(expr->collection), extract_type(expr->field), extract_type(expr->toStore)));
    }
    void visitConditionalExpr(CFG::ConditionalExpr* expr) override {
        auto cond_ty = extract_type(expr->cond);

        return ret_target(typeUnion(last_evaluated(cond_ty, extract_type(expr->thenExpr)),
            last_evaluated(cond_ty, extract_type(expr->elseExpr))));
    }
    void visitCallExpr(CFG::CallExpr* expr) override {
        tyPtr tmp = extract_type(expr->callee);
        for (const auto& arg : expr->args) tmp = last_evaluated(tmp, extract_type(arg));
        return ret_target(tmp);
    }
    void visitInvokeExpr(CFG::InvokeExpr* expr) override {
        tyPtr tmp = extract_type(expr->inst);
        for (const auto& arg : expr->args) tmp = last_evaluated(tmp, extract_type(arg));
        return ret_target(tmp);
    }
    void visitNewExpr(CFG::NewExpr* expr) override {
        tyPtr tmp = getBasicType(TypeFlag::UNKNOWN);
        for (const auto& arg : expr->args) tmp = last_evaluated(tmp, extract_type(arg));
        return ret_target(tmp);
    }
    void visitSpawnStmt(CFG::SpawnStmt* stmt) override {
        if (auto tmp = extract_type(stmt->call); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        return ret_target(visit_antecedents(stmt));
    }
    void visitCreateClosureExpr(CFG::CreateClosureExpr* expr) override {
        return ret_target(getBasicType(TypeFlag::UNKNOWN));
    }
    void visitFuncDecl(CFG::FuncDecl* decl) override {
        // Should never be reached
    }
    void visitExprStmt(CFG::ExprStmt* stmt) override {
        if (auto tmp = extract_type(stmt->expr); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        return ret_target(visit_antecedents(stmt));
    }
    void visitReturnStmt(CFG::ReturnStmt* stmt) override {
        if (auto tmp = extract_type(stmt->expr); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        return ret_target(visit_antecedents(stmt));
    }
    void visitUncondJump(CFG::UncondJump* stmt) override {
        // Should never be reached
    }
    void visitIfStmt(CFG::IfStmt* stmt) override {
        if (auto tmp = extract_type(stmt->cond); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        return ret_target(visit_antecedents(stmt));
    }
    void visitWhileStmt(CFG::WhileStmt* stmt) override {
        // If we're passing through this label a second time we must have gotten here through some loop back edge
        // Since the first antecendents in the loop label are the predecessors of the loop we must have already explored them
        if (_loop_labels.contains(stmt)) return ret_target(getBasicType(TypeFlag::UNKNOWN));

        if (auto tmp = extract_type(stmt->cond); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        _loop_labels.insert(stmt);
        auto tmp = visit_antecedents(stmt);
        _loop_labels.erase(stmt);
        return ret_target(tmp);
    }
    void visitSwitchStmt(CFG::SwitchStmt* stmt) override {
        if (auto tmp = extract_type(stmt->cond); tmp->type != TypeFlag::UNKNOWN) {
            return ret_target(tmp);
        }
        return ret_target(visit_antecedents(stmt));
    }
    void visitClassDecl(CFG::ClassDecl* decl) override {
        // Should never be reached
    }
    void visitInstGet(CFG::InstGet* expr) override {
        return ret_target(extract_type(expr->instance));
    }
    void visitInstSet(CFG::InstSet* expr) override {
        return ret_target(last_evaluated(extract_type(expr->instance), extract_type(expr->toStore)));
    }
    void visitScopeBlock(CFG::ScopeEdge* stmt) override {
        // Should never be reached
    }
};

class TypePrinter : CFG::CFGVisitor{
    int indent = 0;
    string ty_to_str(tyPtr ty) {
        switch (ty->type) {
            case TypeFlag::NIL:
                return "nil";
            case TypeFlag::BOOL:
                return "bool";
            case TypeFlag::NUMBER:
                return "number";
            case TypeFlag::STRING:
                return "string";
            case TypeFlag::MUTEX:
                return "mutex";
            case TypeFlag::FILE:
                return "file";
            case TypeFlag::ANY:
                return "any";
            case TypeFlag::ARRAY:
                return "array";
            case TypeFlag::FUNCTION:
                return "function";
            case TypeFlag::HASHMAP:
                return "hashmap";
            case TypeFlag::INSTANCE:
                return "instance";
            case TypeFlag::CLASS:
                return "class";
            case TypeFlag::UNKNOWN:
                return "unknown";
        }
    }
    public:
    void run(std::shared_ptr<CFG::Function> fn) {
        for (auto stmt : fn->block.stmts) stmt->accept(this);
    }
    void visitVarDecl(CFG::VarDecl* decl) override {
        std::string s(" ", indent);
        std::cout<<s<<"var decl: "<<decl->dbgInfo.varName.getLexeme()<<std::endl;
    }
    void visitVarRead(CFG::VarRead* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"var read: "<<expr->dbgInfo.varName.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
    }
    void visitVarStore(CFG::VarStore* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"var store: "<<expr->dbgInfo.varName.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->toStore->accept(this);
    }
    void visitVarReadNative(CFG::VarReadNative* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"var read native: "<<expr->dbgInfo.varName.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
    }
    void visitArithmeticExpr(CFG::ArithmeticExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"arith expr: "<<expr->dbgInfo.op.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->lhs->accept(this);
        expr->rhs->accept(this);
    }
    void visitComparisonExpr(CFG::ComparisonExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"comp expr: "<<expr->dbgInfo.op.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->lhs->accept(this);
        expr->rhs->accept(this);
    }
    void visitInstanceofExpr(CFG::InstanceofExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"instanceof expr: "<<expr->dbgInfo.op.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->lhs->accept(this);
    }
    void visitUnaryExpr(CFG::UnaryExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"unary expr: "<<expr->dbgInfo.op.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->rhs->accept(this);
    }
    void visitLiteralExpr(CFG::LiteralExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"literal expr: "<<expr->dbgInfo.literal.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
    }
    void visitHashmapExpr(CFG::HashmapExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"hashmap expr: "<<expr->dbgInfo.brace1.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        for (auto [field, expr] : expr->fields) {
            std::cout<<s<<"field :"<<field<<"\n";
            expr->accept(this);
        }
    }
    void visitArrayExpr(CFG::ArrayExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"array expr: "<<expr->dbgInfo.bracket1.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        for (auto field : expr->fields) {
            field->accept(this);
        }
    }
    void visitCollectionGet(CFG::CollectionGet* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"collection get expr: "<<expr->dbgInfo.accessor.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->field->accept(this);
        expr->collection->accept(this);
    }
    void visitCollectionSet(CFG::CollectionSet* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"collection set expr: "<<expr->dbgInfo.accessor.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->field->accept(this);
        expr->collection->accept(this);
        expr->toStore->accept(this);
    }
    void visitConditionalExpr(CFG::ConditionalExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"conditional expr: "<<expr->dbgInfo.questionmark.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;
        expr->cond->accept(this);
        expr->thenExpr->accept(this);
        expr->elseExpr->accept(this);
    }
    void visitCallExpr(CFG::CallExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"call expr: "<<expr->dbgInfo.paren1.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        expr->callee->accept(this);
        for (auto field : expr->args) {
            field->accept(this);
        }
    }
    void visitInvokeExpr(CFG::InvokeExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"invoke expr: "<<expr->dbgInfo.method.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        expr->inst->accept(this);
        for (auto field : expr->args) {
            field->accept(this);
        }
    }
    void visitNewExpr(CFG::NewExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"new expr: "<<expr->dbgInfo.className.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        for (auto field : expr->args) {
            field->accept(this);
        }
    }
    void visitSpawnStmt(CFG::SpawnStmt* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"spawn stmt: "<<stmt->dbgInfo.keyword.getLexeme()<<std::endl;
        indent++;
        stmt->call->accept(this);
        indent--;
    }
    void visitCreateClosureExpr(CFG::CreateClosureExpr* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"closure expr: "<<expr->dbgInfo.keyword.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        indent += 2;
        for (auto stmt : expr->fn->block.stmts) stmt->accept(this);
        indent -= 2;
    }
    void visitFuncDecl(CFG::FuncDecl* decl) override {
        std::string s(" ", indent);
        std::cout<<s<<"func decl: "<<decl->dbgInfo.keyword.getLexeme()<<std::endl;

        indent += 2;
        for (auto stmt : decl->fn->block.stmts) stmt->accept(this);
        indent -= 2;
    }
    void visitExprStmt(CFG::ExprStmt* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"expr stmt: "<<std::endl;
        indent++;
        stmt->expr->accept(this);
        indent--;
    }
    void visitReturnStmt(CFG::ReturnStmt* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"return stmt: "<<std::endl;
        indent++;
        stmt->expr->accept(this);
        indent--;
    }
    void visitUncondJump(CFG::UncondJump* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"jump stmt: "<<std::endl;
    }
    void visitIfStmt(CFG::IfStmt* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"if stmt: "<<std::endl;
        indent++;
        stmt->cond->accept(this);
        for (auto stmt : stmt->thenBlock.stmts) stmt->accept(this);
        for (auto stmt : stmt->elseBlock.stmts) stmt->accept(this);
        indent--;
    }
    void visitWhileStmt(CFG::WhileStmt* stmt) override {
        std::string s(" ", indent);
        std::cout<<s<<"while stmt: "<<std::endl;
        indent++;
        stmt->cond->accept(this);
        for (auto stmt : stmt->loopBody.stmts) stmt->accept(this);
        stmt->afterLoopExpr->accept(this);
        indent--;
    }
    void visitSwitchStmt(CFG::SwitchStmt* stmt) override {

    }
    void visitClassDecl(CFG::ClassDecl* decl) override {

    }
    void visitInstGet(CFG::InstGet* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"instget expr: "<<expr->dbgInfo.field.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        expr->instance->accept(this);
    }
    void visitInstSet(CFG::InstSet* expr) override {
        std::string s(" ", indent);
        std::cout<<s<<"instset expr: "<<expr->dbgInfo.field.getLexeme()<<" ty: "<<ty_to_str(expr->exprType)<<std::endl;

        expr->instance->accept(this);
        expr->toStore->accept(this);
    }
    void visitScopeBlock(CFG::ScopeEdge* stmt) override {

    }
};

tyPtr TypeInferencePass::getVarType(std::shared_ptr<CFG::VarDecl> decl) {
    if (decl->varType == CFG::VarType::GLOBAL) return getBasicType(TypeFlag::ANY);
    if (decl->varType == CFG::VarType::GLOBAL_FUNC) return _function_types[decl->uuid];
    VariableTypeFinder varTypeFinder;
    auto tmp = varTypeFinder.run(decl->uuid, _cur_stmt);
    // If we couldn't constrain the type to anything we know take the most conservative approach
    if (tmp->type == TypeFlag::UNKNOWN) return getBasicType(TypeFlag::ANY);
    return tmp;
}

bool TypeInferencePass::func_complete(tyPtr func) {
    return func->type == TypeFlag::FUNCTION ? _processed_funcs.contains(std::reinterpret_pointer_cast<FunctionType>(func)) : false;
}

void TypeInferencePass::run(std::pair<std::shared_ptr<CFG::Function>, vector<File*>>& main_fn, bool should_print) {
    _cur_stmt = nullptr;
    TypePrinter p;
    for (const auto& stmt : main_fn.first->block.stmts) {
        stmt->accept(this);
        if (should_print) stmt->accept(reinterpret_cast<CFG::CFGVisitor *>(&p));
    }
}


void TypeInferencePass::visitVarDecl(CFG::VarDecl* decl) {
    _cur_stmt = decl;
}
void TypeInferencePass::visitVarRead(CFG::VarRead* expr) {
    expr->exprType = getVarType(expr->varPtr);
}
void TypeInferencePass::visitVarStore(CFG::VarStore* expr) {
    expr->toStore->accept(this);
    expr->exprType = expr->toStore->exprType;
}
void TypeInferencePass::visitVarReadNative(CFG::VarReadNative* expr) {
    // Expr type is set when transforming
    if (expr->exprType->type == TypeFlag::FUNCTION) _processed_funcs.insert(std::reinterpret_pointer_cast<FunctionType>(expr->exprType));
}
void TypeInferencePass::visitArithmeticExpr(CFG::ArithmeticExpr* expr) {
    expr->lhs->accept(this);
    expr->rhs->accept(this);
    switch (expr->opType) {
        case CFG::ArithmeticOp::ADD: {
            auto tmp = typeUnion(expr->lhs->exprType, expr->rhs->exprType);
            if (tmp->type == TypeFlag::NUMBER || tmp->type == TypeFlag::STRING) expr->exprType = tmp;
            break;
        }
        default:
            expr->exprType = getBasicType(TypeFlag::NUMBER); break;
    }
}
void TypeInferencePass::visitComparisonExpr(CFG::ComparisonExpr* expr) {
    expr->lhs->accept(this);
    expr->rhs->accept(this);
    expr->exprType = getBasicType(TypeFlag::BOOL);
}
void TypeInferencePass::visitInstanceofExpr(CFG::InstanceofExpr* expr) {
    expr->lhs->accept(this);
}
void TypeInferencePass::visitUnaryExpr(CFG::UnaryExpr* expr) {
    expr->rhs->accept(this);
    if (expr->opType == CFG::UnaryOp::NEG) expr->exprType = getBasicType(TypeFlag::BOOL);
    else expr->exprType = getBasicType(TypeFlag::NUMBER);
}
void TypeInferencePass::visitLiteralExpr(CFG::LiteralExpr* expr) {
    switch (expr->val.index()) {
        case 0: expr->exprType = getBasicType(TypeFlag::NUMBER); break;
        case 1: expr->exprType = getBasicType(TypeFlag::BOOL); break;
        case 2: expr->exprType = getBasicType(TypeFlag::NIL); break;
        case 3: expr->exprType = getBasicType(TypeFlag::STRING); break;
    }
}
void TypeInferencePass::visitHashmapExpr(CFG::HashmapExpr* expr) {
    for (auto &field: expr->fields | std::views::values) field->accept(this);
    expr->exprType = std::make_shared<HashMapType>(getBasicType(TypeFlag::ANY));
}
void TypeInferencePass::visitArrayExpr(CFG::ArrayExpr* expr) {
    for (auto& field : expr->fields)  field->accept(this);
    expr->exprType = std::make_shared<ArrayType>(getBasicType(TypeFlag::ANY));
}
void TypeInferencePass::visitCollectionGet(CFG::CollectionGet* expr) {
    expr->collection->accept(this);
    expr->field->accept(this);
}
void TypeInferencePass::visitCollectionSet(CFG::CollectionSet* expr) {
    expr->collection->accept(this);
    expr->field->accept(this);
    expr->toStore->accept(this);
    expr->exprType = expr->toStore->exprType;
}
void TypeInferencePass::visitConditionalExpr(CFG::ConditionalExpr* expr) {
    expr->cond->accept(this);
    expr->thenExpr->accept(this);
    expr->elseExpr->accept(this);
    expr->exprType = typeUnion(expr->thenExpr->exprType, expr->elseExpr->exprType);
}

void TypeInferencePass::visitCallExpr(CFG::CallExpr* expr) {
    expr->callee->accept(this);
    for (auto& arg : expr->args) arg->accept(this);

    // Determine the result of the call(if possible)
    // If this is a recursive call we can't know the return type for sure
    if (auto ty = expr->callee->exprType; ty->type == TypeFlag::FUNCTION && func_complete(ty)) {
        expr->exprType = std::reinterpret_pointer_cast<FunctionType>(ty)->retType;
    }
}

void TypeInferencePass::visitInstGet(CFG::InstGet* expr) {
    expr->instance->accept(this);
    if (expr->instance->exprType->type == TypeFlag::INSTANCE) {
        auto class_ty = std::reinterpret_pointer_cast<InstanceType>(expr->instance->exprType)->klass;
        if (class_ty->methods.contains(expr->field)) expr->exprType = class_ty->methods[expr->field].first;
        else if (class_ty->fields.contains(expr->field)) expr->exprType = class_ty->fields[expr->field].first;
    }
}
void TypeInferencePass::visitInstSet(CFG::InstSet* expr) {
    expr->instance->accept(this);
    expr->toStore->accept(this);
    expr->exprType = expr->toStore->exprType;
}
void TypeInferencePass::visitInvokeExpr(CFG::InvokeExpr* expr) {
    expr->inst->accept(this);
    for (const auto& arg : expr->args) arg->accept(this);
    if (expr->inst->exprType->type == TypeFlag::INSTANCE) {
        auto class_ty = std::reinterpret_pointer_cast<InstanceType>(expr->inst->exprType)->klass;
        tyPtr func_ty = nullptr;
        if (class_ty->methods.contains(expr->field)) func_ty = class_ty->methods[expr->field].first;
        else if (class_ty->fields.contains(expr->field)) func_ty = class_ty->fields[expr->field].first;

        if (func_ty->type == TypeFlag::FUNCTION && func_complete(func_ty))
            expr->exprType = std::reinterpret_pointer_cast<FunctionType>(func_ty)->retType;
    }
}
void TypeInferencePass::visitNewExpr(CFG::NewExpr* expr) {
    for (const auto& arg : expr->args) arg->accept(this);
    // No need to set exprType, its set in the transformer
}

void TypeInferencePass::visitSpawnStmt(CFG::SpawnStmt* stmt) {
    _cur_stmt = stmt;
    stmt->call->accept(this);
}

void TypeInferencePass::visitCreateClosureExpr(CFG::CreateClosureExpr* expr) {
    auto data = start_func(expr->fn->fnTy);
    for (auto stmt : expr->fn->block.stmts) stmt->accept(this);
    end_func(data);
}
void TypeInferencePass::visitFuncDecl(CFG::FuncDecl* decl) {
    auto data = start_func(decl->fn->fnTy);
    // To identify function types in global variables and optimize(since those globals will always be a function)
    _function_types[decl->globalVarUuid] = _cur_fn;

    for (auto stmt : decl->fn->block.stmts) stmt->accept(this);
    end_func(data);
}

void TypeInferencePass::visitExprStmt(CFG::ExprStmt* stmt) {
    _cur_stmt = stmt;
    stmt->expr->accept(this);
}
void TypeInferencePass::visitReturnStmt(CFG::ReturnStmt* stmt) {
    _cur_stmt = stmt;
    if (stmt->expr) {
        stmt->expr->accept(this);
        _cur_fn->retType = typeUnion(_cur_fn->retType, stmt->expr->exprType);
    } else {
        _cur_fn->retType = typeUnion(_cur_fn->retType, getBasicType(TypeFlag::NIL));
    }
}
void TypeInferencePass::visitUncondJump(CFG::UncondJump* stmt) {
    // Nothing to do
}
void TypeInferencePass::visitIfStmt(CFG::IfStmt* stmt) {
    _cur_stmt = stmt;
    stmt->cond->accept(this);
    for (const auto& stmt : stmt->thenBlock.stmts) stmt->accept(this);
    for (const auto& stmt : stmt->elseBlock.stmts) stmt->accept(this);
}
void TypeInferencePass::visitWhileStmt(CFG::WhileStmt* stmt) {
    _cur_stmt = stmt;
    // TODO: determine if doing this is really worth it
    for (int i = 0; i < 2; i++) {
        if (stmt->cond) stmt->cond->accept(this);
        for (const auto& stmt : stmt->loopBody.stmts) stmt->accept(this);
        if (stmt->afterLoopExpr) stmt->afterLoopExpr->accept(this);
    }
}
void TypeInferencePass::visitSwitchStmt(CFG::SwitchStmt* stmt) {
    _cur_stmt = stmt;
    stmt->cond->accept(this);
    for (const auto& _case : stmt->cases) {
        for (const auto& stmt : _case.stmts) stmt->accept(this);
    }
}

void TypeInferencePass::visitClassDecl(CFG::ClassDecl* decl) {
    for (const auto& method : decl->methods | std::views::values | std::views::keys) {
        auto data = start_func(method.code->fnTy);
        for (const auto& stmt : method.code->block.stmts) stmt->accept(this);
        end_func(data);
    }
}
void TypeInferencePass::visitScopeBlock(CFG::ScopeEdge* stmt) {
    // Nothing to do
}