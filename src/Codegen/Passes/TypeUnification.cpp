#include "TypeUnification.h"


using namespace typedAST;
using namespace passes::TypeUnification;
TypeUnificator::TypeUnificator() {
    hadError = false;
}

void TypeUnificator::run(typedAST::Function program, vector<File *> srcFiles){
    for(auto stmt : program.block.stmts){
        stmt->accept(this);
    }
}

void TypeUnificator::visitVarDecl(typedAST::VarDecl *decl){
    vector<types::tyPtr> collapsed;
    if(decl->typeConstrained){
        collapsed = collapseType(decl->constrainedValueType);
        collapsedTypes[decl->constrainedValueType] = collapsed;
    }else {
        collapsed = collapseType(decl->possibleTypes);
        collapsedTypes[decl->possibleTypes] = collapsed;
    }
}
void TypeUnificator::visitVarRead(typedAST::VarRead *expr){
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitVarStore(typedAST::VarStore *expr){
    expr->toStore->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitVarReadNative(typedAST::VarReadNative *expr){
    // Expr type is always singular
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitArithmeticExpr(typedAST::ArithmeticExpr *expr){
    expr->lhs->accept(this);
    expr->rhs->accept(this);
    // TODO: solve if lhs and rhs are numbers
    // Need to collapse because of possibility of a string concatenation
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitComparisonExpr(typedAST::ComparisonExpr *expr){
    expr->lhs->accept(this);
    expr->rhs->accept(this);
    // Don't need to collapse a bool type
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitUnaryExpr(typedAST::UnaryExpr *expr){
    expr->rhs->accept(this);
    // Don't need to collapse a number/bool type
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitLiteralExpr(typedAST::LiteralExpr *expr){
    // No need to do anything
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitHashmapExpr(typedAST::HashmapExpr *expr){
    for(auto field : expr->fields){
        field.second->accept(this);
    }
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitArrayExpr(typedAST::ArrayExpr *expr){
    for(auto field : expr->fields){
        field->accept(this);
    }
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitCollectionGet(typedAST::CollectionGet *expr){
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitCollectionSet(typedAST::CollectionSet *expr){
    expr->toStore->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitConditionalExpr(typedAST::ConditionalExpr *expr){
    expr->cond->accept(this);
    expr->thenExpr->accept(this);
    expr->elseExpr->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitCallExpr(typedAST::CallExpr *expr){
    expr->callee->accept(this);
    for(auto arg : expr->args){
        arg->accept(this);
    }
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitInvokeExpr(typedAST::InvokeExpr *expr){
    expr->inst->accept(this);
    for(auto arg : expr->args) arg->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitNewExpr(typedAST::NewExpr *expr){
    expr->callee->accept(this);
    for(auto arg : expr->args){
        arg->accept(this);
    }
    // Type of new expr is always an instance
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitAsyncExpr(typedAST::AsyncExpr *expr){
    expr->callee->accept(this);
    for(auto arg : expr->args){
        arg->accept(this);
    }
    // Type of async expr is always a future which can't be collapsed
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitAwaitExpr(typedAST::AwaitExpr *expr){
    expr->expr->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitCreateClosureExpr(typedAST::CreateClosureExpr *expr){
    for(auto arg : expr->fn.args){
        arg->accept(this);
    }
    // upval.first is guaranteed to be collapsed before evaluating this expr
    for(auto upval : expr->fn.upvals){
        collapsedTypes[upval.second->possibleTypes] = collapsedTypes[upval.first->possibleTypes];
    }
    // Solve the functions return type
    auto types = collapseType(expr->fn.fnType->retType);
    collapsedTypes[expr->fn.fnType->retType] = types;

    for(auto stmt : expr->fn.block.stmts){
        stmt->accept(this);
    }
    // Type of closure expr is always a func
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitFuncDecl(typedAST::FuncDecl *expr){
    for(auto arg : expr->fn.args){
        arg->accept(this);
    }
    auto types = collapseType(expr->fn.fnType->retType);
    collapsedTypes[expr->fn.fnType->retType] = types;
    for(auto stmt : expr->fn.block.stmts){
        stmt->accept(this);
    }
}
void TypeUnificator::visitReturnStmt(typedAST::ReturnStmt *stmt){
    stmt->expr->accept(this);
}
void TypeUnificator::visitRangeExpr(typedAST::RangeExpr *expr){
    expr->lhs->accept(this);
    expr->rhs->accept(this);
    collapsedTypes[expr->exprType] = {expr->exprType};
}
void TypeUnificator::visitUncondJump(typedAST::UncondJump *stmt){
    // Do nothing
}
void TypeUnificator::visitIfStmt(typedAST::IfStmt *stmt){
    stmt->cond->accept(this);
    for(auto s : stmt->thenBlock.stmts){
        s->accept(this);
    }
    for(auto s : stmt->elseBlock.stmts){
        s->accept(this);
    }
}
void TypeUnificator::visitWhileStmt(typedAST::WhileStmt *stmt){
    stmt->cond->accept(this);
    for(auto s : stmt->loopBody.stmts){
        s->accept(this);
    }
    stmt->afterLoopExpr->accept(this);
}
void TypeUnificator::visitSwitchStmt(typedAST::SwitchStmt *stmt){
    stmt->cond->accept(this);
    for(auto _case : stmt->cases){
        for(auto s : _case.stmts){
            s->accept(this);
        }
    }
}
void TypeUnificator::visitClassDecl(typedAST::ClassDecl *stmt){

}
void TypeUnificator::visitInstGet(typedAST::InstGet *expr){
    expr->instance->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitInstSuperGet(typedAST::InstSuperGet *expr){
    expr->instance->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}
void TypeUnificator::visitInstSet(typedAST::InstSet *expr){
    expr->toStore->accept(this);
    expr->instance->accept(this);
    auto types = collapseType(expr->exprType);
    collapsedTypes[expr->exprType] = types;
}

#pragma region Helpers
vector<types::tyPtr> TypeUnificator::collapseType(types::tyPtr toCollapse){

}

void TypeUnificator::collapseFunctionTy(std::shared_ptr<types::FunctionType> fnTy){

}
void TypeUnificator::collapseClassTy(std::shared_ptr<types::ClassType> classTy){

}
void TypeUnificator::collapseUnionTy(std::shared_ptr<types::TypeUnion> typeUnion){

}
void TypeUnificator::collapseDeferredCall(std::shared_ptr<types::CallReturnDeferred> deferredCall){

}
void TypeUnificator::collapseDeferredInstGet(std::shared_ptr<types::InstanceGetDeferred> deferredInstGet){

}
void TypeUnificator::collapseDeferredAwait(std::shared_ptr<types::FutureAwaitDeferred> deferredAwait){

}

void TypeUnificator::error(Token token, string msg){

}

#pragma endregion