#include "CollapseTypes.h"


using namespace typedAST;
using namespace passes::TypeCollapser;
TypeCollapser::TypeCollapser() {
    hadError = false;
}

void TypeCollapser::run(typedAST::Function program, vector<File *> srcFiles){

}

void TypeCollapser::visitVarDecl(typedAST::VarDecl *decl){

}
void TypeCollapser::visitVarRead(typedAST::VarRead *expr){

}
void TypeCollapser::visitVarStore(typedAST::VarStore *expr){

}
void TypeCollapser::visitVarReadNative(typedAST::VarReadNative *expr){

}
void TypeCollapser::visitArithmeticExpr(typedAST::ArithmeticExpr *expr){

}
void TypeCollapser::visitComparisonExpr(typedAST::ComparisonExpr *expr){

}
void TypeCollapser::visitUnaryExpr(typedAST::UnaryExpr *expr){

}
void TypeCollapser::visitLiteralExpr(typedAST::LiteralExpr *expr){

}
void TypeCollapser::visitHashmapExpr(typedAST::HashmapExpr *expr){

}
void TypeCollapser::visitArrayExpr(typedAST::ArrayExpr *expr){

}
void TypeCollapser::visitCollectionGet(typedAST::CollectionGet *expr){

}
void TypeCollapser::visitCollectionSet(typedAST::CollectionSet *expr){

}
void TypeCollapser::visitConditionalExpr(typedAST::ConditionalExpr *expr){

}
void TypeCollapser::visitCallExpr(typedAST::CallExpr *expr){

}
void TypeCollapser::visitInvokeExpr(typedAST::InvokeExpr *expr){

}
void TypeCollapser::visitNewExpr(typedAST::NewExpr *expr){

}
void TypeCollapser::visitAsyncExpr(typedAST::AsyncExpr *expr){

}
void TypeCollapser::visitAwaitExpr(typedAST::AwaitExpr *expr){

}
void TypeCollapser::visitCreateClosureExpr(typedAST::CreateClosureExpr *expr){

}
void TypeCollapser::visitFuncDecl(typedAST::FuncDecl *expr){

}
void TypeCollapser::visitReturnStmt(typedAST::ReturnStmt *expr){

}
void TypeCollapser::visitRangeExpr(typedAST::RangeExpr *expr){

}
void TypeCollapser::visitUncondJump(typedAST::UncondJump *expr){

}
void TypeCollapser::visitIfStmt(typedAST::IfStmt *expr){

}
void TypeCollapser::visitWhileStmt(typedAST::WhileStmt *expr){

}
void TypeCollapser::visitSwitchStmt(typedAST::SwitchStmt *expr){

}
void TypeCollapser::visitClassDecl(typedAST::ClassDecl *expr){

}
void TypeCollapser::visitInstGet(typedAST::InstGet *expr){

}
void TypeCollapser::visitInstSuperGet(typedAST::InstSuperGet *expr){

}
void TypeCollapser::visitInstSet(typedAST::InstSet *expr){

}

#pragma region Helpers
void TypeCollapser::collapseType(types::tyPtr toCollapse){

}

// Collapses all types that aren't considered opaque
std::unordered_set<types::tyPtr> TypeCollapser::shallowCollapse(types::tyPtr toCollapse){

}

void TypeCollapser::collapseFunctionTy(std::shared_ptr<types::FunctionType> fnTy){

}
void TypeCollapser::collapseClassTy(std::shared_ptr<types::ClassType> classTy){

}
void TypeCollapser::collapseFutureTy(std::shared_ptr<types::FutureType> futureTy){

}
void TypeCollapser::collapseUnionTy(std::shared_ptr<types::TypeUnion> typeUnion){

}
void TypeCollapser::collapseDeferredCall(std::shared_ptr<types::CallReturnDeferred> deferredCall){

}
void TypeCollapser::collapseDeferredInstGet(std::shared_ptr<types::InstanceGetDeferred> deferredInstGet){

}
void TypeCollapser::collapseDeferredAwait(std::shared_ptr<types::FutureAwaitDeferred> deferredAwait){

}

void TypeCollapser::error(Token token, string msg){

}

#pragma endregion