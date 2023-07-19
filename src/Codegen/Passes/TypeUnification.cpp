#include "TypeUnification.h"


using namespace typedAST;
using namespace passes::TypeUnification;
TypeUnificator::TypeUnificator() {
    hadError = false;
}

void TypeUnificator::run(vector<std::pair<types::tyPtr, vector<constraint>>> typeEnv){
    for(auto ty : typeEnv){

    }
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