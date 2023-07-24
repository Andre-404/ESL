#include "TypeUnification.h"


using namespace typedAST;
using namespace passes::TypeUnification;
TypeUnificator::TypeUnificator() {
    hadError = false;
}

void TypeUnificator::run(tyEnv env){
    typeEnv = env;
    initalPass();
    for(types::tyVarIdx idx = 0; idx < typeEnv.size(); idx++){
        auto ty = typeEnv[idx];
        collapsedTypes[idx] = collapseType(idx, ty);
    }

}
#pragma region Helpers
// All collapsed types are put into the collapsedTypes env
void TypeUnificator::initalPass(){
    for(types::tyVarIdx idx = 0; idx < typeEnv.size(); idx++){
        auto ty = typeEnv[idx];

        if(ty.second.empty()){
            if(ty.first == nullptr){
                // TODO: error
                exit(64);
            }
            collapsedTypes[idx] = {ty.first};
        }
    }
}
vector<types::tyPtr> TypeUnificator::collapseType(types::tyVarIdx idx, std::pair<types::tyPtr, vector<constraint>>& ty){
    // This type is already collapsed, return held types
    if(ty.second.empty()) return collapsedTypes[idx];

    vector<types::tyPtr> heldTys;
    if(ty.first) heldTys.push_back(ty.first);

    auto tmp = resolveConstraints(ty.second);
    heldTys.insert(heldTys.end(), tmp.begin(), tmp.end());
    return heldTys;
}

vector<types::tyPtr> TypeUnificator::resolveConstraints(vector<constraint> tyConstraints){
    ankerl::unordered_dense::set<constraint> processed;
    vector<types::tyPtr> heldTys;
    while(!tyConstraints.empty()){
        auto c = tyConstraints.back();
        tyConstraints.pop_back();

        if(processed.contains(c)) continue;
        processed.insert(c);
        vector<types::tyPtr> tys;
        vector<constraint> constraints;

        switch(c->type){
            case types::TypeConstraintFlag::ADD_TY: {
                auto tmp = processConstraint(std::reinterpret_pointer_cast<types::AddTyConstraint>(c), processed);
                tys = tmp.first;
                constraints = tmp.second;
                break;
            }
            case types::TypeConstraintFlag::GET_RETURN_TY:{
                auto tmp = processConstraint(std::reinterpret_pointer_cast<types::CallResTyConstraint>(c), processed);
                tys = tmp.first;
                constraints = tmp.second;
                break;
            }
            case types::TypeConstraintFlag::GET_AWAIT_TY:{
                auto tmp = processConstraint(std::reinterpret_pointer_cast<types::AwaitTyConstraint>(c), processed);
                tys = tmp.first;
                constraints = tmp.second;
                break;
            }
            case types::TypeConstraintFlag::INST_GET_FIELD_TY:{
                auto tmp = processConstraint(std::reinterpret_pointer_cast<types::InstGetFieldTyConstraint>(c), processed);
                tys = tmp.first;
                constraints = tmp.second;
                break;
            }
        }

        heldTys.insert(heldTys.end(), tys.begin(), tys.end());
        for(auto newC : constraints){
            if(processed.contains(newC)) continue;
            tyConstraints.push_back(newC);
        }
    }
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(std::shared_ptr<types::AddTyConstraint> addConstraint, ankerl::unordered_dense::set<constraint>& processed){
    auto ty = typeEnv[addConstraint->toAdd];
    if(ty.second.empty()) return std::make_pair(collapsedTypes[addConstraint->toAdd], vector<constraint>());
    if(ty.first){
        auto t = {ty.first};
        return std::make_pair(t, ty.second);
    }
    return std::make_pair(vector<types::tyPtr>(), ty.second);
}
pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(std::shared_ptr<types::CallResTyConstraint> callConstraint, ankerl::unordered_dense::set<constraint>& processed){

}
pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(std::shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint, ankerl::unordered_dense::set<constraint>& processed){

}
pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(std::shared_ptr<types::AwaitTyConstraint> awaitConstraint, ankerl::unordered_dense::set<constraint>& processed){

}

void TypeUnificator::error(Token token, string msg){

}

#pragma endregion