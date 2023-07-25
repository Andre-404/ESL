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

    // Process constraints of the type, erasing constraints in process, and return the possible types according to constraints
    vector<types::tyPtr> heldTys;
    if(ty.first) heldTys.push_back(ty.first);
    constraintSet processed;
    auto tmp = resolveConstraints(ty.second, processed);
    // Collapsed types don't have any constraints
    ty.second.clear();
    heldTys.insert(heldTys.end(), tmp.begin(), tmp.end());
    return heldTys;
}

vector<types::tyPtr> TypeUnificator::resolveConstraints(vector<constraint> tyConstraints, constraintSet& processed){
    vector<types::tyPtr> heldTys;
    while(!tyConstraints.empty()){
        auto c = tyConstraints.back();
        tyConstraints.pop_back();

        // Ignore already constrained types
        if(processed.contains(c)) continue;
        processed.insert(c);

        pair<vector<types::tyPtr>, vector<constraint>> constraintRes;

        switch(c->type){
            case types::TypeConstraintFlag::ADD_TY: {
                constraintRes = processConstraint(std::reinterpret_pointer_cast<types::AddTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::GET_RETURN_TY:{
                constraintRes = processConstraint(std::reinterpret_pointer_cast<types::CallResTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::GET_AWAIT_TY:{
                constraintRes = processConstraint(std::reinterpret_pointer_cast<types::AwaitTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::INST_GET_FIELD_TY:{
                constraintRes = processConstraint(std::reinterpret_pointer_cast<types::InstGetFieldTyConstraint>(c));
                break;
            }
        }

        heldTys.insert(heldTys.end(), constraintRes.first.begin(), constraintRes.first.end());
        for(auto newC : constraintRes.second){
            if(processed.contains(newC)) continue;
            tyConstraints.push_back(newC);
        }
    }
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::AddTyConstraint> addConstraint){
    auto ty = typeEnv[addConstraint->toAdd];
    // If the type is already collapsed return it
    if(ty.second.empty()) return std::make_pair(collapsedTypes[addConstraint->toAdd], vector<constraint>());
    // Sanity check, all typed variables(for which ty.first isn't null) don't have any constraints
    if(ty.first){
        auto t = {ty.first};
        return std::make_pair(t, ty.second);
    }
    // Return all constraints, ty.first is null so it can be ignored
    return std::make_pair(vector<types::tyPtr>(), ty.second);
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint){
    auto calleeTy = typeEnv[callConstraint->calleeType];
    vector<types::tyPtr> possibleCalleeTypes;

    // Gets the possible callee types, then filter which of these are functions and get their return type
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error
    if(calleeTy.second.empty()) possibleCalleeTypes = collapsedTypes[callConstraint->calleeType];
    else{
        constraintSet processed;
        // Insert this constraint to prevent infinite recursion
        processed.insert(callConstraint);
        possibleCalleeTypes = resolveConstraints(calleeTy.second, processed);
    }
    // After getting all the possible types of the callee get the return types of every type that is a function
    return getPossibleRetTysFromFuncs(possibleCalleeTypes);
}
pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint){
    auto instTy = typeEnv[instGetConstraint->potentialInst];
    vector<types::tyPtr> possibleInstTypes;

    // Gets the possible instance types, then filter which of these are instances
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error
    if(instTy.second.empty()) possibleInstTypes = collapsedTypes[instGetConstraint->potentialInst];
    else{
        constraintSet processed;
        // Insert this constraint to prevent infinite recursion
        processed.insert(instGetConstraint);
        possibleInstTypes = resolveConstraints(instTy.second, processed);
    }

    pair<vector<types::tyPtr>, vector<constraint>> toReturn;
    for(auto inst : possibleInstTypes){
        if(inst->type != types::TypeFlag::INSTANCE){
            auto casted = std::reinterpret_pointer_cast<types::InstanceType>(inst);
            auto klass = casted->klass;
            
            if(klass->methods.contains(instGetConstraint->field)){
                auto idx = klass->methods[instGetConstraint->field];
                // Methods are already collapsed so just get the type at index 0
                toReturn.first.push_back(collapsedTypes[idx][0]);
            }else if(klass->fields.contains(instGetConstraint->field)){
                // Exit early because type is ANY
                // TODO: when fields become typed change this
                toReturn.first = {collapsedTypes[static_cast<int>(types::TypeFlag::ANY)]};
                toReturn.second.clear();
                return toReturn;
            }
        }
    }
    return toReturn;
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint){
    auto calleeTy = typeEnv[awaitConstraint->potentialFuture];
    vector<types::tyPtr> possibleFutureTypes;

    // Gets the possible types of the value being awaited
    if(calleeTy.second.empty()) possibleFutureTypes = collapsedTypes[awaitConstraint->potentialFuture];
    else{
        constraintSet processed;
        // Insert this constraint to prevent infinite recursion
        processed.insert(awaitConstraint);
        possibleFutureTypes = resolveConstraints(calleeTy.second, processed);
    }

    vector<types::tyPtr> possibleFuncs = getPossibleFuncsFromFuts(awaitConstraint, possibleFutureTypes);
    // Once all the possible types that could be async called to create the future being awaited are known,
    // get the return types of the functions
    return getPossibleRetTysFromFuncs(possibleFuncs);
}

vector<types::tyPtr> TypeUnificator::getPossibleFuncsFromFuts(shared_ptr<types::AwaitTyConstraint> awaitConstraint, vector<types::tyPtr> possibleFutureTypes){
    vector<types::tyPtr> possibleFuncs;
    // Gets all possible types that could have been called to create the future being awaited,
    // this is done because a future can be created by async calling a variable which holds a function
    for(auto ty : possibleFutureTypes){
        if(ty->type != types::TypeFlag::FUTURE) continue;

        auto futTy = std::reinterpret_pointer_cast<types::FutureType>(ty);
        auto val = typeEnv[futTy->calleeType];

        // Get all possible types that are contained in the value being async called
        if(val.second.empty()) {
            auto collapsed = collapsedTypes[futTy->calleeType];
            possibleFuncs.insert(possibleFuncs.end(), collapsed.begin(), collapsed.end());
        }
        else{
            constraintSet processed;
            // Insert this constraint to prevent infinite recursion
            processed.insert(awaitConstraint);
            auto possibleFuncTypes = resolveConstraints(val.second, processed);
            possibleFuncs.insert(possibleFuncs.end(), possibleFuncTypes.begin(), possibleFuncTypes.end());
        }
    }
    return possibleFuncs;
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::getPossibleRetTysFromFuncs(vector<types::tyPtr> possibleFuncTypes){
    // Gets all possible return types from the list of possible functions
    // Ignore all types that aren't functions
    // types held in retTy of each function is not collapsed, but rather it's constraints are returned(as with the add constraint)
    pair<vector<types::tyPtr>, vector<constraint>> toReturn;
    for(auto ty : possibleFuncTypes){
        if(ty->type != types::TypeFlag::FUNCTION) continue;

        auto fnTy = std::reinterpret_pointer_cast<types::FunctionType>(ty);
        auto retTy = typeEnv[fnTy->retType];
        // For collapsed types
        if(retTy.second.empty()) {
            auto collapsed = collapsedTypes[fnTy->retType];
            toReturn.first.insert(toReturn.first.end(), collapsed.begin(), collapsed.end());
        }
        else{
            // For types that haven't been collapsed
            toReturn.second.insert(toReturn.second.end(), retTy.second.begin(), retTy.second.end());
        }
    }
    return toReturn;
}

void TypeUnificator::error(Token token, string msg){

}

#pragma endregion