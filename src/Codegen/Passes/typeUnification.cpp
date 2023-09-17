#include "typeUnification.h"


using namespace typedAST;
using namespace passes::typeUnification;
TypeUnificator::TypeUnificator() {
    hadError = false;
}

vector<vector<types::tyPtr>> TypeUnificator::run(tyEnv& env){
    typeEnv = env;
    collapsedTypes.resize(typeEnv.size());
    initalPass();
    for(types::tyVarIdx idx = 0; idx < typeEnv.size(); idx++){
        collapsedTypes[idx] = collapseType(idx);
        if(collapsedTypes[idx].empty()){
            // TODO: do something when set is empty(maybe a separate pass?)
            std::cout<<"Empty set at index: " << idx << "\n";
        }
    }
    return collapsedTypes;
}
#pragma region Helpers
// All collapsed types are put into the collapsedTypes env
void TypeUnificator::initalPass(){
    for(types::tyVarIdx idx = 0; idx < typeEnv.size(); idx++){
        auto ty = typeEnv[idx];

        if(ty.second.empty()){
            if(ty.first == nullptr){
                // TODO: error
                std::cout<<"Type is neither constrained to a single type nor has any constraints.\n";
                exit(64);
            }
            collapsedTypes[idx] = {ty.first};
        }
    }
}

vector<types::tyPtr> TypeUnificator::collapseType(const types::tyVarIdx idx, shared_ptr<types::TypeConstraint> typeConstraint){
    constraintSet processed;
    // If this type is being collapsed by some constraint, add it to the constraint set beforehand to prevent recursion
    if(typeConstraint) processed.insert(typeConstraint);

    pair<types::tyPtr, vector<constraint>>& ty = typeEnv[idx];
    // This type is already collapsed, return held types
    if(ty.second.empty()) return collapsedTypes[idx];

    // Process constraints of the type, erasing constraints in process, and return the possible types according to constraints
    vector<types::tyPtr> heldTys;
    if(ty.first) heldTys.push_back(ty.first);
    auto tmp = resolveConstraints(ty.second, processed);
    // After collapsing this type shouldn't have any constraints
    ty.second.clear();
    heldTys.insert(heldTys.end(), tmp.begin(), tmp.end());
    return heldTys;
}

#define rp_cast std::reinterpret_pointer_cast

vector<types::tyPtr> TypeUnificator::resolveConstraints(vector<constraint>& tyConstraints, constraintSet& processed){
    std::unordered_set<types::tyPtr> heldTys;
    while(!tyConstraints.empty()){
        auto c = tyConstraints.back();
        tyConstraints.pop_back();

        // Ignore already constrained types
        if(processed.contains(c)) continue;
        processed.insert(c);

        pair<vector<types::tyPtr>, vector<constraint>> constraintRes;

        switch(c->type){
            case types::TypeConstraintFlag::ADD_TY: {
                constraintRes = processConstraint(rp_cast<types::AddTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::GET_RETURN_TY:{
                constraintRes = processConstraint(rp_cast<types::CallResTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::GET_AWAIT_TY:{
                constraintRes = processConstraint(rp_cast<types::AwaitTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::INST_GET_FIELD_TY:{
                constraintRes = processConstraint(rp_cast<types::InstGetFieldTyConstraint>(c));
                break;
            }
            case types::TypeConstraintFlag::COMPUTE_ADD_TYS:
                constraintRes = processConstraint(rp_cast<types::ComputeAddTysConstraint>(c));
                break;
        }
        for(auto ty : constraintRes.first){
            heldTys.insert(ty);
        }
        for(auto newC : constraintRes.second){
            if(processed.contains(newC)) continue;
            tyConstraints.push_back(newC);
        }
    }
    return vector<types::tyPtr>(heldTys.begin(), heldTys.end());
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
    // Return all constraints, ty.first is null, so it can be ignored
    return std::make_pair(vector<types::tyPtr>(), ty.second);
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint){
    vector<types::tyPtr> possibleCalleeTypes = collapseType(callConstraint->calleeType, callConstraint);

    // Gets the possible callee types, then filter which of these are functions and get their return type
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error

    // After getting all the possible types of the callee get the return types of every type that is a function
    return getPossibleRetTysFromFuncs(possibleCalleeTypes);
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint){
    vector<types::tyPtr> possibleInstTypes = collapseType(instGetConstraint->potentialInst, instGetConstraint);

    // Gets the possible types of instTy, then filter which of those are instances
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error
    pair<vector<types::tyPtr>, vector<constraint>> toReturn;
    for(auto inst : possibleInstTypes){
        if(inst->type != types::TypeFlag::INSTANCE) continue;

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
    return toReturn;
}

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint){
    vector<types::tyPtr> possibleFutureTypes = collapseType(awaitConstraint->potentialFuture, awaitConstraint);

    vector<types::tyPtr> possibleFuncs = getPossibleFuncsFromFuts(awaitConstraint, possibleFutureTypes);
    // Once all the possible types that could be async called to create the future being awaited are known,
    // get the return types of the functions
    return getPossibleRetTysFromFuncs(possibleFuncs);
}

vector<types::tyPtr> TypeUnificator::getPossibleFuncsFromFuts(shared_ptr<types::AwaitTyConstraint> awaitConstraint,
                                                              vector<types::tyPtr>& possibleFutureTypes){
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

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::getPossibleRetTysFromFuncs(vector<types::tyPtr>& possibleFuncTypes){
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

pair<vector<types::tyPtr>, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::ComputeAddTysConstraint> computeAddConstraint){
    vector<types::tyPtr> possibleLhsTys = collapseType(computeAddConstraint->lhs, computeAddConstraint);
    vector<types::tyPtr> possibleRhsTys = collapseType(computeAddConstraint->rhs, computeAddConstraint);
    bool containsNums = false;
    bool containsStrings = false;
    for(auto ty : possibleLhsTys){
        if(ty->type == types::TypeFlag::NUMBER) containsNums = true;
        else if(ty->type == types::TypeFlag::STRING) containsStrings = true;
    }
    for(auto ty : possibleRhsTys){
        if(ty->type == types::TypeFlag::NUMBER) containsNums = true;
        else if(ty->type == types::TypeFlag::STRING) containsStrings = true;
    }
    vector<types::tyPtr> res;
    if(containsNums) res.push_back(types::getBasicType(types::TypeFlag::NUMBER));
    if(containsStrings) res.push_back(types::getBasicType(types::TypeFlag::STRING));
    if(!(containsStrings || containsNums)){
        res.push_back(types::getBasicType(types::TypeFlag::NUMBER));
        res.push_back(types::getBasicType(types::TypeFlag::STRING));
    }
    return std::make_pair(res, vector<constraint>());
}

#pragma endregion