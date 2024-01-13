#include "typeUnification.h"
#include "ASTToTypedAST.h"

using namespace typedAST;
using namespace passes::typeUnification;
TypeUnificator::TypeUnificator() {
    hadError = false;
}

vector<types::tyPtr> TypeUnificator::run(tyEnv& env){
    typeEnv = env;
    collapsedTypes.resize(typeEnv.size());
    initalPass();
    for(types::tyVarIdx idx = 0; idx < typeEnv.size(); idx++){
        collapsedTypes[idx] = collapseType(idx);
        // Only remove constraints when type is fully collapsed,
        // since collapseType can be called with the same idx in the middle of half collapsing another type
        typeEnv[idx].second.clear();
        if(collapsedTypes[idx] == nullptr){
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
            collapsedTypes[idx] = ty.first;
        }
    }
}

types::tyPtr TypeUnificator::collapseType(const types::tyVarIdx idx, shared_ptr<types::TypeConstraint> typeConstraint){
    constraintSet processed;
    // If this type is being collapsed by some constraint, add it to the constraint set beforehand to prevent recursion
    if(typeConstraint) processed.insert(typeConstraint);

    pair<types::tyPtr, vector<constraint>>& ty = typeEnv[idx];
    // This type is already collapsed, return held types
    if(ty.second.empty()) return collapsedTypes[idx];

    auto tmp = resolveConstraints(ty.second, processed);
    if(tmp == nullptr){
        std::cout<<"what";
    }
    if(ty.first && ty.first != tmp) return types::getBasicType(types::TypeFlag::ANY);
    else return tmp;
}

#define rp_cast std::reinterpret_pointer_cast

types::tyPtr TypeUnificator::resolveConstraints(vector<constraint> tyConstraints, constraintSet& processed){
    std::unordered_set<types::tyPtr> heldTys;
    while(!tyConstraints.empty()){
        auto c = tyConstraints.back();
        tyConstraints.pop_back();

        // Ignore already constrained types
        if(processed.contains(c)) continue;
        processed.insert(c);

        pair<types::tyPtr, vector<constraint>> constraintRes;

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
        if(constraintRes.first) heldTys.insert(constraintRes.first);
        for(auto newC : constraintRes.second){
            if(processed.contains(newC)) continue;
            tyConstraints.push_back(newC);
        }
    }
    if(heldTys.size() > 1) return types::getBasicType(types::TypeFlag::ANY);
    else {
        if(*heldTys.begin() == nullptr){
            std::cout<<"whaa";
        }
        return *heldTys.begin();
    }
}

pair<types::tyPtr, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::AddTyConstraint> addConstraint){
    auto ty = typeEnv[addConstraint->toAdd];
    // If the type is already collapsed return it
    if(ty.second.empty()) return std::make_pair(collapsedTypes[addConstraint->toAdd], vector<constraint>());
    // Sanity check, all typed variables(for which ty.first isn't null) don't have any constraints
    if(ty.first){
        auto t = ty.first;
        return std::make_pair(t, ty.second);
    }
    // Return all constraints, ty.first is null, so it can be ignored
    return std::make_pair(nullptr, ty.second);
}

pair<types::tyPtr, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint){
    types::tyPtr possibleCalleeTypes = collapseType(callConstraint->calleeType, callConstraint);

    // Gets the possible callee types, then filter which of these are functions and get their return type
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error

    // After getting all the possible types of the callee get the return types of every type that is a function
    return getPossibleRetTysFromFuncs(possibleCalleeTypes);
}
//TODO: rework this
pair<types::tyPtr, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint){
    types::tyPtr instTy = collapseType(instGetConstraint->potentialInst, instGetConstraint);

    // Gets the possible types of instTy, then filter which of those are instances
    // Can safely ignore possible type being e.g. an int because that will cause a runtime error
    pair<types::tyPtr, vector<constraint>> toReturn;
    if(instTy->type != types::TypeFlag::INSTANCE){
        toReturn.first = types::getBasicType(types::TypeFlag::ANY);
        return toReturn;
    }
    auto casted = std::reinterpret_pointer_cast<types::InstanceType>(instTy);
    auto klass = casted->klass;

    if(klass->methods.contains(instGetConstraint->field)){
        auto idx = klass->methods[instGetConstraint->field];
        // Methods are already collapsed
        toReturn.first = collapsedTypes[idx];
    }else if(klass->fields.contains(instGetConstraint->field)){
        // TODO: when fields become typed change this
        toReturn.first = collapsedTypes[static_cast<int>(types::TypeFlag::ANY)];
    }
    return toReturn;
}

pair<types::tyPtr, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint){
    types::tyPtr futTy = collapseType(awaitConstraint->potentialFuture, awaitConstraint);

    types::tyPtr possibleFunc = getPossibleFuncFromFut(awaitConstraint, futTy);
    // Once all the possible types that could be async called to create the future being awaited are known,
    // get the return types of the functions
    return getPossibleRetTysFromFuncs(possibleFunc);
}

types::tyPtr TypeUnificator::getPossibleFuncFromFut(shared_ptr<types::AwaitTyConstraint> awaitConstraint,
                                                      types::tyPtr possibleFutureType){
    if(possibleFutureType->type != types::TypeFlag::FUTURE){
        return types::getBasicType(types::TypeFlag::ANY);
    }

    auto futTy = std::reinterpret_pointer_cast<types::FutureType>(possibleFutureType);
    auto val = typeEnv[futTy->calleeType];

    // Get all possible types that are contained in the value being async called
    if(val.second.empty()) {
        auto collapsed = collapsedTypes[futTy->calleeType];
        return collapsed;
    }
    else{
        constraintSet processed;
        // Insert this constraint to prevent infinite recursion
        processed.insert(awaitConstraint);
        return resolveConstraints(val.second, processed);
    }
}

pair<types::tyPtr, vector<constraint>> TypeUnificator::getPossibleRetTysFromFuncs(types::tyPtr fnType){
    // Gets all possible return types from the list of possible functions
    // Ignore all types that aren't functions
    // types held in retTy of each function is not collapsed, but rather it's constraints are returned(as with the add constraint)
    pair<types::tyPtr, vector<constraint>> toReturn;
    if(fnType->type != types::TypeFlag::FUNCTION) {
        toReturn.first = types::getBasicType(types::TypeFlag::ANY);
        return toReturn;
    }
    auto fnTy = std::reinterpret_pointer_cast<types::FunctionType>(fnType);
    auto retTy = typeEnv[fnTy->retType];
    // For collapsed types
    if(retTy.second.empty()) {
        auto collapsed = collapsedTypes[fnTy->retType];
        toReturn.first = collapsed;
    }
    else{
        // For types that haven't been collapsed
        toReturn.second.insert(toReturn.second.end(), retTy.second.begin(), retTy.second.end());
    }
    return toReturn;
}

pair<types::tyPtr, vector<constraint>> TypeUnificator::processConstraint(shared_ptr<types::ComputeAddTysConstraint> computeAddConstraint){
    types::tyPtr possibleLhsTy = collapseType(computeAddConstraint->lhs, computeAddConstraint);
    types::tyPtr possibleRhsTy = collapseType(computeAddConstraint->rhs, computeAddConstraint);
    bool containsNums = possibleLhsTy->type == types::TypeFlag::NUMBER || possibleRhsTy->type == types::TypeFlag::NUMBER;
    bool containsStrings = possibleLhsTy->type == types::TypeFlag::STRING || possibleRhsTy->type == types::TypeFlag::STRING;
    vector<types::tyPtr> res;
    if(!(containsStrings ^ containsNums)) return make_pair(types::getBasicType(types::TypeFlag::ANY), vector<constraint>());
    if(containsNums) return make_pair(types::getBasicType(types::TypeFlag::NUMBER), vector<constraint>());
    if(containsStrings) return make_pair(types::getBasicType(types::TypeFlag::STRING), vector<constraint>());
}

#pragma endregion