#include "Types.h"

using namespace types;

std::unordered_map<TypeFlag, std::shared_ptr<Type>> basicTypes;

// Ensures all basic types are uniqued to avoid duplication, and makes
tyPtr types::getBasicType(const TypeFlag type){
    if(basicTypes.contains(type)) return basicTypes.at(type);
    auto typtr = std::make_shared<Type>(type);
    basicTypes.insert_or_assign(type, typtr);
    return typtr;
}
