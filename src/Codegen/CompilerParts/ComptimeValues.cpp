#include "ComptimeValues.h"
#include "../LLVMHelperFunctions.h"
#include "../../Runtime/Values/valueHelpers.h"

llvm::Constant* ComptimeValues::createESLString(const string& str){
    if(ESLStrings.contains(str)) return ESLStrings[str];
    auto obj = llvm::ConstantStruct::get(llvm::StructType::getTypeByName(_b.getContext(), "ObjString"), {
        createConstObjHeader(+object::ObjType::STRING), _b.getInt32(str.size())
    });
    obj = llvm::ConstantStruct::getAnon({obj, llvm::ConstantDataArray::getString(_b.getContext(), str)});
    const auto val = constObjToVal(storeConstObj(obj), +object::ObjType::STRING);
    ESLStrings[str] = val;
    return val;
}

llvm::Constant* ComptimeValues::createMethodObj(const std::string& name, uint8_t arity, llvm::Function* method_ptr){
    // Every function is converted to a closure(if even it has 0 freevars) for ease of use when calling
    llvm::Constant* closure =  llvm::ConstantStruct::get(llvm::StructType::getTypeByName(_b.getContext(), "ObjClosure"),{
        createConstObjHeader(+object::ObjType::CLOSURE),
        _b.getInt8(arity),
        _b.getInt8(0), // freevar count, methods can't capture surrounding context
        llvm::ConstantExpr::getBitCast(method_ptr, _b.getPtrTy()), // type erased function
        createConstStr(name)
    });
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(_b.getContext(), "ObjClosureAligned"),
    { closure, _b.getInt64(0) }
    );
}

// TODO: change this to call a gc specified function for header
// right now its hardcoded to move_state::unmanaged
llvm::Constant* ComptimeValues::createConstObjHeader(int type) const {
    return llvm::ConstantStruct::get(llvm::StructType::getTypeByName(_b.getContext(), "Obj"), _b.getInt8(4), _b.getInt8(type));
}

llvm::Constant* ComptimeValues::constObjToVal(llvm::Constant* obj, uint8_t type) const {
    auto val = llvm::ConstantExpr::getPtrToInt(obj, _b.getInt64Ty());
    return _tyhelper.ConstCastToESLVal(llvm::ConstantExpr::getAdd(val, _b.getInt64(mask_signature_obj | type)));
}

llvm::GlobalVariable* ComptimeValues::storeConstObj(llvm::Constant* obj) const {
    auto gv =  new llvm::GlobalVariable(*_b.GetInsertBlock()->getModule(), obj->getType(), true,
        llvm::GlobalVariable::LinkageTypes::PrivateLinkage, obj, "internal.const.obj"
    );
    gv->setAlignment(llvm::Align(16));
    return gv;
}

llvm::Constant* ComptimeValues::createConstStr(const string& str){
    if(CStrings.contains(str)) return CStrings[str];
    auto constant = _b.CreateGlobalString(str, "internal.string");

    CStrings[str] = constant;
    return constant;
}

// Returns i64 constant
llvm::Constant* ComptimeValues::createConstant(const std::variant<double, bool, void*,string>& constant){
    switch(constant.index()){
        case 0: {
            auto tmp = llvm::ConstantFP::get(_b.getContext(), llvm::APFloat(get<double>(constant)));
            return llvm::ConstantExpr::getBitCast(tmp, _b.getInt64Ty());
        }
        case 1: {
            uInt64 val = get<bool>(constant) ? mask_signature_true : mask_signature_false;
            return _b.getInt64(val);
        }
        case 2: return _b.getInt64(mask_signature_null);
        case 3: {
            return _tyhelper.ESLConstTo(createESLString(get<string>(constant)), _b.getInt64Ty());
        }
        default: break;
    }
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}
