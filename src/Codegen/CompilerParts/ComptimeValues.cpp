#include "ComptimeValues.h"
#include "../../Runtime/Values/valueHelpers.h"

llvm::Constant* ComptimeValues::createESLString(const string& str){
    if(ESLStrings.contains(str)) return ESLStrings[str];
    const auto val = constObjToVal(storeConstObj(RtString::const_build(_bridge, str)), RtString::tag);
    ESLStrings[str] = val;
    return val;
}

llvm::Constant* ComptimeValues::createMethodObj(const std::string& name, uint8_t arity, llvm::Function* method_ptr){
    // Every function is converted to a closure(if even it has 0 freevars) for ease of use when calling.
    // Methods can't capture surrounding context, so the env is always empty.
    return RtClosure::const_build_aligned(_bridge, arity, 0,
        llvm::ConstantExpr::getBitCast(method_ptr, _b.getPtrTy()), createConstStr(name)
    );
}

llvm::Constant* ComptimeValues::constObjToVal(llvm::Constant* obj, object::rt_type type) const {
    auto val = llvm::ConstantExpr::getPtrToInt(obj, _b.getInt64Ty());
    return _tyhelper.ConstCastToESLVal(llvm::ConstantExpr::getAdd(val, _b.getInt64(mask_signature_obj | +type)));
}

llvm::GlobalVariable* ComptimeValues::storeConstObj(llvm::Constant* obj) const {
    auto gv =  new llvm::GlobalVariable(*_b.GetInsertBlock()->getModule(), obj->getType(), true,
        llvm::GlobalVariable::LinkageTypes::PrivateLinkage, obj, "internal.const.obj"
    );
    // 16 because constObjToVal tags the address, and the tag occupies the low 4 bits
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
            return llvm::ConstantExpr::getBitCast(
                llvm::ConstantFP::get(
                    _b.getContext(), llvm::APFloat(get<double>(constant))
                ),
                _b.getInt64Ty()
            );
        }
        case 1: {
            return _b.getInt64(
                get<bool>(constant) ? mask_signature_true : mask_signature_false
            );
        }
        case 2: return _b.getInt64(mask_signature_null);
        case 3: {
            return _tyhelper.ESLConstTo(
                createESLString(get<string>(constant)), _b.getInt64Ty()
            );
        }
        default: break;
    }
    _e.reportUnrecoverableError("Unreachable code reached during compilation.");
    __builtin_unreachable();
}
