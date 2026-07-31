#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../Includes/unorderedDense.h"
#include "../../ErrorHandling/errorHandler.h"
#include "TypeHelper.h"

#include <ranges>
#include <variant>


class ComptimeValues {
    template<class T, class K>
    using ankerl_map = ankerl::unordered_dense::map<T, K>;

    llvm::IRBuilder<>& _b;
    errorHandler::ErrorHandler& _e;
    TypeHelper& _tyhelper;
    runtime_bridge& _bridge;

    ankerl_map<string, llvm::Constant*> CStrings;
    ankerl_map<string, llvm::Constant*> ESLStrings;

    public:
    ComptimeValues(
        llvm::IRBuilder<>& b, errorHandler::ErrorHandler& e, TypeHelper& tyhelper, runtime_bridge& bridge
    ) : _b(b), _e(e), _tyhelper(tyhelper), _bridge(bridge) {};
    // Const objects. The layouts themselves live on the bridge classes' const_build; what's left
    // here is the interning and the placing into globals.
    llvm::Constant* createMethodObj(const std::string& name, uint8_t arity, llvm::Function* method_ptr);
    llvm::Constant* createESLString(const std::string& str);
    llvm::Constant* constObjToVal(llvm::Constant* obj, object::rt_type type) const;
    llvm::GlobalVariable* storeConstObj(llvm::Constant* obj) const;
    auto ESL_strings() { return ESLStrings | std::views::values; }

    // Constants that aren't ESL objects
    llvm::Constant* createConstStr(const std::string& str);
    llvm::Constant* createConstant(const std::variant<double, bool, void*, std::string>& constant);
};
