#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../common.h"
#include "../../Includes/unorderedDense.h"
#include "../../ErrorHandling/errorHandler.h"
#include "TypeHelper.h"

#include <ranges>


class ComptimeValues {
    template<class T, class K>
    using ankerl_map = ankerl::unordered_dense::map<T, K>;
    public:
    ComptimeValues(llvm::IRBuilder<>& b, errorHandler::ErrorHandler& e, TypeHelper& tyhelper) : _b(b), _e(e), _tyhelper(tyhelper) {};
    // Const objects
    llvm::Constant* createMethodObj(const std::string& name, uint8_t arity, llvm::Function* method_ptr);
    llvm::Constant* createESLString(const std::string& str);
    llvm::Constant* createConstObjHeader(int type) const;
    llvm::Constant* constObjToVal(llvm::Constant* obj, uint8_t type) const;
    llvm::GlobalVariable* storeConstObj(llvm::Constant* obj) const;
    auto ESL_strings() { return ESLStrings | std::views::values; }

    // Constants that aren't ESL objects
    llvm::Constant* createConstStr(const std::string& str);
    llvm::Constant* createConstant(const std::variant<double, bool, void*, std::string>& constant);
private:
    llvm::IRBuilder<>& _b;
    errorHandler::ErrorHandler& _e;
    TypeHelper& _tyhelper;


    ankerl_map<string, llvm::Constant*> CStrings;
    ankerl_map<string, llvm::Constant*> ESLStrings;
};
