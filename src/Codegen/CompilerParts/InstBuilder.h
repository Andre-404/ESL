#pragma once
#include "llvm/IR/IRBuilder.h"
#include "ComptimeValues.h"
#include "RuntimeTypecheck.h"


class InstBuilder {
    using callback = std::function<llvm::Value*(llvm::FunctionCallee, std::span<llvm::Value*>)>;
    using known_access = std::tuple<llvm::Value*, const std::string&, const types::ClassType&>;
    using unknown_access = std::tuple<llvm::Value*, const std::string&>;
public:
    InstBuilder(llvm::IRBuilder<>& b, TypeHelper& tyhelp, ComptimeValues& ct, RuntimeTypecheck& rt, errorHandler::ErrorHandler& e)
        : _b(b), _tyhelp(tyhelp), _ct(ct), _rt(rt), _e(e) {};

    llvm::GlobalVariable* createInstanceTemplate(llvm::Constant* klass, int fieldN) const;
    llvm::Value* optimizeInstGet(known_access access_data, llvm::Constant* vtable) const;
    llvm::Value* instGetUnoptimized(unknown_access access_data);

    llvm::Value* getOptInstFieldPtr(known_access access_data) const;
    llvm::Value* getUnoptInstFieldPtr(unknown_access access_data) const;

    llvm::Value* optimizeInvoke(known_access access_data, llvm::Constant* vtable, uint8_t argc, const callback& cb) const;
    llvm::Value* unoptimizedInvoke(unknown_access access_data, uint8_t argc, const callback& cb) const;
private:
    llvm::IRBuilder<>& _b;
    TypeHelper& _tyhelp;
    ComptimeValues& _ct;
    RuntimeTypecheck& _rt;
    errorHandler::ErrorHandler& _e;

    std::pair<llvm::Value*, llvm::Value*> instGetClassPtr(llvm::Value* inst) const;
    std::pair<llvm::Value*, llvm::Value*> instGetUnoptIdx(llvm::Value* klass, const std::string &field) const;
    llvm::Value* instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx) const;

    // Helper
    void create_if(llvm::Value* cond, const std::function<void()>& then, const std::function<void()>& _else) const;
    llvm::Function* get_func(const std::string& name) const;
};