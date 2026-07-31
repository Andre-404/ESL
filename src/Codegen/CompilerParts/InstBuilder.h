#pragma once
#include "llvm/IR/IRBuilder.h"
#include "ComptimeValues.h"
#include "RuntimeTypecheck.h"


class InstBuilder {
    using callback = std::function<llvm::Value*(llvm::FunctionCallee, std::span<llvm::Value*>)>;
    using known_access = std::tuple<llvm::Value*, const std::string&, const types::ClassType&>;
    using unknown_access = std::tuple<llvm::Value*, const std::string&>;

    llvm::IRBuilder<>& _b;
    TypeHelper& _tyhelp;
    ComptimeValues& _ct;
    RuntimeTypecheck& _rt;
    runtime_bridge& _bridge;
    errorHandler::ErrorHandler& _e;
public:
    InstBuilder(
        llvm::IRBuilder<>& b, TypeHelper& tyhelp, ComptimeValues& ct, RuntimeTypecheck& rt,
        runtime_bridge& bridge, errorHandler::ErrorHandler& e
    ) : _b(b), _tyhelp(tyhelp), _ct(ct), _rt(rt), _bridge(bridge), _e(e) {};

    // Nulled [N x Value] that create_inst copies into each new instance
    llvm::GlobalVariable* createFieldTemplate(int fieldN) const;
    llvm::Value* optimizeInstGet(known_access access_data, llvm::Constant* vtable) const;
    llvm::Value* instGetUnoptimized(unknown_access access_data);

    llvm::Value* getOptInstFieldPtr(known_access access_data) const;
    llvm::Value* getUnoptInstFieldPtr(unknown_access access_data) const;

    llvm::Value* optimizeInvoke(known_access access_data, llvm::Constant* vtable, uint8_t argc, const callback& cb) const;
    llvm::Value* unoptimizedInvoke(unknown_access access_data, uint8_t argc, const callback& cb) const;
private:
    // A method slot inside the class' inline method array, tagged as a closure Value
    llvm::Constant* constMethodVal(llvm::Constant* vtable, uint64_t methodIdx) const;
    std::pair<llvm::Value*, llvm::Value*> instGetUnoptIdx(CompClass klass, const std::string& field) const;
    llvm::Value* instGetIdxType(llvm::Value* fieldIdx, llvm::Value* methodIdx) const;
};
