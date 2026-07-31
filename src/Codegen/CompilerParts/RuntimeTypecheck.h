#pragma once
#include "llvm/IR/IRBuilder.h"
#include "ComptimeValues.h"


class RuntimeTypecheck {
    llvm::IRBuilder<>& _b;
    TypeHelper& _tyhelp;
    ComptimeValues& _ct;
    runtime_bridge& _bridge;
public:
    RuntimeTypecheck(llvm::IRBuilder<>& b, TypeHelper& tyhelp, ComptimeValues& ct, runtime_bridge& bridge)
        : _b(b), _tyhelp(tyhelp), _ct(ct), _bridge(bridge) {};

    // Typechecks the value, then hands back the decoded handle
    RtInst checked_inst(const string& err, llvm::Value* val) const;
    RtArr checked_arr(const string& err, llvm::Value* val) const;
    RtClosure checked_closure(const string& err, llvm::Value* val) const;

    void createTypeCheckFail(const string& err, llvm::Value* val) const;
    void createTypeCheckUnary(const string& err, llvm::Value* val, std::tuple<uint64_t, uint64_t, bool> masks) const;
    void createTypeCheckBinary(
        const string& err, llvm::Value* lhs, llvm::Value* rhs, std::tuple<uint64_t, uint64_t, bool> masks
    ) const;
    void createArrBoundsCheck(const string& err, llvm::Value* arr, llvm::Value* index) const;
    void createInstNoField(const string& err, const string& field, llvm::Value* inst) const;
    void createInstClassCheck(
        const string& err, llvm::Value* inst, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd
    ) const;
    llvm::FunctionCallee createUnoptFunCall(llvm::Value* closureVal, int argc) const;

    // First block is default, doesn't technically belong here but seems like the best place to put it
    void createWeightedSwitch(llvm::Value* cond, vector<std::tuple<int, llvm::BasicBlock*, int>> cases) const;
private:
    void createArgCountCheck(const string& err, llvm::Value* closure, uint8_t expectedArity) const;
};