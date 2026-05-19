#pragma once
#include "llvm/IR/IRBuilder.h"
#include "../../common.h"
#include "ComptimeValues.h"


class RuntimeTypecheck {
public:
    RuntimeTypecheck(llvm::IRBuilder<>& b, TypeHelper& tyhelp, ComptimeValues& ct) : _b(b), _tyhelp(tyhelp), _ct(ct) {};

    void createTypeCheckFail(const string& err, llvm::Value* val) const;
    void createTypeCheckUnary(const string& err, llvm::Value* val, std::tuple<uint64_t, uint64_t, bool> masks) const;
    void createTypeCheckBinary(const string& err, llvm::Value* lhs, llvm::Value* rhs, std::tuple<uint64_t, uint64_t, bool> masks) const;
    void createArgCountCheck(const string& err, llvm::Value* closure, uint8_t expectedArity) const;
    void createArrBoundsCheck(const string& err, llvm::Value* arr, llvm::Value* index) const;
    void createInstNoField(const string& err, const string& field, llvm::Value* inst) const;
    void createInstClassCheck(const string& err, llvm::Value* inst, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd) const;
private:
    llvm::IRBuilder<>& _b;
    TypeHelper& _tyhelp;
    ComptimeValues& _ct;

    // Helper
    void create_if(llvm::Value* cond, const std::function<void()>& then, const std::function<void()>& _else) const;
    llvm::Function* get_func(const string& name) const;
};