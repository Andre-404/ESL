#include "RuntimeTypecheck.h"
#include "../LLVMHelperFunctions.h"
#include "../../Runtime/Values/valueHelpers.h"
#include "../../Includes/fmt/format.h"

#include "llvm/IR/Module.h"


enum class rterr_ty : uint8_t {
    WRONG_TYPE,
    WRONG_TYPE_BINARY,
    ARG_CNT,
    INST_FIELD,
    OUT_OF_BOUNDS
};
constexpr unsigned operator+ (rterr_ty const val) { return static_cast<byte>(val); }

void RuntimeTypecheck::createTypeCheckFail(const string& err, llvm::Value* val) const {
    llvm::Value* intMax = _b.getInt64(UINT64_MAX);
    llvm::Value* castVal = _tyhelp.ESLValTo(val, _b.getInt64Ty());
    _b.CreateCall(get_func("runtimeError"),{ _ct.createConstStr(err),
        _b.getInt8(+rterr_ty::WRONG_TYPE), castVal, intMax, intMax});
    _b.CreateUnreachable();
}

// All type info is contained inside the 64bits of the NaN boxed value, so we can just ICmp
// runtimeError requires that the nonUsed arguments be UINT64_MAX
void RuntimeTypecheck::createTypeCheckUnary(const string& err, llvm::Value* const val, std::tuple<uint64_t, uint64_t, bool> masks) const {
    auto [expected, mask, useNEQ] = masks;

    llvm::Value* castVal = _tyhelp.ESLValTo(val, _b.getInt64Ty());
    llvm::Value* expectedType = _b.getInt64(expected);
    llvm::Value* cond = _b.CreateICmpEQ(_b.CreateAnd(castVal, mask), expectedType, "type.check");
    if(useNEQ) cond = _b.CreateNot(cond);

    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b.CreateNot(cond),
        [&]() {
            llvm::Value* intMax = _b.getInt64(UINT64_MAX);
            _b.CreateCall(get_func("runtimeError"),{ _ct.createConstStr(err),
                _b.getInt8(+rterr_ty::WRONG_TYPE), castVal, intMax, intMax});
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createTypeCheckBinary(const string& err, llvm::Value* const lhs, llvm::Value* const rhs,
        std::tuple<uint64_t, uint64_t, bool> masks) const {
    auto [expected, mask, useNEQ] = masks;

    llvm::Value* castLhs = _tyhelp.ESLValTo(lhs, _b.getInt64Ty());
    llvm::Value* castRhs = _tyhelp.ESLValTo(rhs, _b.getInt64Ty());
    llvm::Value* expectedType = _b.getInt64(expected);

    llvm::Value* condLhs = _b.CreateICmpEQ(_b.CreateAnd(castLhs, mask), expectedType, "type.check.lhs");;
    llvm::Value* condRhs = _b.CreateICmpEQ(_b.CreateAnd(castRhs, mask), expectedType, "type.check.rhs");
    if(useNEQ) {
        condLhs = _b.CreateNot(condLhs);
        condRhs = _b.CreateNot(condRhs);
    }
    llvm::Value* cond = _b.CreateAnd(condLhs, condRhs);
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b.CreateNot(cond),
        [&]() {
            llvm::Value* intMax = _b.getInt64(UINT64_MAX);
            llvm::Constant* str = _ct.createConstStr(err);
            _b.CreateCall(get_func("runtimeError"),{str,
                _b.getInt8(+rterr_ty::WRONG_TYPE_BINARY), castLhs, castRhs, intMax});
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createArgCountCheck(const string& err, llvm::Value* closure, uint8_t expectedArity) const {
    llvm::Value* decodedClosure = _b.CreateCall(get_func("decodeObj"), closure, "decoded.closure");
    vector<llvm::Value *> idxList = {_b.getInt32(0), _b.getInt32(1)};
    auto argNumPtr = _b.CreateInBoundsGEP(_tyhelp.internal_obj_ty("ObjClosure"), decodedClosure, idxList);
    auto argNum = _b.CreateLoad(_b.getInt8Ty(), argNumPtr);

    create_if(_b.CreateICmpNE(argNum, _b.getInt8(expectedArity)),
        [&]() {
            llvm::Value* intMax = _b.getInt64(UINT64_MAX);
            llvm::Constant* str = _ct.createConstStr(err);
            _b.CreateCall(get_func("runtimeError"),{str, _b.getInt8(+rterr_ty::ARG_CNT),
                _tyhelp.ESLValTo(closure, _b.getInt64Ty()), _b.getInt64(expectedArity), intMax});
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createArrBoundsCheck(const string& err, llvm::Value* arr, llvm::Value* index) const {
    llvm::Value* decodedArr = _b.CreateCall(get_func("decodeObj"), arr, "decoded.arr");
    llvm::Value* ptrToSize = _b.CreateConstInBoundsGEP2_32(_tyhelp.internal_obj_ty("ObjArray"), decodedArr, 0, 2);
    llvm::Value* upperbound = _b.CreateLoad(_b.getInt32Ty(), ptrToSize, "arr.size");
    upperbound = _b.CreateZExt(upperbound, _b.getInt64Ty());

    // Index can be negative
    auto castIndex = _b.CreateFPToSI(_tyhelp.ESLValTo(index, _b.getDoubleTy()), _b.getInt64Ty(), "index");
    auto cond = _b.CreateICmpSGE(castIndex, upperbound);
    auto cond2 = _b.CreateICmpSLT(castIndex, _b.getInt64(0));
    cond = _b.CreateOr(cond, cond2, "out.of.bounds");
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(false)});

    create_if(cond,
        [&]() {
            llvm::Value* intMax = _b.getInt64(UINT64_MAX);
            llvm::Constant* str = _ct.createConstStr(err);
            llvm::Value* castArray = _tyhelp.ESLValTo(arr, _b.getInt64Ty());
            _b.CreateCall(get_func("runtimeError"),{str,
                _b.getInt8(+rterr_ty::OUT_OF_BOUNDS), castArray, castIndex, intMax});
            _b.CreateUnreachable();
        },
        [](){}
    );
}
// Doesn't actually perform any checks, that is done inside instUnoptGet/Set
void RuntimeTypecheck::createInstNoField(const string& err, const string& field, llvm::Value* inst) const {
    llvm::Value* castInst = _tyhelp.ESLValTo(inst, _b.getInt64Ty());
    llvm::Value* intMax = _b.getInt64(UINT64_MAX);
    llvm::Constant* str = _ct.createConstStr(err);
    llvm::Constant* fieldStr = _ct.createConstStr(field);
    // Have to case because runtimeError expects int64 for the 3 args
    fieldStr = llvm::ConstantExpr::getPtrToInt(fieldStr, _b.getInt64Ty());
    _b.CreateCall(get_func("runtimeError"),{str, _b.getInt8(+rterr_ty::INST_FIELD),
                                                    castInst, fieldStr, intMax});
}
void RuntimeTypecheck::createInstClassCheck(const string& err, llvm::Value* inst,
        llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd) const {
    llvm::Value* cond = _b.CreateCall(get_func("isInstAndClass"), {inst, subClassIdxStart, subClassIdxEnd});
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b.CreateNot(cond),
       [&]() {
            llvm::Value* intMax = _b.getInt64(UINT64_MAX);
            llvm::Constant* str = _ct.createConstStr(err);
            _b.CreateCall(get_func("runtimeError"),{str,
                _b.getInt8(+rterr_ty::WRONG_TYPE), _tyhelp.ESLValTo(inst, _b.getInt64Ty()), intMax, intMax});
            _b.CreateUnreachable();
       },
       [](){}
   );
}

// Helper
void RuntimeTypecheck::create_if(llvm::Value* cond, const std::function<void()>& then, const std::function<void()>& _else) const {
    llvm::Function *F = _b.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(_b.getContext(), "then", F);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(_b.getContext(), "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(_b.getContext(), "merge");
    _b.CreateCondBr(cond, thenBB, elseBB);
    _b.SetInsertPoint(thenBB);
    then();
    if (!_b.GetInsertBlock()->back().isTerminator()) _b.CreateBr(mergeBB);

    F->insert(F->end(), elseBB);
    _b.SetInsertPoint(elseBB);
    _else();
    if (_b.GetInsertBlock()->empty() || !_b.GetInsertBlock()->back().isTerminator()) _b.CreateBr(mergeBB);
    F->insert(F->end(), mergeBB);
    _b.SetInsertPoint(mergeBB);
}

llvm::Function* RuntimeTypecheck::get_func(const string& name) const {
    auto* fn = _b.GetInsertBlock()->getModule()->getFunction(name);
    if(!fn){
        std::cerr<<fmt::format("Function {} hasn't been created yet.\n", name);
        exit(64);
    }
    return fn;
}
