#include "RuntimeTypecheck.h"
#include "IRUtils.h"
#include "../../Includes/fmt/core.h"

#include "llvm/IR/Module.h"


enum class rterr_ty : uint8_t {
    WRONG_TYPE,
    WRONG_TYPE_BINARY,
    ARG_CNT,
    INST_FIELD,
    OUT_OF_BOUNDS
};
constexpr unsigned operator+ (rterr_ty const val) { return static_cast<byte>(val); }

RtInst RuntimeTypecheck::checked_inst(const string& err, llvm::Value* val) const {
    createTypeCheckUnary(err, val, obj_type_masks(RtInst::tag));
    return _bridge.inst(val);
}
RtArr RuntimeTypecheck::checked_arr(const string& err, llvm::Value* val) const {
    createTypeCheckUnary(err, val, obj_type_masks(RtArr::tag));
    return _bridge.arr(val);
}
RtClosure RuntimeTypecheck::checked_closure(const string& err, llvm::Value* val) const {
    createTypeCheckUnary(err, val, obj_type_masks(RtClosure::tag));
    return _bridge.closure(val);
}

void RuntimeTypecheck::createTypeCheckFail(const string& err, llvm::Value* val) const {
    auto intMax = _b.getInt64(UINT64_MAX);
    auto castVal = _tyhelp.ESLValTo(val, _b.getInt64Ty());
    _bridge.call(
        "rt_error",
        { _ct.createConstStr(err), _b.getInt8(+rterr_ty::WRONG_TYPE), castVal, intMax, intMax}
    );
    _b.CreateUnreachable();
}

// All type info is contained inside the 64bits of the NaN boxed value, so we can just ICmp
// runtimeError requires that the nonUsed arguments be UINT64_MAX
void RuntimeTypecheck::createTypeCheckUnary(const string& err, llvm::Value* const val, std::tuple<uint64_t, uint64_t, bool> masks) const {
    auto [expected, mask, useNEQ] = masks;

    auto castVal = _tyhelp.ESLValTo(val, _b.getInt64Ty());
    auto expectedType = _b.getInt64(expected);
    auto cond = _b.CreateICmpEQ(_b.CreateAnd(castVal, mask), expectedType, "type.check");
    if(useNEQ) cond = _b.CreateNot(cond);

    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b, _b.CreateNot(cond),
        [&]() {
            auto intMax = _b.getInt64(UINT64_MAX);
            _bridge.call(
                "rt_error",
                { _ct.createConstStr(err), _b.getInt8(+rterr_ty::WRONG_TYPE), castVal, intMax, intMax}
            );
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createTypeCheckBinary(
    const string& err, llvm::Value* const lhs, llvm::Value* const rhs, std::tuple<uint64_t, uint64_t, bool> masks
) const {
    auto [expected, mask, useNEQ] = masks;

    auto castLhs = _tyhelp.ESLValTo(lhs, _b.getInt64Ty());
    auto castRhs = _tyhelp.ESLValTo(rhs, _b.getInt64Ty());
    auto expectedType = _b.getInt64(expected);

    auto condLhs = _b.CreateICmpEQ(_b.CreateAnd(castLhs, mask), expectedType, "type.check.lhs");;
    auto condRhs = _b.CreateICmpEQ(_b.CreateAnd(castRhs, mask), expectedType, "type.check.rhs");
    if(useNEQ) {
        condLhs = _b.CreateNot(condLhs);
        condRhs = _b.CreateNot(condRhs);
    }
    auto cond = _b.CreateAnd(condLhs, condRhs);
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b, _b.CreateNot(cond),
        [&]() {
            auto intMax = _b.getInt64(UINT64_MAX);
            auto str = _ct.createConstStr(err);
            _bridge.call(
                "rt_error",
                {str, _b.getInt8(+rterr_ty::WRONG_TYPE_BINARY), castLhs, castRhs, intMax}
            );
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createArgCountCheck(const string& err, llvm::Value* closure, uint8_t expectedArity) const {
    auto argNum = _bridge.closure(closure).arity().load();

    auto cond = _b.CreateICmpNE(argNum, _b.getInt8(expectedArity));
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(false)});
    create_if(_b, cond,
        [&]() {
            auto intMax = _b.getInt64(UINT64_MAX);
            auto str = _ct.createConstStr(err);
            _bridge.call(
                "rt_error",
                {
                    str, _b.getInt8(+rterr_ty::ARG_CNT), 
                    _tyhelp.ESLValTo(closure, _b.getInt64Ty()),
                    _b.getInt64(expectedArity), intMax
                }
            );
            _b.CreateUnreachable();
        },
        [](){}
    );
}
void RuntimeTypecheck::createArrBoundsCheck(const string& err, llvm::Value* arr, llvm::Value* index) const {
    llvm::Value* upperbound = _bridge.arr(arr).size().load();
    upperbound = _b.CreateZExt(upperbound, _b.getInt64Ty());

    // Index can be negative
    auto castIndex = _b.CreateFPToSI(_tyhelp.ESLValTo(index, _b.getDoubleTy()), _b.getInt64Ty(), "index");
    auto cond = _b.CreateICmpSGE(castIndex, upperbound);
    auto cond2 = _b.CreateICmpSLT(castIndex, _b.getInt64(0));
    cond = _b.CreateOr(cond, cond2, "out.of.bounds");
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(false)});

    create_if(_b, cond,
        [&]() {
            auto intMax = _b.getInt64(UINT64_MAX);
            auto str = _ct.createConstStr(err);
            auto castArray = _tyhelp.ESLValTo(arr, _b.getInt64Ty());
            _bridge.call(
                "rt_error",
                {str, _b.getInt8(+rterr_ty::OUT_OF_BOUNDS), castArray, castIndex, intMax}
            );
            _b.CreateUnreachable();
        },
        [](){}
    );
}
// Doesn't actually perform any checks, that is done inside instUnoptGet/Set
void RuntimeTypecheck::createInstNoField(const string& err, const string& field, llvm::Value* inst) const {
    auto castInst = _tyhelp.ESLValTo(inst, _b.getInt64Ty());
    auto intMax = _b.getInt64(UINT64_MAX);
    auto str = _ct.createConstStr(err);
    auto fieldStr = _ct.createConstStr(field);
    // Have to cast because runtimeError expects int64 for the 3 args
    // rt_error typecasts back to a ptr
    fieldStr = llvm::ConstantExpr::getPtrToInt(fieldStr, _b.getInt64Ty());
    _bridge.call(
        "rt_error",
        {str, _b.getInt8(+rterr_ty::INST_FIELD), castInst, fieldStr, intMax}
    );
}
void RuntimeTypecheck::createInstClassCheck(
    const string& err, llvm::Value* inst, llvm::Constant* subClassIdxStart, llvm::Constant* subClassIdxEnd
) const {
    auto cond = _bridge.call("is_inst_of_class", {inst, subClassIdxStart, subClassIdxEnd});
    cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, {cond, _b.getInt1(true)});
    create_if(_b, _b.CreateNot(cond),
       [&]() {
            auto intMax = _b.getInt64(UINT64_MAX);
            auto str = _ct.createConstStr(err);
            _bridge.call(
                "rt_error",
                {str, _b.getInt8(+rterr_ty::WRONG_TYPE),
                    _tyhelp.ESLValTo(inst, _b.getInt64Ty()), intMax, intMax
                }
            );
            _b.CreateUnreachable();
       },
       [](){}
   );
}

llvm::FunctionCallee RuntimeTypecheck::createUnoptFunCall(llvm::Value* closureVal, int argc) const {
    auto closure = checked_closure("Expected a function for a callee, got '{}'.", closureVal);

    auto err = fmt::format("Function {} being called with {} arguments when it accepts {}.", "{}", argc, "{}");
    createArgCountCheck(err, closureVal, argc);

    auto fnPtr = closure.func().load();
    return { _tyhelp.getFuncType(argc), _b.CreateBitCast(fnPtr, _b.getPtrTy()) };
}

void RuntimeTypecheck::createWeightedSwitch(llvm::Value* cond, vector<std::tuple<int, llvm::BasicBlock*, int>> cases) const {
    auto sw = _b.CreateSwitch(cond, std::get<1>(cases.front()));
    // Convert weights to LLVM constants. branch_weights needs one entry per successor, and the
    // default block is a successor too, so its weight goes in before the cases.
    std::vector<llvm::Metadata*> weights;
    weights.push_back(llvm::MDString::get(_b.getContext(), "branch_weights"));
    weights.push_back(llvm::ConstantAsMetadata::get(_b.getInt32(std::get<2>(cases.front()))));

    for(auto [_case, BB, weight] : cases | std::views::drop(1)){
        sw->addCase(_b.getInt8(_case), BB);
        weights.push_back(llvm::ConstantAsMetadata::get(_b.getInt32(weight)));
    }

    sw->setMetadata(
        llvm::LLVMContext::MD_prof, llvm::MDNode::get(_b.getContext(), weights)
    );
}
