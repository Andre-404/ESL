#include "RuntimeBridge.h"
#include "../../Runtime/Values/valueHelpers.h"
#include "../../Runtime/Values/valueHelpersInline.h"
#include "../../Runtime/ConcurrentGC/TCB.h"
#include "../../Includes/fmt/core.h"
#include "BridgeObjects.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/Alignment.h"

#include <iostream>

namespace {

// ---------------------------------------------------------------------------------------------
// Offsets into the runtime's thread control block, which the write barrier and the safepoint poll
// reach through r15
// If the structure of TCB, mark_buf or mark_info changed this will need to be updated
// ---------------------------------------------------------------------------------------------
constexpr size_t align_to(size_t off, size_t align) { return (off + align - 1) & ~(align - 1); }

using thd_state_t = std::atomic<gc::detail::thd_state>;

// gc::detail::tcb derives from gc::tcb_handle and declares _thd_state first, then _mark_info.
constexpr size_t tcb_state_off = align_to(sizeof(gc::tcb_handle), alignof(thd_state_t));
constexpr size_t tcb_mark_info_off = align_to(tcb_state_off + sizeof(thd_state_t), alignof(gc::detail::thd_mark_info));
// _wbbuf is thd_mark_info's first member, so the buffer pointer sits at the start of _mark_info.
constexpr size_t tcb_wbbuf_off = tcb_mark_info_off;

// gc::detail::mark_buf derives from tnode<mark_buf> which puts the intrusive pointer first
// and then declares _cnt and _data.
constexpr size_t markbuf_cnt_off = align_to(sizeof(gc::detail::tnode<gc::detail::mark_buf>), alignof(size_t));
constexpr size_t markbuf_data_off = markbuf_cnt_off + sizeof(size_t);
constexpr size_t markbuf_capacity = (sizeof(gc::detail::mark_buf) - markbuf_data_off) / sizeof(gc::managed*);

static_assert(
    tcb_state_off == 16 && tcb_wbbuf_off == 24,
    "TCB layout moved; the IR emitted by safepoint_poll/gc_write_barrier follows automatically, "
    "but check that nothing else assumed the old offsets"
);
static_assert(
    tcb_state_off % 8 == 0,
    "safepoint_poll loads the thread state assuming 8 byte alignment, which is much better for the "
    "cpu than the natural alignment of the single byte it actually reads"
);
static_assert(
    markbuf_data_off == 16 && markbuf_capacity > 0,
    "mark_buf layout moved; gc_write_barrier follows automatically"
);

// Invariants that live purely in C++ and so can be checked at compile time.
static_assert(sizeof(object::rt_obj) == 2, "the object header must stay 2 bytes");
static_assert(
    sizeof(object::comp_class) % 16 == 0,
    "comp_class must be a multiple of 16 bytes: the method array sits directly behind it and those "
    "closures get NaN-boxed, which needs the low 4 bits of the pointer free"
);

// ---------------------------------------------------------------------------------------------
// Attribute bundles
// Runtime functions have no body in this module, so their attributes are the only thing the
// optimizer knows about them
// ---------------------------------------------------------------------------------------------

// A call that always returns, touches nothing the caller can observe beyond its stated memory
// effects, and can't reenter the module. Memory effects are always stated separately
// The NoCallback is important when we start adding native functions that take callbacks
constexpr auto total_fn = std::array {
    llvm::Attribute::NoUnwind,  llvm::Attribute::WillReturn, llvm::Attribute::MustProgress,
    llvm::Attribute::NoRecurse, llvm::Attribute::NoSync,     llvm::Attribute::NoFree,
    llvm::Attribute::NoCallback,
};

// A call that can allocate, and so can reach a safepoint. Deliberately says nothing about memory:
// the collector moves objects and rewrites the heap slots referring to them, and jitted code has no
// read barrier, so a field load is not valid across one of these. No nofree (the collector frees)
// and no nosync (the safepoint handshake is synchronization)
constexpr auto may_gc_fn = std::array {
    llvm::Attribute::NoUnwind, llvm::Attribute::WillReturn, llvm::Attribute::MustProgress,
    llvm::Attribute::NoCallback,
};

void add_attrs(llvm::Function* f, llvm::ArrayRef<llvm::Attribute::AttrKind> attrs) {
    for (auto attr : attrs) f->addFnAttr(attr);
}

llvm::Attribute no_capture(llvm::LLVMContext& c) {
    return llvm::Attribute::getWithCaptureInfo(c, llvm::CaptureInfo::none());
}

[[noreturn]] void bridge_abort(const std::string& msg) {
    std::cerr << fmt::format("Runtime bridge: {}\n", msg);
    exit(64);
}

} // namespace

runtime_bridge::runtime_bridge(llvm::IRBuilder<>& b, llvm::Module& mod, const llvm::DataLayout& DL)
    : _b(b), _mod(mod), _DL(DL) {
    create_types();
    verify_layouts();
    declare_runtime_functions();
    build_ir_helpers();
}

llvm::Type* runtime_bridge::eslValType(llvm::LLVMContext& ctx) {
    return llvm::PointerType::get(ctx, 1);
}

// -------------------------------------------------------------------------------------------------
// Layouts
// -------------------------------------------------------------------------------------------------

void runtime_bridge::create_types() {
    auto& c = ctx();
    auto i8 = llvm::Type::getInt8Ty(c);
    auto i16 = llvm::Type::getInt16Ty(c);
    auto i32 = llvm::Type::getInt32Ty(c);
    auto i64 = llvm::Type::getInt64Ty(c);
    auto ptr = llvm::PointerType::getUnqual(c);

    auto make = [&](bridge_ty ty, const char* name, std::initializer_list<llvm::Type*> body) {
        _types[+ty] = llvm::StructType::create(c, body, name);
    };

    make(bridge_ty::rt_obj, "rt_obj",
        { i8, i8 }
    );
    auto obj = _types[+bridge_ty::rt_obj];

    make(bridge_ty::rt_string, "rt_string",
        { obj, i32 }
    );
    make(bridge_ty::rt_arr, "rt_arr",
        { obj, i32, ptr }
    );
    make(bridge_ty::rt_arr_store, "rt_arr_store",
        { obj, i8, i32 }
    );
    make(bridge_ty::rt_closure, "rt_closure",
        { obj, i8, i8, ptr, ptr }
    );
    make(bridge_ty::rt_inst, "rt_inst",
        { obj, ptr }
    );

    // Compiler-only: rt_closure padded out so an array of them keeps every element 16 byte aligned
    make(bridge_ty::rt_closure_aligned, "rt_closure_aligned",
        { _types[+bridge_ty::rt_closure], i64 }
    );

    // object::comp_class. The trailing i64 is the tail padding alignas(16) adds
    make(bridge_ty::comp_class, "comp_class",
        { i16, i16, i32, i32, ptr, ptr, ptr, i64 }
    );
}

uint64_t runtime_bridge::field_off(bridge_ty ty, unsigned idx) const {
    return _DL.getStructLayout(_types[+ty])->getElementOffset(idx);
}

llvm::Align runtime_bridge::field_align(bridge_ty ty, unsigned idx) const {
    return llvm::commonAlignment(_DL.getABITypeAlign(_types[+ty]), field_off(ty, idx));
}

uint64_t runtime_bridge::trailing_off(bridge_ty ty) const {
    return _DL.getTypeAllocSize(_types[+ty]);
}

void runtime_bridge::verify_layouts() const {
    struct entry { bridge_ty ty; const char* name; size_t cpp_size; size_t cpp_align; };
    const entry entries[] = {
        { bridge_ty::rt_obj,       "rt_obj",       sizeof(object::rt_obj),       alignof(object::rt_obj) },
        { bridge_ty::rt_string,    "rt_string",    sizeof(object::rt_string),    alignof(object::rt_string) },
        { bridge_ty::rt_arr,       "rt_arr",       sizeof(object::rt_arr),       alignof(object::rt_arr) },
        { bridge_ty::rt_arr_store, "rt_arr_store", sizeof(object::rt_arr_store), alignof(object::rt_arr_store) },
        { bridge_ty::rt_closure,   "rt_closure",   sizeof(object::rt_closure),   alignof(object::rt_closure) },
        { bridge_ty::rt_inst,      "rt_inst",      sizeof(object::rt_inst),      alignof(object::rt_inst) },
        { bridge_ty::comp_class,   "comp_class",   sizeof(object::comp_class),   alignof(object::comp_class) },
    };

    for (const auto& e : entries) {
        auto llvmSize = _DL.getTypeAllocSize(_types[+e.ty]);
        if (llvmSize != e.cpp_size)
            bridge_abort(fmt::format(
                "layout drift in {}: the mirror is {} bytes, C++ says {}. ", e.name, llvmSize, e.cpp_size
            ));
        // The mirror may be *less* strictly aligned than the C++ type (comp_class is alignas(16)
        // while LLVM only infers 8 from its members)
        auto llvmAlign = _DL.getABITypeAlign(_types[+e.ty]).value();
        if (llvmAlign <= e.cpp_align) continue;

        bridge_abort(fmt::format(
            "alignment drift in {}: the mirror wants {}, C++ only guarantees {}.", e.name, llvmAlign, e.cpp_align
        ));
    }

    struct class_field { unsigned idx; const char* name; size_t cpp_off; };
    const class_field classFields[] = {
        { 0, "methodArrLen",        offsetof(object::comp_class, methodArrLen) },
        { 1, "fieldsArrLen",        offsetof(object::comp_class, fieldsArrLen) },
        { 2, "classHierarchyStart", offsetof(object::comp_class, classHierarchyStart) },
        { 3, "classHierarchyEnd",   offsetof(object::comp_class, classHierarchyEnd) },
        { 4, "name",                offsetof(object::comp_class, name) },
        { 5, "getMethod",           offsetof(object::comp_class, getMethod) },
        { 6, "getField",            offsetof(object::comp_class, getField) },
    };
    for (const auto& f : classFields) {
        auto off = field_off(bridge_ty::comp_class, f.idx);
        if (off == f.cpp_off) continue;
        
        bridge_abort(fmt::format(
            "layout drift in comp_class::{}: the mirror puts it at {}, C++ at {}.", f.name, off, f.cpp_off
        ));
    }

    if (trailing_off(bridge_ty::comp_class) % 16 != 0) {
        bridge_abort(
            "comp_class does not end on a 16 byte boundary, so tagged method pointers would collide with the type tag"
        );
    }
    if (_DL.getTypeAllocSize(_types[+bridge_ty::rt_closure_aligned]) % 16 != 0) {
        bridge_abort(
            "rt_closure_aligned is not a multiple of 16 bytes, so method array elements would not all be 16 byte aligned"
        );
    }
}

llvm::Constant* runtime_bridge::const_header(gc::move_state state, object::rt_type type) const {
    return llvm::ConstantStruct::get(
        _types[+bridge_ty::rt_obj], { _b.getInt8(+state), _b.getInt8(+type) }
    );
}

// -------------------------------------------------------------------------------------------------
// Functions
// -------------------------------------------------------------------------------------------------

llvm::Function* runtime_bridge::fn(const std::string& name) const {
    auto f = _mod.getFunction(name);
    if (!f) bridge_abort(fmt::format("no runtime function named '{}'", name));
    return f;
}

llvm::CallInst* runtime_bridge::call(const std::string& name, llvm::ArrayRef<llvm::Value*> args,
                                    const llvm::Twine& valName) const {
    return _b.CreateCall(fn(name), args, valName);
}

llvm::Function* runtime_bridge::define_fn(
    const std::string& name, llvm::FunctionType* ty, llvm::GlobalValue::LinkageTypes linkage,
    const std::function<void(llvm::Function*)>& body
) {
    auto F = llvm::Function::Create(ty, linkage, name, _mod);
    _b.SetInsertPoint(llvm::BasicBlock::Create(ctx(), "entry", F));
    body(F);
    llvm::verifyFunction(*F);
    return F;
}

void runtime_bridge::declare_runtime_functions() {
    auto& c = ctx();
    auto val = eslValType();
    auto voidTy = llvm::Type::getVoidTy(c);
    auto i8 = llvm::Type::getInt8Ty(c);
    auto i32 = llvm::Type::getInt32Ty(c);
    auto i64 = llvm::Type::getInt64Ty(c);
    auto ptr = llvm::PointerType::getUnqual(c);

    // Anything returning a Value hands back a NaN boxed pointer
    auto boxed_ret = [](llvm::Function* f) {
        f->addRetAttr(llvm::Attribute::NonNull);
        f->addRetAttr(llvm::Attribute::NoUndef);
    };

    // TODO: dllimport is only meaningful on windows
    auto declare = [&](
        const char* name, llvm::Type* ret, std::initializer_list<llvm::Type*> args,
        llvm::ArrayRef<llvm::Attribute::AttrKind> attrs = {}, bool isVarArg = false
    ) {
        auto* f = llvm::Function::Create(
            llvm::FunctionType::get(ret, args, isVarArg),
            llvm::Function::ExternalLinkage, name, _mod
        );
        f->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
        add_attrs(f, attrs);
        return f;
    };

    auto err = declare("rt_error", voidTy, { ptr, i8, i64, i64, i64 }, {
        llvm::Attribute::NoReturn,  llvm::Attribute::Cold,     llvm::Attribute::NoUnwind,
        llvm::Attribute::NoFree,    llvm::Attribute::NoRecurse, llvm::Attribute::MustProgress,
        llvm::Attribute::NoCallback
    });
    // Everything it needs beyond the format string is passed by value
    err->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    err->addParamAttr(0, llvm::Attribute::NonNull);
    err->addParamAttr(0, llvm::Attribute::ReadOnly);
    err->addParamAttr(0, no_capture(c));

    // From the C standard library
    declare("printf", i32, { ptr }, { llvm::Attribute::NoUnwind }, true);
    declare("exit", voidTy, { i32 },
        { llvm::Attribute::NoReturn, llvm::Attribute::NoUnwind, llvm::Attribute::Cold }
    );

    boxed_ret(declare("str_add", val, { val, val }, may_gc_fn));

    auto strCmp = declare("str_cmp", val, { val, val }, total_fn);
    // Strings are immutable, so two compares of the same pair can be folded into one
    strCmp->setMemoryEffects(llvm::MemoryEffects::argMemOnly(llvm::ModRefInfo::Ref));
    boxed_ret(strCmp);

    // Invoked from safepoint_poll once the thread has been asked to stop. Cold so the taken branch
    // gets laid out out of line even if the llvm.expect metadata gets dropped along the way
    declare("safepoint", voidTy, {}, {
        llvm::Attribute::NoUnwind, llvm::Attribute::Cold, llvm::Attribute::WillReturn,
        llvm::Attribute::MustProgress
    });

    auto allocArr = declare("alloc_arr", val, { i32 }, may_gc_fn);
    allocArr->addParamAttr(0, llvm::Attribute::NoUndef);
    boxed_ret(allocArr);

    auto gcInit = declare("gc_init", ptr, { ptr }, { llvm::Attribute::NoUnwind });
    gcInit->addParamAttr(0, llvm::Attribute::NonNull);
    gcInit->addRetAttr(llvm::Attribute::NonNull);
    gcInit->addRetAttr(llvm::Attribute::NoUndef);

    declare("intern_str", voidTy, { val }, may_gc_fn);

    declare("gc_add_root", voidTy, { ptr }, may_gc_fn)
        ->addParamAttr(0, llvm::Attribute::NonNull);

    auto* mkInst = declare("create_inst", ptr, { ptr, ptr }, may_gc_fn);
    mkInst->addFnAttr(llvm::Attribute::NoRecurse);
    {
        llvm::AttrBuilder ab(c);
        // 16 rather than 8: the result gets NaN-boxed, which needs the low 4 bits free
        ab.addAlignmentAttr(16);
        mkInst->addRetAttr(ab.getAttribute(llvm::Attribute::Alignment));
        ab.addDereferenceableAttr(sizeof(object::rt_inst));
        mkInst->addRetAttr(ab.getAttribute(llvm::Attribute::Dereferenceable));
    }
    // A fresh instance can't alias anything that existed before the call, so the field stores the
    // constructor emits right after don't have to be treated as clobbering unrelated loads
    mkInst->addRetAttr(llvm::Attribute::NoAlias);
    mkInst->addRetAttr(llvm::Attribute::NonNull);
    mkInst->addRetAttr(llvm::Attribute::NoUndef);
    // The class is stored into the instance, so it is captured. The field template is only copied out of
    mkInst->addParamAttr(0, llvm::Attribute::NonNull);
    mkInst->addParamAttr(0, llvm::Attribute::ReadOnly);
    mkInst->addParamAttr(1, llvm::Attribute::NonNull);
    mkInst->addParamAttr(1, llvm::Attribute::ReadOnly);
    mkInst->addParamAttr(1, no_capture(c));

    // First arg is the field count, followed by that many {Value(string), Value} pairs
    boxed_ret(declare("create_hashmap", val, { i32 }, may_gc_fn, true));
    // fn ptr, arity, name, freevar count, then that many freevars
    boxed_ret(declare("create_closure", val, { ptr, i8, ptr, i32 }, may_gc_fn, true));
    declare("hashmap_get", val, { ptr, ptr }, may_gc_fn);
    declare("hashmap_set", voidTy, { ptr, ptr, val }, may_gc_fn);

    // wrapper fn ptr, alloca holding the args, argc. No nocallback and no nosync: the wrapper is a
    // function from this module and this starts it running on another thread
    auto* newThd = declare("create_new_thd", voidTy, { ptr, ptr, i64 }, { llvm::Attribute::NoUnwind });
    // The args are copied out rather than kept
    newThd->addParamAttr(1, llvm::Attribute::ReadOnly);
    newThd->addParamAttr(1, no_capture(c));

    // frame address, the tcb handle handed over by create_new_thd
    declare("thd_init", voidTy, { ptr, ptr }, { llvm::Attribute::NoUnwind });
    declare("thd_destruct", voidTy, {}, { llvm::Attribute::NoUnwind });

    // Only reached once the write barrier's buffer is full
    declare("flush_wb", voidTy, {}, {
        llvm::Attribute::NoUnwind, llvm::Attribute::Cold, llvm::Attribute::WillReturn,
        llvm::Attribute::MustProgress
    });
    declare("end_program", voidTy, {}, { llvm::Attribute::NoUnwind });

    _mod.getOrInsertGlobal("gc_flag", _b.getInt8Ty());
    _gc_flag = _mod.getNamedGlobal("gc_flag");
    _gc_flag->setLinkage(llvm::GlobalVariable::PrivateLinkage);
    _gc_flag->setInitializer(_b.getInt8(0));
    _gc_flag->setAlignment(llvm::Align::Of<uint64_t>());
}

// -------------------------------------------------------------------------------------------------
// IR level helpers
// -------------------------------------------------------------------------------------------------

void runtime_bridge::build_ir_helpers() {
    auto& c = ctx();
    auto val = eslValType();
    auto voidTy = llvm::Type::getVoidTy(c);
    auto i1 = llvm::Type::getInt1Ty(c);
    auto i8 = llvm::Type::getInt8Ty(c);
    auto i64 = llvm::Type::getInt64Ty(c);
    auto ptr = llvm::PointerType::getUnqual(c);
    const auto priv = llvm::Function::PrivateLinkage;

    // ESL val to int
    auto toI64 = [&](llvm::Value* v) { return _b.CreatePtrToInt(v, _b.getInt64Ty()); };
    auto ft = [&](llvm::Type* ret, std::initializer_list<llvm::Type*> args) {
        return llvm::FunctionType::get(ret, args, false);
    };

    define_fn("encode_bool", ft(val, { i1 }), priv, [&](llvm::Function* f) {
        // MASK_SIGNATURE_BOOL | x
        auto arg = _b.CreateZExt(f->getArg(0), _b.getInt64Ty());
        _b.CreateRet(_b.CreateIntToPtr(_b.CreateOr(_b.getInt64(mask_signature_bool), arg), val));
    });
    define_fn("encode_null", ft(val, {}), priv, [&](llvm::Function*) {
        _b.CreateRet(_b.CreateIntToPtr(_b.getInt64(encodeNil()), val));
    });
    define_fn("decode_bool", ft(i1, { val }), priv, [&](llvm::Function* f) {
        // Known to be either MASK_SIGNATURE_TRUE or MASK_SIGNATURE_FALSE, so a compare is enough
        _b.CreateRet(_b.CreateICmpEQ(toI64(f->getArg(0)), _b.getInt64(mask_signature_true)));
    });
    define_fn("is_num", ft(i1, { val }), priv, [&](llvm::Function* f) {
        // (x & MASK_QNAN) != MASK_QNAN
        auto qnan = _b.getInt64(mask_qnan);
        _b.CreateRet(_b.CreateICmpNE(_b.CreateAnd(toI64(f->getArg(0)), qnan), qnan));
    });
    define_fn("is_bool", ft(i1, { val }), priv, [&](llvm::Function* f) {
        // (x & MASK_SIGNATURE_BOOL) == MASK_SIGNATURE_BOOL
        auto m = _b.getInt64(mask_signature_bool);
        _b.CreateRet(_b.CreateICmpEQ(m, _b.CreateAnd(m, toI64(f->getArg(0)))));
    });
    define_fn("is_null", ft(i1, { val }), priv, [&](llvm::Function* f) {
        _b.CreateRet(_b.CreateICmpEQ(toI64(f->getArg(0)), _b.getInt64(mask_signature_null)));
    });
    define_fn("is_obj", ft(i1, { val }), priv, [&](llvm::Function* f) {
        // (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ
        auto masked = _b.CreateAnd(toI64(f->getArg(0)), _b.getInt64(mask_signature));
        _b.CreateRet(_b.CreateICmpEQ(masked, _b.getInt64(mask_signature_obj)));
    });
    define_fn("is_truthy", ft(i1, { val }), priv, [&](llvm::Function* f) {
        // !((isBool(x) && !decodeBool(x)) || isNil(x))
        // x might not be a bool, but decoding it as one is just an icmp so that's fine
        auto arg = f->getArg(0);
        auto c1 = call("is_bool", arg);
        auto c2 = _b.CreateNot(call("decode_bool", arg));
        auto c3 = call("is_null", arg);
        _b.CreateRet(_b.CreateNot(_b.CreateOr(_b.CreateAnd(c1, c2), c3)));
    });
    define_fn("encode_obj", ft(val, { ptr, i64 }), priv, [&](llvm::Function* f) {
        // MASK_SIGNATURE_OBJ | type | (Int64)x
        auto cast = _b.CreatePtrToInt(f->getArg(0), _b.getInt64Ty());
        auto mask = _b.CreateOr(_b.getInt64(mask_signature_obj), f->getArg(1));
        _b.CreateRet(_b.CreateIntToPtr(_b.CreateOr(mask, cast), val));
    });
    define_fn("decode_obj", ft(ptr, { val }), priv, [&](llvm::Function* f) {
        // (rt_obj*)(x & MASK_PAYLOAD_OBJ)
        auto masked = _b.CreateAnd(toI64(f->getArg(0)), _b.getInt64(mask_payload_obj));
        _b.CreateRet(_b.CreateIntToPtr(masked, ptr));
    });
    define_fn("is_rt_type", ft(i1, { val, i8 }), priv, [&](llvm::Function* f) {
        // (x & (MASK_SIGNATURE | MASK_PAYLOAD_TYPE)) == (MASK_SIGNATURE_OBJ | expectedType)
        auto arg = toI64(f->getArg(0));
        auto mask = _b.getInt64(mask_signature | mask_payload_type);
        auto res = _b.CreateAnd(arg, mask, "val.type");
        auto expected = _b.CreateOr(
            _b.getInt64(mask_signature_obj), _b.CreateZExt(f->getArg(1), _b.getInt64Ty()), "expected"
        );
        _b.CreateRet(_b.CreateICmpEQ(res, expected, "is.obj"));
    });

    // Even though the inliner can figure out that these functions can be inlined and have no effects
    // not having attributes prevents earlier optimizations from getting the most out of their passes
    for (const char* name : { "encode_bool", "encode_null", "decode_bool", "is_num", "is_bool",
                              "is_null", "is_obj", "is_truthy", "encode_obj", "decode_obj",
                              "is_rt_type" }) {
        auto* helper = fn(name);
        add_attrs(helper, total_fn);
        // Since this is just arithmetic it can be speculatable
        helper->addFnAttr(llvm::Attribute::Speculatable);
        helper->addFnAttr(llvm::Attribute::AlwaysInline);
        helper->setMemoryEffects(llvm::MemoryEffects::none());
    }

    // Memory effects stay unknown on purpose: a safepoint is where the collector gets to move
    // objects and rewrite the heap slots pointing at them, so no heap load survives across one
    auto* poll = define_fn("safepoint_poll", ft(voidTy, {}), llvm::Function::ExternalLinkage,
        [&](llvm::Function* F) {
            auto runGCBB = llvm::BasicBlock::Create(c, "runGC", F);
            auto mergeBB = llvm::BasicBlock::Create(c, "merge", F);

            auto R15MD = llvm::MDNode::get(c, { llvm::MDString::get(c, "r15") });
            auto MDAsVal = llvm::MetadataAsValue::get(c, R15MD);
            llvm::Value* tcb = _b.CreateIntrinsic(
                _b.getInt64Ty(), llvm::Intrinsic::read_register, { MDAsVal }
            );
            tcb = _b.CreateIntToPtr(tcb, ptr);

            auto gep = _b.CreateConstInBoundsGEP1_64(i8, tcb, tcb_state_off, "tcb.state");
            // TODO: can we have some static check that the offset is 8?
            // its much better for the cpu to assume 8 byte alignment for atomic loads
            auto load = _b.CreateAlignedLoad(i8, gep, llvm::Align(8), "thd.state");
            load->setAtomic(llvm::AtomicOrdering::Monotonic);

            // A thread in RUNNING(=0) only leaves it involuntarily, so any nonzero state means stop
            auto cond = _b.CreateICmpNE(load, _b.getInt8(0));
            cond = _b.CreateIntrinsic(
                _b.getInt1Ty(), llvm::Intrinsic::expect, { cond, _b.getInt1(false) }
            );
            _b.CreateCondBr(cond, runGCBB, mergeBB);

            _b.SetInsertPoint(runGCBB);
            call("safepoint", {});
            _b.CreateRetVoid();

            _b.SetInsertPoint(mergeBB);
            _b.CreateRetVoid();
        });
    poll->addFnAttr(llvm::Attribute::NoUnwind);

    auto* isInstOf = define_fn("is_inst_of_class",
        ft(i1, { val, llvm::Type::getInt32Ty(c), llvm::Type::getInt32Ty(c) }),
        priv, [&](llvm::Function* F) {
            auto instVal = F->getArg(0);
            auto subclassIdxStart = F->getArg(1);
            auto subclassIdxEnd = F->getArg(2);

            auto cond1 = call(
                "is_rt_type", { instVal, _b.getInt8(+object::rt_type::INSTANCE) }
            );

            auto checkTypeBB = llvm::BasicBlock::Create(c, "checkClassType", F);
            auto notObjBB = llvm::BasicBlock::Create(c, "notObj", F);
            cond1 = _b.CreateIntrinsic(
                _b.getInt1Ty(), llvm::Intrinsic::expect, { cond1, _b.getInt1(true) });
            _b.CreateCondBr(cond1, checkTypeBB, notObjBB);

            _b.SetInsertPoint(checkTypeBB);
            auto klass = inst(instVal).klass();

            _b.CreateRet(_b.CreateAnd(
                _b.CreateICmpUGE(klass.hierarchy_start().load(), subclassIdxStart),
                _b.CreateICmpULE(klass.hierarchy_end().load(), subclassIdxEnd)
            ));

            _b.SetInsertPoint(notObjBB);
            _b.CreateRet(_b.getInt1(false));
        });
    add_attrs(isInstOf, total_fn);
    // Reads the instance's class pointer and that class' hierarchy range, neither of which is ever
    // written after the object is built, so `x is Animal` can be hoisted out of a loop
    isInstOf->setMemoryEffects(llvm::MemoryEffects::readOnly());

    auto* writeBarrier = define_fn("gc_write_barrier", ft(voidTy, { val }), priv, [&](llvm::Function* F) {
        auto flag = _b.CreateAlignedLoad(i8, _gc_flag, llvm::Align(8));
        flag->setAtomic(llvm::AtomicOrdering::Acquire);
        auto flagSet = _b.CreateICmpNE(flag, _b.getInt8(0));
        auto isObjV = call("is_obj", F->getArg(0));

        auto inactiveWB = llvm::BasicBlock::Create(c, "inactive", F);
        auto activeWB = llvm::BasicBlock::Create(c, "active", F);

        auto cond = _b.CreateAnd(flagSet, isObjV);
        cond = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, { cond, _b.getInt1(false) });
        _b.CreateCondBr(cond, activeWB, inactiveWB);

        _b.SetInsertPoint(inactiveWB);
        _b.CreateRetVoid();

        _b.SetInsertPoint(activeWB);

        auto R15MD = llvm::MDNode::get(c, { llvm::MDString::get(c, "r15") });
        auto MDAsVal = llvm::MetadataAsValue::get(c, R15MD);
        llvm::Value* tcb = _b.CreateIntrinsic(_b.getInt64Ty(), llvm::Intrinsic::read_register,
                                              { MDAsVal });
        tcb = _b.CreateIntToPtr(tcb, ptr);

        auto bufSlot = _b.CreateConstInBoundsGEP1_64(i8, tcb, tcb_wbbuf_off, "tcb.wbbuf");
        auto buf = _b.CreateAlignedLoad(ptr, bufSlot, llvm::Align(8), "wbbuf");

        // mark_buf::push is `_data[_cnt++] = obj`, so write at the current count and store back the incremented one
        auto cntPtr = _b.CreateConstInBoundsGEP1_64(i8, buf, markbuf_cnt_off, "wbbuf.cnt");
        auto cnt = _b.CreateAlignedLoad(i64, cntPtr, llvm::Align(8));
        auto dataPtr = _b.CreateConstInBoundsGEP1_64(i8, buf, markbuf_data_off, "wbbuf.data");
        auto slot = _b.CreateInBoundsGEP(ptr, dataPtr, cnt);

        _b.CreateAlignedStore(call("decode_obj", { F->getArg(0) }), slot, llvm::Align(8));
        auto next = _b.CreateAdd(cnt, _b.getInt64(1));
        _b.CreateAlignedStore(next, cntPtr, llvm::Align(8));

        auto full = _b.CreateICmpEQ(next, _b.getInt64(markbuf_capacity));
        full = _b.CreateIntrinsic(_b.getInt1Ty(), llvm::Intrinsic::expect, { full, _b.getInt1(false) });

        auto fullBuf = llvm::BasicBlock::Create(c, "full_buf", F);
        auto partialBuf = llvm::BasicBlock::Create(c, "partial_buf", F);
        _b.CreateCondBr(full, fullBuf, partialBuf);

        _b.SetInsertPoint(partialBuf);
        _b.CreateRetVoid();

        _b.SetInsertPoint(fullBuf);
        call("flush_wb", {});
        _b.CreateRetVoid();
    });
    writeBarrier->addFnAttr(llvm::Attribute::NoUnwind);

    auto* arrBarrier = define_fn("arr_write_barrier", ft(voidTy, { ptr, val }), priv, [&](llvm::Function* F) {
        auto isObjV = call("is_obj", F->getArg(1));

        auto isObjBB = llvm::BasicBlock::Create(c, "is.obj");
        auto noObjBB = llvm::BasicBlock::Create(c, "no.obj", F);
        _b.CreateCondBr(isObjV, isObjBB, noObjBB);

        _b.SetInsertPoint(noObjBB);
        _b.CreateRetVoid();
        F->insert(F->end(), isObjBB);
        _b.SetInsertPoint(isObjBB);

        RtArr::from_raw(*this, F->getArg(0)).storage().set_has_obj();
        _b.CreateRetVoid();
    });
    // No memory effects: the flag it sets lives on the storage object, which it reaches by loading a
    // pointer out of the array, so it isn't argmem. No nosync either, the store is a release
    add_attrs(arrBarrier, {
        llvm::Attribute::NoUnwind, llvm::Attribute::WillReturn, llvm::Attribute::MustProgress,
        llvm::Attribute::NoRecurse, llvm::Attribute::NoFree, llvm::Attribute::NoCallback
    });
    arrBarrier->addParamAttr(0, llvm::Attribute::NonNull);
}

// -------------------------------------------------------------------------------------------------
// Handle factories
// -------------------------------------------------------------------------------------------------
// TODO: can we remove the const cast somehow? its very ugly
RtString runtime_bridge::str(llvm::Value* val) const {
    return RtString::from_raw(const_cast<runtime_bridge&>(*this), call("decode_obj", val, "str.ptr"));
}
RtArr runtime_bridge::arr(llvm::Value* val) const {
    return RtArr::from_raw(const_cast<runtime_bridge&>(*this), call("decode_obj", val, "arr.ptr"));
}
RtClosure runtime_bridge::closure(llvm::Value* val) const {
    return RtClosure::from_raw(const_cast<runtime_bridge&>(*this), call("decode_obj", val, "closure.ptr"));
}
RtInst runtime_bridge::inst(llvm::Value* val) const {
    return RtInst::from_raw(const_cast<runtime_bridge&>(*this), call("decode_obj", val, "inst.ptr"));
}
CompClass runtime_bridge::klass(llvm::Value* rawPtr) const {
    return CompClass::from_raw(const_cast<runtime_bridge&>(*this), rawPtr);
}
RtArrStore runtime_bridge::arr_store(llvm::Value *rawPtr) const {
    return RtArrStore::from_raw(const_cast<runtime_bridge&>(*this), rawPtr);
}
