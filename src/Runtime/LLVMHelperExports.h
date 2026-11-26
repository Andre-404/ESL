#pragma once
#include "../Includes/fmt/core.h"
#include "../Includes/rpmalloc/rpmalloc.h"
#include "Objects/objects.h"
#include "Values/valueHelpersInline.h"
#include "esl-gc-helpers.h"
#include "Objects/string-interner.h"
#include <csetjmp>
#include <stdarg.h>
#include "JIT/JIT.h"
#include <unwind.h>
#include <pthread.h>


// Functions which the compiler calls, separate from the native functions provided by the language as part of runtime library
#define EXPORT extern "C" DLLEXPORT

EXPORT NOINLINE void safepoint(){
    gc::exec_at_safepoint(gc::read_tcb());
}

enum class runtimeErrorType : uint8_t{
    WRONG_TYPE,
    WRONG_TYPE_BINARY,
    ARG_CNT,
    INST_FIELD,
    OUT_OF_BOUNDS
};
inline constexpr unsigned operator+ (runtimeErrorType const val) { return static_cast<byte>(val); }
// A bit hacky, but we only ever need a maximum of 3 values for errors, what does values are(ESL "Value"s, char ptrs..)
// is up to the error type to interpret
EXPORT void rt_error(const char* msg, uint8_t errType, uint64_t val1, uint64_t val2, uint64_t val3){
    string str(msg);
    switch(errType){
        case +runtimeErrorType::WRONG_TYPE: {
            auto type = valueHelpers::typeToStr(val1);
            str = fmt::vformat(msg, fmt::make_format_args(type));
            break;
        }
        case +runtimeErrorType::WRONG_TYPE_BINARY:{
            auto lhsType = valueHelpers::typeToStr(val1);
            auto rhsType = valueHelpers::typeToStr(val2);
            str = fmt::vformat(msg, fmt::make_format_args(lhsType, rhsType));
            break;
        }
        case +runtimeErrorType::ARG_CNT:{
            auto funcName = asClosure(val1)->name();
            auto funcArity = asClosure(val1)->arity();
            auto wrongArgCount = val2;
            str = fmt::vformat(msg, fmt::make_format_args(funcName, funcArity, wrongArgCount));
            break;
        }
        case +runtimeErrorType::INST_FIELD:{
            string instType = valueHelpers::typeToStr(val1);
            char* field = reinterpret_cast<char*>(val2);
            str = fmt::vformat(msg, fmt::make_format_args(instType, std::string_view(field)));
            break;
        }
        case +runtimeErrorType::OUT_OF_BOUNDS:{
            auto array = asArray(val1);
            auto arrCnt = array->get_data().size();
            // Hacky, but index is a signed 64bit num, and reinterpret cast doesn't let you convert uint->int
            auto index = *reinterpret_cast<int64_t*>(&val2);
            str = fmt::vformat(msg, fmt::make_format_args(arrCnt, index));
            break;
        }
    }
    std::cout<<str<<std::endl;
    _Unwind_Backtrace([](struct _Unwind_Context* context, void* arg){
        uint64_t ip = (uint64_t)_Unwind_GetIP(context);
        // This takes care of printing
        ESLJIT::getJIT().addressToFunc(ip);
        return _URC_NO_REASON;
    }, nullptr);
    exit(64);
}
// Both values are known to be strings
EXPORT Value str_add(Value lhs, Value rhs){
    return encodeObj(asString(lhs)->concat(asString(rhs)));
}

EXPORT Value str_cmp(Value lhs, Value rhs){
    return encodeBool(asString(lhs)->compare(asString(rhs)));
}

EXPORT Value alloc_arr(uint32_t arrSize){
    // The constructor sizes the storage to bit_ceil(arrSize), so the caller can safely write arrSize elements
    return encodeObj(gc::esl_make_gc<rt_arr>(0, arrSize));
}

EXPORT gc::tcb_handle* gc_init(uint8_t* gcFlag){
    gc::init_gc(*gcFlag);
    object::string_interner::init();
    return gc::create_tcb(nullptr, 0);
}

// TODO: right now we cant allocate this because it uses ankerl::unordered and that can cause issues
EXPORT Value create_hashmap(int nFields, ...){
    auto map = gc::esl_make_gc<rt_hashmap>(0);
    va_list ap;
    va_start(ap, nFields);
    for(int i=0; i<nFields; i++){
        auto k = asString(va_arg(ap, Value));
        map->insert_or_assign(k, va_arg(ap, Value));
    }
    va_end(ap);
    return encodeObj(map);
}

// The env comes back zeroed, the jitted caller fills it
EXPORT rt_closure* create_closure(void* fn, uint8_t arity, char* name, int upvalCount){
    return gc::esl_make_gc<rt_closure>(upvalCount*sizeof(Value), arity, upvalCount, fn, name);
}

EXPORT void gc_add_root(Value* ptr){
    gc::register_root(ptr);
}

// Slow paths of gc_read_val/gc_read_ptr
EXPORT [[clang::preserve_all]] Value gc_heal_val(Value* slot, Value seen) {
    return gc::heal_word(slot, seen);
}
EXPORT [[clang::preserve_all]] size_t gc_heal_ptr(size_t* slot, size_t seen) {
    return gc::heal_ptr(slot, seen);
}

EXPORT Value hashmap_get(rt_hashmap* map, rt_string* key){
    if (auto v = map->find(key); v) return gc::read_ref(*v);
    return encodeNil();
}

// Can't error since if str isn't in map it's inserted as a new value
EXPORT void hashmap_set(rt_hashmap* map, rt_string* key, Value v){
    map->insert_or_assign(key, v);
}

EXPORT rt_inst* create_inst(comp_class* klass, Value* fields) {
    return gc::esl_make_gc<rt_inst>(klass->fieldsArrLen*sizeof(Value), klass, fields);
}

EXPORT void intern_str(Value val){
    object::string_interner::get().intern(reinterpret_cast<rt_string*>(decodeObj(val)));
}

using wrapper = void*(*)(void*);

EXPORT void create_new_thd(wrapper llvmWrapper, int64_t* alloca, int64_t argc){
    auto mem = rpmalloc(argc*sizeof(Value));
    memcpy(mem, alloca, argc*sizeof(Value));
    auto tcb = gc::create_tcb(mem, argc);
    pthread_t p;
    // The tcb is already registered, and a thread that never runs would hold up every later pause
    if (auto err = pthread_create(&p, nullptr, llvmWrapper, tcb)) {
        fmt::print(stderr, "Failed to create a thread: {}\n", std::strerror(err));
        std::abort();
    }
    pthread_detach(p);
}

EXPORT void thd_init(uintptr_t* frameAddr, gc::tcb_handle* handle){
    gc::set_tcb(handle);
    gc::init_thd(handle, frameAddr);
    auto [args, _] = handle->take_start_args();
    if (args) rpfree(args);
}

EXPORT void thd_destruct(){
    gc::delete_tcb(gc::read_tcb());
}

EXPORT void flush_wb() {
    gc::push_wbbuf(gc::read_tcb());
}

// Main returning ends the program and kills the threads that haven't finished. _Exit skips the static
// destructors and the JIT teardown, which would pull memory out from under them
EXPORT void end_program() {
    gc::end_gc();
    // Not fflush(nullptr): that locks every stream, and a thread blocked in input() holds stdin's
    std::fflush(stdout);
    std::_Exit(0);
}

#undef EXPORT
