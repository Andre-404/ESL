#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "../Includes/rpmalloc/rpmalloc.h"
#include "Values/valueHelpersInline.cpp"
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
EXPORT void runtimeError(const char* msg, uint8_t errType, uint64_t val1, uint64_t val2, uint64_t val3){
    string str(msg);
    switch(errType){
        case +runtimeErrorType::WRONG_TYPE: {
            string type = valueHelpers::typeToStr(val1);
            str = fmt::vformat(msg, fmt::make_format_args(type));
            break;
        }
        case +runtimeErrorType::WRONG_TYPE_BINARY:{
            string lhsType = valueHelpers::typeToStr(val1);
            string rhsType = valueHelpers::typeToStr(val2);
            str = fmt::vformat(msg, fmt::make_format_args(lhsType, rhsType));
            break;
        }
        case +runtimeErrorType::ARG_CNT:{
            string funcName = asClosure(val1)->name;
            uint8_t funcArity = asClosure(val1)->arity;
            uint64_t wrongArgCount = val2;
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
            ObjArray* array = asArray(val1);
            uint64_t arrCnt = array->size;
            // Hacky, but index is a signed 64bit num, and reinterpret cast doesn't let you convert uint->int
            int64_t index = *reinterpret_cast<int64_t*>(&val2);
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
EXPORT Value strAdd(Value lhs, Value rhs){
    return encodeObj(asString(lhs)->concat(asString(rhs)));
}

EXPORT Value strCmp(Value lhs, Value rhs){
    return encodeBool(asString(lhs)->compare(asString(rhs)));
}

EXPORT Value createArr(uint32_t arrSize){
    auto ptr = gc::allocate(sizeof(ObjArray));
    return encodeObj(new(ptr) ObjArray(arrSize));
}

EXPORT Value* getArrPtr(Value arr){
    return asArray(arr)->getData();
}

EXPORT int64_t getArrSize(Value arr){
    return asArray(arr)->size;
}

EXPORT gc::tcb_handle* gcInit(uint8_t* gcFlag){
    gc::init_gc(*gcFlag);
    object::string_interner::init();
    return gc::create_tcb(nullptr, 0);
}

// TODO: right now we cant allocate this because it uses ankerl::unordered and that can cause issues
EXPORT Value createHashMap(int nFields, ...){
    auto map = new(gc::allocate(sizeof(ObjHashMap))) object::ObjHashMap();
    va_list ap;
    va_start(ap, nFields);
    for(int i=0; i<nFields; i++){
        object::ObjString* str = asString(va_arg(ap, Value));
        map->fields.insert_or_assign(str, va_arg(ap, Value));
    }
    va_end(ap);
    return encodeObj(map);
}

EXPORT Value createClosure(char* fn, int arity, char* name, int upvalCount, ...){
    auto ptr = gc::allocate(sizeof(ObjClosure) + upvalCount*sizeof(Value));
    ObjClosure* closure = new(ptr) ObjClosure();
    closure->arity = arity;
    closure->name = name;
    closure->func = fn;
    closure->freevarCount = upvalCount;
    va_list ap;
    va_start(ap, upvalCount);
    for(int i=0; i<upvalCount; i++){
        closure->getFreevarArr()[i] = va_arg(ap, Value);
    }
    va_end(ap);
    return encodeObj(closure);
}

EXPORT void addGCRoot(Value* ptr){
    gc::register_root(ptr);
}

EXPORT Value hashmapGetV(ObjHashMap* map, ObjString* str){
    auto it = map->fields.find(str);
    if(it == map->fields.end()) {
        // TODO: error
    }
    return it->second;
}

// Can't error since if str isn't in map it's inserted as a new value
EXPORT void hashmapSetV(ObjHashMap* map, ObjString* str, Value v){
    auto it = map->fields.find(str);
    if(it == map->fields.end()) map->fields.insert_or_assign(str, v);
    else it->second = v;
}

// TODO: think about if this fucks up the gc in some strange way? related to marking
// Concern: allocate -> some other thread scans its stack and finds us -> marks us -> tries to trace us -> shit happens
// Actually, this isn't an issue? since mark bits only get updated on next allocation
EXPORT Obj* gcAlloc(int64_t bytes){
    return new(gc::alloc(bytes, gc::read_tcb())) Obj(ObjType::DEALLOCATED, false);
}

EXPORT void gcInternStr(Value val){
    // Known to be an ObjString
    auto ptr = reinterpret_cast<ObjString*>(decodeObj(val));
    object::string_interner::get().intern(ptr);
}

using wrapper = void*(*)(void*);

EXPORT void createNewThread(wrapper llvmWrapper, int64_t* alloca, int64_t argc){
    auto mem = rpmalloc(argc*sizeof(Value));
    memcpy(mem, alloca, argc*sizeof(Value));
    auto tcb = gc::create_tcb(mem, argc);
    pthread_t p;
    pthread_create(&p, nullptr, llvmWrapper, tcb);
}

EXPORT void threadInit(uintptr_t* frameAddr, gc::tcb_handle* handle){
    gc::set_tcb(handle);
    gc::init_thd(handle, frameAddr);
    auto [args, _] = handle->take_start_args();
    if (args) rpfree(args);
}

EXPORT void threadDestruct(){
    gc::delete_tcb(gc::read_tcb());
}

EXPORT void flush_wb() {
    gc::push_wbbuf(gc::read_tcb());
}

EXPORT void endProgram() {
    gc::end_gc();
}

#undef EXPORT
