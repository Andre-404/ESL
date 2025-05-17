#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "Values/valueHelpersInline.cpp"
#include "MemoryManagment/garbageCollector.h"
#include <csetjmp>
#include <stdarg.h>
#include "../Includes/rpmalloc/rpmalloc.h"
#include <format>
#include "JIT/JIT.h"
#include <unwind.h>
#include <pthread.h>


#define __READ_RBP() __asm__ volatile("movq %%rbp, %0" : "=r"(__rbp))
#define __READ_RSP() __asm__ volatile("movq %%rsp, %0" : "=r"(__rsp))
// Functions which the compiler calls, separate from the native functions provided by the language as part of runtime library
#define EXPORT extern "C" DLLEXPORT

EXPORT NOINLINE void stopThread(memory::ThreadLocalData* threadData){
    if(!memory::gc) return;
    // Get stack end(lowest address) and then spill the registers to the stack
    jmp_buf jb;
    setjmp(jb);
    uintptr_t* stackEnd;
    __asm__ volatile("movq %%rsp, %0" : "=r"(stackEnd));

    memory::gc->suspendThread(std::this_thread::get_id(), stackEnd, memory::getLocalArena(threadData));
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
EXPORT Value strAdd(memory::ThreadLocalData* threadData, Value lhs, Value rhs){
    return encodeObj(asString(lhs)->concat(asString(rhs), memory::getLocalArena(threadData)));
}

EXPORT Value strCmp(Value lhs, Value rhs){
    return encodeBool(asString(lhs)->compare(asString(rhs)));
}

EXPORT Value createArr(memory::ThreadLocalData* threadData, uint32_t arrSize){
    memory::ThreadArena& allocator = memory::getLocalArena(threadData);
    return encodeObj(new(allocator) object::ObjArray(arrSize, allocator));
}

EXPORT Value* getArrPtr(Value arr){
    return asArray(arr)->getData();
}

EXPORT int64_t getArrSize(Value arr){
    return asArray(arr)->size;
}

EXPORT void gcInit(uint64_t* gcFlag){
    memory::gc = new memory::GarbageCollector(*gcFlag);
}

// hashMap is guaranteed to be an ObjHashMap, str is guaranteed to be an ObjString
EXPORT Value createHashMap(memory::ThreadLocalData* threadData, int nFields, ...){
    object::ObjHashMap* map = new(memory::getLocalArena(threadData)) object::ObjHashMap();
    va_list ap;
    va_start(ap, nFields);
    for(int i=0; i<nFields; i++){
        object::ObjString* str = asString(va_arg(ap, Value));
        map->fields.insert_or_assign(str, va_arg(ap, Value));
    }
    va_end(ap);
    return encodeObj(map);
}

EXPORT Value createClosure(memory::ThreadLocalData* threadData, char* fn, int arity, char* name, int upvalCount, ...){
    ObjClosure* closure = static_cast<ObjClosure *>(
            memory::getLocalArena(threadData).alloc(sizeof(ObjClosure) + upvalCount*sizeof(ObjFreevar*)));
    closure->arity = arity;
    closure->name = name;
    closure->func = fn;
    closure->freevarCount = upvalCount;
    closure->type = +ObjType::CLOSURE;
    va_list ap;
    va_start(ap, upvalCount);
    for(int i=0; i<upvalCount; i++){
        closure->getFreevarArr()[i] = va_arg(ap, object::ObjFreevar*);
    }
    va_end(ap);
    return encodeObj(closure);
}

EXPORT void addGCRoot(Value* ptr){
    memory::gc->addGlobalRoot(ptr);
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
    map->fields.insert_or_assign(str, v);
}

EXPORT Obj* gcAlloc(memory::ThreadLocalData* threadData, int64_t bytes){
    Obj* ptr = (Obj*)memory::getLocalArena(threadData).alloc(bytes);
    ptr->type = +object::ObjType::DEALLOCATED;
    return ptr;
}

EXPORT void gcInternStr(Value val){
    // Known to be an ObjString
    ObjString* ptr = reinterpret_cast<ObjString*>(decodeObj(val));
    memory::gc->interned.internString(ptr);
}

using wrapper = void*(*)(void*);

EXPORT void createNewThread(wrapper llvmWrapper, int64_t* alloca){
    pthread_t p;
    pthread_create(&p, nullptr, llvmWrapper, alloca);
}

EXPORT void threadInit(uintptr_t* frameAddr){
    memory::gc->addStackStart(std::this_thread::get_id(), frameAddr);
}

EXPORT void threadDestruct(memory::ThreadLocalData* threadData){
    memory::deleteLocalArena(threadData);
    memory::gc->removeStackStart(std::this_thread::get_id());
}

#undef EXPORT
