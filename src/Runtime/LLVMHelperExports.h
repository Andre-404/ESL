#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "Values/valueHelpersInline.cpp"
#include "MemoryManagment/garbageCollector.h"
#include <csetjmp>
#include <stdarg.h>
#include "../Includes/rpmalloc/rpmalloc.h"

#define __READ_RBP() __asm__ volatile("movq %%rbp, %0" : "=r"(__rbp))
#define __READ_RSP() __asm__ volatile("movq %%rsp, %0" : "=r"(__rsp))
// Functions which the compiler calls, separate from the native functions provided by the language as part of runtime library
#define EXPORT extern "C"

EXPORT NOINLINE void stopThread(memory::ThreadLocalData* threadData){
    if(!memory::gc) return;
    // Get stack end(lowest address) and then spill the registers to the stack
    jmp_buf jb;
    setjmp(jb);
    uintptr_t* stackEnd;
    __asm__ volatile("movq %%rsp, %0" : "=r"(stackEnd));

    memory::gc->suspendThread(std::this_thread::get_id(), stackEnd, memory::getLocalArena(threadData));
}

EXPORT double asNum(Value x){
    if(isNumber(x)){
        return decodeNumber(x);
    }
    exit(64);
}

EXPORT void runtimeErr(const char* ptr, char**args, int argSize){
    string str(ptr);
    size_t pos = 0;
    for(uInt i = 0; i < argSize; i++){
        pos = str.find("{}", pos);
        if(pos == str.npos){
            std::cout<< "Error\n";
        }
        str.replace(pos, 2, args[i]);
        pos += strlen(args[i]);
    }

    std::cout<<str<<std::endl;
    exit(64);
}

// TODO: Probably use printf for this since it's easier to do the format
EXPORT void tyErrSingle(const char* ptr, const char* fileName, const int line, Value val){
    string str(ptr);
    string type = valueHelpers::typeToStr(val);

    size_t pos = str.find("{}");
    if(pos == str.npos) std::cout<< "Error formatting string for error output\n";
    str.replace(pos, 2, type);

    std::cout<<str<<std::endl;
    exit(64);
}

EXPORT void tyErrDouble(const char* ptr, const char* fileName, const int line, Value lhs, Value rhs){
    string str(ptr);
    string lhsTy = valueHelpers::typeToStr(lhs);
    string rhsTy = valueHelpers::typeToStr(rhs);

    // Doesn't look pretty, but better than a loop
    size_t pos = str.find("{}");
    if(pos == str.npos)std::cout<< "Error formatting string for error output\n";
    str.replace(pos, 2, lhsTy);
    pos += lhsTy.length();

    pos = str.find("{}", pos);
    if(pos == str.npos) std::cout<< "Error formatting string for error output\n";
    str.replace(pos, 2, rhsTy);


    std::cout<<str<<std::endl;
    exit(64);
}

// Both values are known to be strings
EXPORT Value strAdd(memory::ThreadLocalData* threadData, Value lhs, Value rhs){
    return encodeObj(asString(lhs)->concat(asString(rhs), memory::getLocalArena(threadData)));
}

EXPORT Value strTryAdd(memory::ThreadLocalData* threadData, Value lhs, Value rhs, const char* fileName, const int line){
    if(!isString(lhs) || !isString(rhs)) tyErrDouble("Operands must be numbers or strings, got '{}' and '{}'.", fileName, line, lhs, rhs);
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
