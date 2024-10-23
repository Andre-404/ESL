#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "Values/valueHelpersInline.cpp"
#include "MemoryManagment/garbageCollector.h"
#include <csetjmp>
#include <stdarg.h>
#include "unwind.h"
#include <any>

#define __READ_RBP() __asm__ volatile("movq %%rbp, %0" : "=r"(__rbp))
#define __READ_RSP() __asm__ volatile("movq %%rsp, %0" : "=r"(__rsp))
// Functions which the compiler calls, separate from the native functions provided by the language as part of runtime library
#define EXPORT extern "C" DLLEXPORT


EXPORT NOINLINE void stopThread(){
    if(!memory::gc) return;
    // Get stack end(lowest address) and then spill the registers to the stack
    jmp_buf jb;
    setjmp(jb);
    uintptr_t* stackEnd;
    __asm__ volatile("movq %%rsp, %0" : "=r"(stackEnd));

    memory::gc->suspendThread(std::this_thread::get_id(), stackEnd, memory::getLocalArena());
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
EXPORT Value strAdd(Value lhs, Value rhs){
    return encodeObj(asString(lhs)->concat(asString(rhs)));
}

EXPORT Value strTryAdd(Value lhs, Value rhs, const char* fileName, const int line){
    if(!isString(lhs) || !isString(rhs)) tyErrDouble("Operands must be numbers or strings, got '{}' and '{}'.", fileName, line, lhs, rhs);
    return encodeObj(asString(lhs)->concat(asString(rhs)));
}

EXPORT Value strCmp(Value lhs, Value rhs){
    return encodeBool(asString(lhs)->compare(asString(rhs)));
}

EXPORT Value createArr(uint32_t arrSize){
    return encodeObj(new object::ObjArray(arrSize));
}

EXPORT Value* getArrPtr(Value arr){
    return asArray(arr)->values.data();
}

EXPORT int64_t getArrSize(Value arr){
    return asArray(arr)->values.size();
}

EXPORT void gcInit(uint64_t* gcFlag, uintptr_t* frameAddr){
    memory::gc = new memory::GarbageCollector(*gcFlag);
    memory::gc->addStackStart(std::this_thread::get_id(), frameAddr);
}

// hashMap is guaranteed to be an ObjHashMap, str is guaranteed to be an ObjString
EXPORT Value createHashMap(int nFields, ...){
    object::ObjHashMap* map = new object::ObjHashMap();
    va_list ap;
    va_start(ap, nFields);
    for(int i=0; i<nFields; i++){
        object::ObjString* str = asString(va_arg(ap, Value));
        map->fields.insert_or_assign(str, va_arg(ap, Value));
    }
    va_end(ap);
    return encodeObj(map);
}

EXPORT object::ObjFreevar* createFreevar(){
    Value tmp = encodeNil();
    return new object::ObjFreevar(tmp);
}

EXPORT object::ObjFreevar* getFreevar(Value closure, int index){
    object::ObjClosure* cl = asClosure(closure);
    return cl->freevars[index];
}

EXPORT Value createClosure(char* fn, int arity, char* name, int upvalCount, ...){
    object::ObjClosure* closure = new object::ObjClosure(fn, arity, name, upvalCount);
    va_list ap;
    va_start(ap, upvalCount);
    for(int i=0; i<upvalCount; i++){
        closure->freevars[i] = va_arg(ap, object::ObjFreevar*);
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

EXPORT Value arrayGetV(ObjArray* arr, Value num){
    int64_t n = floor(decodeNumber(num));
    if(n < 0 || n >= arr->values.size()){
        // TODO: error
    }
    return arr->values[n];
}

EXPORT void arraySetV(ObjArray* arr, Value num, Value v){
    int64_t n = floor(decodeNumber(num));
    if(n < 0 || n >= arr->values.size()){
        // TODO: error
    }
    arr->values[n] = v;
}

EXPORT Obj* gcAlloc(int bytes){
    Obj* ptr = (Obj*)memory::getLocalArena().alloc(bytes);
    ptr->type = +object::ObjType::DEALLOCATED;
    return ptr;
}

EXPORT void gcInternStr(Value val){
    // Known to be an ObjString
    ObjString* ptr = reinterpret_cast<ObjString*>(decodeObj(val));
    memory::gc->interned[ptr->str] = ptr;
}


#undef EXPORT
