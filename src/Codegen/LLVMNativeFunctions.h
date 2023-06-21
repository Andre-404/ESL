#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "valueHelpersInline.cpp"
#include "../Includes/fmt/format.h"
#include <csetjmp>
#include <stdarg.h>
// Functions which the compiler calls, seperate from the native functions provided by the language as part of runtime library

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __attribute__((visibility("default")))
#endif
#define EXPORT extern "C" DLLEXPORT


EXPORT void stopThread(){
    // Get stack end(lowest address) and then spill the registers to the stack
    uintptr_t* stackEnd = getStackPointer();
    jmp_buf jb;
    setjmp(jb);
    memory::gc->setStackEnd(std::this_thread::get_id(), stackEnd);
    memory::gc->suspendMe();
}

EXPORT double asNum(Value x){
    if(isNumber(x)){
        return decodeNumber(x);
    }
    exit(64);
}

EXPORT void print(Value x){
    memory::gc->active = 1;
    if(memory::gc->active == 1) stopThread();
    std::cout<< "Value is: "<< valueHelpers::toString(x)<<std::endl;
}

EXPORT Value createStr(char* ptr){
    return encodeObj(ObjString::createStr(string(ptr)));
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

EXPORT void tyErrSingle(const char* ptr, const char* fileName, const int line, Value val){
    string str(ptr);
    string type = valueHelpers::typeToStr(val);

    size_t pos = str.find("{}");
    if(pos == str.npos)std::cout<< "Error formatting string for error output\n";
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
    if(pos == str.npos)std::cout<< "Error formatting string for error output\n";
    str.replace(pos, 2, rhsTy);


    std::cout<<str<<std::endl;
    exit(64);
}

EXPORT Value strAdd(Value lhs, Value rhs, const char* fileName, const int line){
    if(!isString(lhs) || !isString(rhs)) tyErrDouble("Operands must be numbers or strings, got '{}' and '{}'.", fileName, line, lhs, rhs);
    string temp = asString(lhs)->str + asString(rhs)->str;
    return encodeObj(object::ObjString::createStr(temp));
}

EXPORT Value createArr(int arrSize){
    return encodeObj(new object::ObjArray(arrSize));
}

EXPORT Value* getArrPtr(Value arr){
    return asArray(arr)->values.data();
}

EXPORT bool gcSafepoint(){
    return memory::gc->active == 1;
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

