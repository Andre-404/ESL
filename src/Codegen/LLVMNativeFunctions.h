#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "valueHelpersInline.cpp"
// Functions which the compiler calls, seperate from the native functions provided by the language as part of runtime library

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif
#define EXPORT extern "C" DLLEXPORT

EXPORT double asNum(Value x){
    if(isNumber(x)){
        return decodeNumber(x);
    }
    exit(64);
}

EXPORT void print(Value x){
    std::cout<< "Value is: "<< valueHelpers::toString(x)<<std::endl;
}

EXPORT Value createStr(char* ptr){
    string str(ptr);
    return encodeObj(ObjString::createStr(str));
}

EXPORT bool valueIsTrue(Value x){
    return !((isBool(x) && !decodeBool(x)) || isNil(x));
}

// Encode functions are temporary and should be moved to LLVM IR, inline version in C++ are to be used by the GC
EXPORT Value nativeEncodeBool(bool b){
    return encodeBool(b);
}