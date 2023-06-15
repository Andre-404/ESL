#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "valueHelpersInline.cpp"
#include "../Includes/fmt/format.h"
// Functions which the compiler calls, seperate from the native functions provided by the language as part of runtime library

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __attribute__((visibility("default")))
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
    return 0;
}
