#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "Values/valueHelpers.h"
#include "Values/valueHelpersInline.cpp"
#include <csetjmp>
#include <stdarg.h>

#define EXPORT extern "C" DLLEXPORT

EXPORT Value print(ObjClosure* ptr, Value x){
    std::cout<< valueHelpers::toString(x)<<std::endl;
    return encodeNil();
}

EXPORT Value ms_since_epoch(ObjClosure* ptr){
    double duration = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
    return encodeNumber(duration);
}

EXPORT Value arr_push(ObjClosure* ptr, Value arr, Value top){
    asArray(arr)->values.push_back(top);
    return arr;
}

#undef EXPORT