#pragma once
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "Values/valueHelpers.h"
#include "Values/valueHelpersInline.cpp"
#include <iostream>

using namespace object;

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
    asArray(arr)->containsObjects |= isObj(top);
    asArray(arr)->push(top, memory::getLocalArena());
    return arr;
}

EXPORT Value input(ObjClosure* ptr){
    string in;
    std::getline(std::cin, in);
    return encodeObj(ObjString::createStr((char*)in.c_str(), memory::getLocalArena()));
}

EXPORT Value random_num(ObjClosure* ptr){
    return encodeNumber(rand());
}

EXPORT Value as_number(ObjClosure* ptr, Value num){
    if (isNumber(num)) { return num; }
    if (!isString(num)){
        std::cerr << "Cannot convert value to number.\n"; 
        exit(64);
    }
    try {
        return encodeNumber(std::stod(asString(num)->str));
    }
    catch (std::exception &e){
        std::cerr << fmt::format("Cannot convert \"{}\" to Number.\n", asString(num)->str);
        exit(64);
    }
}

// doggy?
EXPORT Value cpu_clock(ObjClosure* ptr){
    return encodeNumber(std::clock());
}

// doggy?
EXPORT Value clocks_per_sec(ObjClosure* ptr){
    return encodeNumber(CLOCKS_PER_SEC);
}

#undef EXPORT
