#pragma once
#include "../Includes/fmt/core.h"
#include "Objects/objects.h"
#include "Values/valueHelpers.h"
#include "Values/valueHelpersInline.h"
#include <iostream>

using namespace object;

#define EXPORT extern "C" DLLEXPORT

EXPORT Value print(void*, Value x){
    std::cout<< valueHelpers::toString(x)<<std::endl;
    return encodeNil();
}

EXPORT Value ms_since_epoch(void*){
    double duration = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
    return encodeNumber(duration);
}

EXPORT Value arr_push(void*, Value arr, Value top){
    asArray(arr)->push(top);
    return arr;
}

EXPORT Value input(void*){
    string in;
    std::getline(std::cin, in);
    return encodeObj(rt_string::create((char*)in.c_str()));
}

EXPORT Value random_num(void*){
    return encodeNumber(rand());
}

EXPORT Value as_number(void*, Value num){
    if (isNumber(num)) { return num; }
    if (!isString(num)){
        std::cerr << "Cannot convert value to number.\n"; 
        exit(64);
    }
    try {
        return encodeNumber(std::stod(asString(num)->get_str()));
    }
    catch (std::exception &e){
        std::cerr << fmt::format("Cannot convert \"{}\" to Number.\n", asString(num)->get_str());
        exit(64);
    }
}

EXPORT Value to_string(void*, Value val) {
    auto str = valueHelpers::toString(val);
    return encodeObj(rt_string::create((char*)str.c_str()));
}

// doggy?
EXPORT Value cpu_clock(void*){
    return encodeNumber(std::clock());
}

// doggy?
EXPORT Value clocks_per_sec(void*){
    return encodeNumber(CLOCKS_PER_SEC);
}

#undef EXPORT
