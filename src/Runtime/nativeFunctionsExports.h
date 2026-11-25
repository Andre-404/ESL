#pragma once
#include "../Includes/fmt/core.h"
#include "Objects/objects.h"
#include "Values/valueHelpers.h"
#include "Values/valueHelpersInline.h"
#include "esl-gc-helpers.h"
#include "LLVMHelperExports.h"
#include <iostream>

using namespace object;

#define EXPORT extern "C" DLLEXPORT

EXPORT Value print(void*, Value x){
    auto str = valueHelpers::toString(x);
    // The write can block on a full pipe, and the collector mustn't wait for this thread meanwhile
    gc::run_blocking([&]() { std::cout << str << std::endl; });
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
    gc::run_blocking([&]() {
        std::getline(std::cin, in);
    });
    return encodeObj(rt_string::create(in));
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
    return encodeObj(rt_string::create(str));
}

// doggy?
EXPORT Value cpu_clock(void*){
    return encodeNumber(std::clock());
}

// doggy?
EXPORT Value clocks_per_sec(void*){
    return encodeNumber(CLOCKS_PER_SEC);
}

static void type_check(bool ok, const char* msg, Value v) {
    if (!ok) rt_error(msg, +runtimeErrorType::WRONG_TYPE, v, 0, 0);
}

// cleanup(arg) runs on the cleanup runner some time after tracked becomes unreachable
EXPORT Value add_cleanup(void*, Value tracked, Value cleanup, Value arg) {
    type_check(isObj(tracked), "Expected an object to track, got '{}'.", tracked);
    type_check(isClosure(cleanup), "Expected a function as the cleanup, got '{}'.", cleanup);
    if (asClosure(cleanup)->arity() != 1)
        rt_error("Cleanup {} takes {} arguments, it must take {}.", +runtimeErrorType::ARG_CNT, cleanup, 1, 0);
    auto captures = arg == tracked;
    for (auto& slot : asClosure(cleanup)->get_env()) captures |= gc::read_ref(slot) == tracked;
    type_check(!captures, "A cleanup and its argument can't hold the object they clean up, it would never be collected.", tracked);
    gc::add_cleanup(decodeObj(tracked), cleanup, arg);
    return encodeNil();
}

EXPORT Value mtx_new(void*) {
    return encodeObj(gc::esl_make_gc<rt_mutex>(0));
}

EXPORT Value mtx_lock(void*, Value m) {
    type_check(isMutex(m), "Expected a mutex, got '{}'.", m);
    auto& mtx = asMutex(m)->get();
    if (!mtx.try_lock()) gc::run_blocking([&] { mtx.lock(); });
    return encodeNil();
}

EXPORT Value mtx_unlock(void*, Value m) {
    type_check(isMutex(m), "Expected a mutex, got '{}'.", m);
    asMutex(m)->get().unlock();
    return encodeNil();
}

#undef EXPORT
