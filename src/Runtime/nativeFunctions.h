#pragma once
#include <utility>
#include "../Objects/objects.h"

namespace runtime {
    enum class Builtin{
        STRING,
        ARRAY,
        FILE,
        MUTEX,
        FUTURE,
        COMMON
    };
    inline constexpr unsigned operator+ (Builtin const val) { return static_cast<byte>(val); }

    vector<object::ObjNativeFunc *> createNativeFuncs();

    ankerl::unordered_dense::map<string, uInt> createNativeNameTable(vector<object::ObjNativeFunc *>& natives);

    vector<object::ObjClass*> createBuiltinClasses(object::ObjClass* baseClass);
}

