#pragma once
#include <utility>
#include "../Objects/objects.h"

namespace runtime {
    struct BuiltinMethod{
        object::NativeFn func;
        // Arity of -1 means that the native function takes in a variable number of arguments
        int8_t arity;

        BuiltinMethod(object::NativeFn _func, int8_t _arity){
            func = _func;
            arity = _arity;
        }
    };
    struct BuiltinClass {
        // Only store raw native function pointers, at runtime an object::ObjBoundNative is created and the caller is bound to it
        ankerl::unordered_dense::map<object::ObjString*, BuiltinMethod> methods;

        BuiltinClass() = default;
        BuiltinClass(BuiltinClass* parent){
            methods.insert(parent->methods.begin(), parent->methods.end());
        }
    };

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

