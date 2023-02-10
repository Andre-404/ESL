#pragma once
#include <utility>

#include "../Includes/robinHood.h"
#include "../Objects/objects.h"

namespace runtime {
    struct BuiltinMethod{
        object::NativeFn func;
        // Arity of -1 means that the native function takes in a variable number of arguments
        int arity;

        BuiltinMethod(object::NativeFn _func, int _arity){
            func = _func;
            arity = _arity;
        }
    };
    struct BuiltinClass {
        // Only store raw native function pointers, at runtime an object::ObjBoundNative is created and the caller is bound to it
        robin_hood::unordered_map<string, BuiltinMethod> methods;

        BuiltinClass() = default;
        BuiltinClass(BuiltinClass* parent){
            methods.insert(parent->methods.begin(), parent->methods.end());
        }
    };

    enum class Builtin{
        COMMON,
        STRING,
        ARRAY,
        FILE,
        MUTEX,
        FUTURE
    };
    inline constexpr unsigned operator+ (Builtin const val) { return static_cast<byte>(val); }

    vector<object::ObjNativeFunc *> createNativeFuncs();

    robin_hood::unordered_map<string, uInt> createNativeNameTable(vector<object::ObjNativeFunc *>& natives);

    vector<BuiltinClass> createBuiltinClasses();
}

