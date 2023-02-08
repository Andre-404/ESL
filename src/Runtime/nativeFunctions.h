#pragma once
#include "../Includes/robinHood.h"
#include "../Objects/objects.h"

namespace runtime {
    struct BuiltinClass {
        // Only store raw native function pointers, at runtime an object::ObjBoundNative is created and the caller is bound to it
        robin_hood::unordered_map<string, object::ObjNativeFunc*> methods;
        string name;

        BuiltinClass(string _name) : name(_name) {}
    };

    enum class Builtin{
        NUMBER,
        STRING,
        ARRAY,
        INSTANCE,
        FILE,
        MUTEX
    };
    inline constexpr unsigned operator+ (Builtin const val) { return static_cast<byte>(val); }

    vector<object::ObjNativeFunc *> createNativeFuncs();

    robin_hood::unordered_map<string, uInt> createNativeNameTable(vector<object::ObjNativeFunc *>& natives);

    vector<BuiltinClass> createBuiltinClasses();
}

