#pragma once
#include "objects.h"

namespace object {

    class string_interner{
        uint64_t largestStrSize = 0;
        ankerl::unordered_dense::set<ObjString*, stringHash, stringEQ> interned;
        inline static string_interner* global = nullptr;
    public:
        // Not thread safe
        void intern(ObjString* str) {
            interned.insert(str);
            if(str->size > largestStrSize) largestStrSize = str->size;
        }
        // Optimizes checking to avoid having to hash large strings
        ObjString* check_interned(ObjString* str) {
            if(str->size > largestStrSize) return str;
            auto it = interned.find(str);
            if(it != interned.end()) return *it;
            return str;
        }
        static void init() {
            string_interner::global = new string_interner {};
        }

        static string_interner& get() {
            return *string_interner::global;
        }
    };


}