#pragma once
#include "objects.h"

namespace object {

    class string_interner{
        uint64_t largestStrSize = 0;
        ankerl::unordered_dense::set<rt_string*, rt_string::hash, rt_string::equality> interned;
        inline static string_interner* global = nullptr;
    public:
        // Not thread safe
        void intern(rt_string* str) {
            interned.insert(str);
            if(str->sz() > largestStrSize) largestStrSize = str->sz();
        }
        // Optimizes checking to avoid having to hash large strings
        rt_string* check_interned(rt_string* str) {
            if(str->sz() > largestStrSize) return str;
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