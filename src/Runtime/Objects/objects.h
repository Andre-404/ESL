#pragma once
#include "../../Includes/unorderedDense.h"
#include "../../common.h"
#include <fstream>
#include <stdio.h>
#include <shared_mutex>
#include <future>
#include <thread>

namespace object {

    enum class ObjType {
        DEALLOCATED,
        STRING,
        CLOSURE,
        FREEVAR,
        CLASS,
        INSTANCE,
        ARRAY,
        HASH_MAP,
        FILE,
        MUTEX,
    };
    inline constexpr unsigned operator+ (ObjType const val) { return static_cast<byte>(val); }

    class Obj{
    public:
        int8_t GCInfo[2];
        byte type;

        size_t getSize();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        // This reroutes the new operator to take memory which the GC gives out
        void* operator new(const size_t size);
    };

    void runObjDestructor(object::Obj* obj);

    // This is a header which is followed by the bytes of the string
    class ObjString : public Obj {
    public:
        uint32_t size;
        char* str;

        bool compare(ObjString* other);

        bool compare(const string other);

        ObjString* concat(ObjString* other);

        static ObjString* createStr(char* str);

    };

    struct stringHash {
        uint64_t operator()(object::ObjString* str) const noexcept;
    };

    struct stringEQ
    {
        bool operator()(object::ObjString* x, object::ObjString* y) const {
            return x->compare(y);
        }
    };


    class ObjArray : public Obj {
    public:
        // Used to decrease marking speed, if an array is filled with eg. numbers there is no need to scan it for ptrs
        // TODO: this doesnt work
        uInt numOfHeapPtr;
        vector<Value> values;
        ObjArray();
        ObjArray(const size_t size);
    };

    class ObjFreevar : public Obj {
    public:
        ObjFreevar(Value val);
        Value val;
    };

    // Pointer to a compiled function
    using Function = char*;
    using CheckFieldFunc = int (*)(ObjString*);

    // Multiple closures with different freevars can point to the same function
    class ObjClosure : public Obj {
    public:
        // A function can have a maximum of 255 parameters and 255 upvalues
        byte arity;
        byte freevarCount;
        Function func;
        char* name;

        ObjFreevar** getFreevarArr();
    };

    class ObjClass : public Obj {
    public:
        const char* name;
        // Uses copy down inheritance, superclass ptr is still here for instanceof operator
        int classHierarchyStart;
        int classHierarchyEnd;
        CheckFieldFunc getMethod;
        CheckFieldFunc getField;
        uInt64 methodArrLen;
        uInt64 fieldsArrLen;
        // We use ObjClosure* instead of ObjClosure** for optimization purposes to avoid having to allocate
        // each ObjClosure individually and scatter it in memory
        ObjClosure* methods;

        ObjClass(string _name);
    };

    // ObjInstance is a header followed by array of values(fields)
    class ObjInstance : public Obj {
    public:
        uint32_t fieldArrLen;
        ObjClass* klass;

        Value* getFields();
    };

    class ObjHashMap : public Obj{
    public:
        ankerl::unordered_dense::map<object::ObjString*, Value> fields;
        ObjHashMap();
    };

    class ObjFile : public Obj {
    public:
        std::fstream stream;
        const string path;
        // 0: read, 1: write
        int openType;

        ObjFile(const string& path, int _openType);
        ~ObjFile();
    };

    // Language representation of a mutex object
    class ObjMutex : public Obj {
    public:
        std::shared_mutex mtx;

        ObjMutex();
    };


}