#pragma once
#include "../MemoryManagment/garbageCollector.h"
#include "../Includes/unorderedDense.h"
#include <fstream>
#include <stdio.h>
#include <shared_mutex>
#include <future>

namespace object {

    enum class ObjType {
        STRING,
        ARRAY,
        CLOSURE,
        FREEVAR,
        CLASS,
        INSTANCE,
        HASH_MAP,
        FILE,
        MUTEX,
        FUTURE,
        RANGE,

        THUNK
    };
    inline constexpr unsigned operator+ (ObjType const val) { return static_cast<byte>(val); }

    class Obj{
    public:
        byte type;
        bool marked;

        size_t getSize();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        //this reroutes the new operator to take memory which the GC gives out
        void* operator new(const size_t size) {
            return memory::gc->alloc(size);
        }
    };


    // Pointer to a compiled function
    using Function = char*;
    using CheckFieldFunc = int (*)(ObjString*);

    // This is a header which is followed by the bytes of the string
    class ObjString : public Obj {
    public:
        uInt64 len;
        char* str;

        ObjString(char* _str);

        bool compare(ObjString* other);

        bool compare(const string other);

        ObjString* concat(ObjString* other);

        static ObjString* createStr(char* str);

        //this reroutes the new operator to take memory which the GC gives out
        void* operator new(size_t size, const int64_t strLen) {
            return memory::gc->alloc(size+strLen);
        }

    };

    class ObjArray : public Obj {
    public:
        vector<Value> values;
        // Used to decrease marking speed, if an array is filled with eg. numbers there is no need to scan it for ptrs
        uInt numOfHeapPtr;
        ObjArray();
        ObjArray(const size_t size);
    };

    class ObjFreevar : public Obj {
    public:
        Value val;
        ObjFreevar(const Value& _value);
    };

    // Multiple closures with different freevars can point to the same function
    class ObjClosure : public Obj {
    public:
        // A function can have a maximum of 255 parameters and 255 upvalues
        const byte arity;
        const byte freevarCount;
        const Function func;
        const char* name;
        ObjFreevar** freevars;
        ObjClosure(const Function _func, const int _arity, const char* _name, const int _freevarCount);
        ~ObjClosure();
    };

    class ObjClass : public Obj {
    public:
        const char* name;
        // Uses copy down inheritance, superclass ptr is still here for instanceof operator
        object::ObjClass* superclass;
        CheckFieldFunc getMethod;
        CheckFieldFunc getField;
        uInt64 methodArrLen;
        uInt64 fieldsArrLen;
        // We use ObjClosure* instead of ObjClosure** for optimization purposes to avoid having to allocate
        // each ObjClosure individually and scatter it in memory
        ObjClosure* methods;

        ObjClass(string _name, object::ObjClass* _superclass);

    };

    // ObjInstance is a header followed by array of values(fields)
    class ObjInstance : public Obj {
    public:
        ObjClass* klass;
        uInt64 fieldArrLen;
        Value* fields;

        ObjInstance(ObjClass* _klass, uInt64 _fieldsArrLen);

        //this reroutes the new operator to take memory which the GC gives out
        void* operator new(size_t size, const int64_t fieldsN) {
            return memory::gc->alloc(size+sizeof(Value)*fieldsN);
        }
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

    // Returned by "async func()" call, when the thread finishes it will populate returnVal and delete the vm
    class ObjFuture : public Obj {
    public:
        std::jthread thread;
        std::atomic<bool> done;
        Value val;

        ObjFuture(ObjClosure* func, int argc, Value* args);
        ~ObjFuture();
    };

    class ObjRange : public Obj{
    public:
        const double start;
        const double end;
        const bool isEndInclusive;

        ObjRange(const double _start, const double _end, const bool _isEndInclusive);
    };
}