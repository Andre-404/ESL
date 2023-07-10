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
        FUNC,
        ARRAY,
        CLOSURE,
        UPVALUE,
        CLASS,
        INSTANCE,
        BOUND_METHOD,
        HASH_MAP,
        FILE,
        MUTEX,
        FUTURE,
        RANGE
    };
    inline constexpr unsigned operator+ (ObjType const val) { return static_cast<byte>(val); }

    class Obj{
    public:
        byte type;
        bool marked;

        size_t getSize();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        //this reroutes the new operator to take memory which the GC gives out
        void* operator new(size_t size) {
            return memory::gc->alloc(size);
        }
    };


    // Pointer to a compiled function
    using Function = char*;

    // This is a header which is followed by the bytes of the string
    class ObjString : public Obj {
    public:
        string str;

        ObjString(string& _str);

        bool compare(ObjString* other);

        bool compare(string other);

        ObjString* concat(ObjString* other);

        static ObjString* createStr(string str);
    };

    class ObjArray : public Obj {
    public:
        vector<Value> values;
        // Used to decrease marking speed, if an array is filled with eg. numbers there is no need to scan it for ptrs
        uInt numOfHeapPtr;
        ObjArray();
        ObjArray(size_t size);
    };

    class ObjFunc : public Obj {
    public:
        Function func;
        char* name;
        // A function can have a maximum of 255 parameters
        int arity;
        ObjFunc(int _arity, Function _func);
    };

    class ObjUpval : public Obj {
    public:
        Value val;
        ObjUpval(Value& _value);
    };

    // Multiple closures with different upvalues can point to the same function
    class ObjClosure : public Obj {
    public:
        ObjFunc* func;
        ObjUpval** upvals;
        int upvalCount;
        ObjClosure(ObjFunc* _func, int upvalCount);
        ~ObjClosure();
    };

    // Parent classes use copy down inheritance, meaning all methods of a superclass are copied into the hash map of this class
    class ObjClass : public Obj {
    public:
        object::ObjString* name;
        // Uses copy down inheritance, but superclass ptr is still here for instanceof operator
        object::ObjClass* superclass;
        ankerl::unordered_dense::map<object::ObjString*, Obj*> methods;
        // Dummy map which has the names of defined fields already inserted, gets copied to each ObjInstance
        ankerl::unordered_dense::map<object::ObjString*, Value> fieldsInit;

        ObjClass(string _name, object::ObjClass* _superclass);
    };

    // Fields contains both public and private fields, private fields have a prefix '!' which cannot appear as part of an identifier
    // thus making sure that only the compiler can emit code to access a private member
    class ObjInstance : public Obj {
    public:
        ObjClass* klass;
        ankerl::unordered_dense::map<object::ObjString*, Value> fields;

        ObjInstance(ObjClass* _klass);
    };

    // Method bound to a specific instance of a class
    // as long as the method exists, the instance it's bound to won't be GC-ed
    class ObjBoundMethod : public Obj {
    public:
        Value receiver;
        ObjFunc* method;
        ObjBoundMethod(Value _receiver, ObjFunc* _method);
    };

    class ObjHashMap : public Obj{
    public:
        ankerl::unordered_dense::map<object::ObjString*, Value> fields;
        ObjHashMap();
    };

    class ObjFile : public Obj {
    public:
        std::fstream stream;
        string path;
        // 0: read, 1: write
        int openType;

        ObjFile(string& path, int _openType);
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
        double start;
        double end;
        bool isEndInclusive;

        ObjRange(double _start, double _end, bool _isEndInclusive);
    };
}