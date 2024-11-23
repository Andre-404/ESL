#pragma once
#include "../../Includes/unorderedDense.h"
#include "../../common.h"
#include <fstream>
#include <stdio.h>
#include <shared_mutex>
#include <future>
#include <thread>

namespace memory{
    class ThreadArena;
}

namespace object {

    enum class ObjType {
        DEALLOCATED,
        STRING,
        CLOSURE,
        FREEVAR,
        CLASS,
        INSTANCE,
        ARRAY,
        ARRAY_STORAGE_HEADER,
        HASH_MAP,
        FILE,
        MUTEX,
        CHANNEL,
    };
    inline constexpr unsigned operator+ (ObjType const val) { return static_cast<byte>(val); }

    class Obj{
    public:
        int8_t GCInfo[2];
        byte type;

        size_t getSize();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        // This reroutes the new operator to take memory which the GC gives out
        void* operator new(const size_t size, memory::ThreadArena& allocator);
    };

    void runObjDestructor(object::Obj* obj);

    // This is a header which is followed by the bytes of the string
    class ObjString : public Obj {
    public:
        uint32_t size;
        char* str;

        bool compare(ObjString* other);

        bool compare(const string other);

        ObjString* concat(ObjString* other, memory::ThreadArena& allocator);

        static ObjString* createStr(char* str, memory::ThreadArena& allocator);

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

    // Header for array storage
    class ObjArrayStorage : public Obj{
    public:
        uint32_t capacity;

        inline Value* getData();

        static ObjArrayStorage* allocArray(uint32_t capacity, memory::ThreadArena& allocator);
    };

    class ObjArray : public Obj {
    public:
        byte containsObjects;
        uint32_t size;
        ObjArrayStorage* storage;

        ObjArray(memory::ThreadArena& allocator);
        ObjArray(const size_t size, memory::ThreadArena& allocator);

        Value* getData();
        void push(Value item, memory::ThreadArena& allocator);
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
        uint16_t methodArrLen;
        uint16_t fieldsArrLen;
        uint32_t classHierarchyStart;
        uint32_t classHierarchyEnd;
        const char* name;
        CheckFieldFunc getMethod;
        CheckFieldFunc getField;

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