#pragma once
#include "../../Includes/unorderedDense.h"
#include "../../common.h"
#include <fstream>
#include <shared_mutex>
#include "../esl-gc-helpers.h"

namespace object {

    enum class ObjType {
        DEALLOCATED,
        STRING,
        CLOSURE,
        CLASS,
        INSTANCE,
        ARRAY,
        ARRAY_STORAGE_HEADER,
        HASH_MAP,
        FILE,
        MUTEX,
        CHANNEL,
        WAIT_GROUP
    };
    inline constexpr unsigned operator+ (ObjType const val) { return static_cast<byte>(val); }

    class Obj : public gc::managed{
    public:
        Obj(ObjType type, bool is_pinned) : gc::managed(+type, is_pinned ? gc::move_state::pinned : gc::move_state::none) {}

        ObjType type() { return static_cast<ObjType>(get_type_id()); }
        size_t getSize();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
    };

    void runObjDestructor(object::Obj* obj);

    // This is a header which is followed by the bytes of the string
    class ObjString : public Obj {
    public:
        ObjString() : Obj(ObjType::STRING, false) {}
        uint32_t size;

        char* get_str() const { return (char*)(this + 1);}

        bool compare(ObjString* other);

        bool compare(const string other);

        ObjString* concat(ObjString* other);

        static ObjString* createStr(char* str);

    };

    struct stringHash {
        uint64_t operator()(const object::ObjString* str) const noexcept;
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

        ObjArrayStorage() : Obj(ObjType::ARRAY_STORAGE_HEADER, false) {}

        inline Value* getData();

        static ObjArrayStorage* allocArray(uint32_t capacity);
    };

    class ObjArray : public Obj {
    public:
        byte containsObjects;
        uint32_t size;
        ObjArrayStorage* storage;

        ObjArray();
        ObjArray(size_t size);

        Value* getData();
        void push(Value item);
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

        ObjClosure() : Obj(ObjType::CLOSURE, false) {}

        Value* getFreevarArr();
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
    };

    // ObjInstance is a header followed by array of values(fields)
    class ObjInstance : public Obj {
    public:
        uint32_t fieldArrLen;
        ObjClass* klass;

        ObjInstance() : Obj(ObjType::INSTANCE, false) {}

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

        ObjMutex() : Obj(ObjType::MUTEX, true) {}
    };


}