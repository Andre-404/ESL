#pragma once
#include "../codegen/codegenDefs.h"
#include "../MemoryManagment/garbageCollector.h"
#include "../Includes/unorderedDense.h"
#include "../Includes/customAllocator.h"
#include <fstream>
#include <stdio.h>
#include <shared_mutex>
#include <future>

namespace runtime {
	class VM;
	class Thread;
}

namespace object {

	enum class ObjType {
		STRING,
		FUNC,
		NATIVE,
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

	class Obj{
	public:
		ObjType type;
		bool marked;

		virtual string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) = 0;
		virtual void trace() = 0;
		virtual uInt64 getSize() = 0;
		virtual ~Obj() = default;

		//this reroutes the new operator to take memory which the GC gives out
		void* operator new(size_t size) {
			return memory::gc.alloc(size);
		}
	};

	// Pointer to a native C++ function
	using NativeFn = void(*)(runtime::Thread* thread, int8_t argCount);
    using NativeMethod = void(*)(runtime::Thread* thread, int8_t argCount);

	// This is a header which is followed by the bytes of the string
	class ObjString : public Obj {
	public:
		string str;

		ObjString(string& _str);
		~ObjString() {}

		bool compare(ObjString* other);

		bool compare(string other);

		ObjString* concat(ObjString* other);

        static ObjString* createStr(string str);

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	class ObjArray : public Obj {
	public:
		vector<Value> values;
		// Used to decrease marking speed, if an array is filled with eg. numbers there is no need to scan it for ptrs
		uInt numOfHeapPtr;
		ObjArray();
		ObjArray(size_t size);
		~ObjArray() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	class ObjFunc : public Obj {
	public:
		uInt64 bytecodeOffset;
		uInt64 constantsOffset;
		string name;
		// A function can have a maximum of 255 parameters
		byte arity;
		int upvalueCount;
		ObjFunc();
		~ObjFunc() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	class ObjNativeFunc : public Obj {
	public:
		NativeFn func;
        // Arity of -1 means that the native function takes in a variable number of arguments
		int8_t arity;
        // For debugging purposes
        const char* name;

		ObjNativeFunc(NativeFn _func, int8_t _arity, const char* _name);
		~ObjNativeFunc() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	class ObjUpval : public Obj {
	public:
		Value val;
		ObjUpval(Value& _value);
		~ObjUpval() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	// Multiple closures with different upvalues can point to the same function
	class ObjClosure : public Obj {
	public:
		ObjFunc* func;
		vector<ObjUpval*> upvals;
		ObjClosure(ObjFunc* _func);
		~ObjClosure() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

    typedef Obj* Method;
	// Parent classes use copy down inheritance, meaning all methods of a superclass are copied into the hash map of this class
	class ObjClass : public Obj {
	public:
        object::ObjString* name;
        // Uses copy down inheritance, but superclass ptr is still here for instanceof operator
        object::ObjClass* superclass;
        ankerl::unordered_dense::map<object::ObjString*, Method> methods;
        // Dummy map which has the names of defined fields already inserted, gets copied to each ObjInstance
        ankerl::unordered_dense::map<object::ObjString*, Value> fieldsInit;

		ObjClass(string _name, object::ObjClass* _superclass);
		~ObjClass() {}

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

    // Fields contains both public and private fields, private fields have a prefix '!' which cannot appear as part of an identifier
    // thus making sure that only the compiler can emit code to access a private member
	class ObjInstance : public Obj {
	public:
		ObjClass* klass;
		ankerl::unordered_dense::map<object::ObjString*, Value> fields;

		ObjInstance(ObjClass* _klass);
		~ObjInstance() = default;

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

    // Method bound to a specific instance of a class
    // as long as the method exists, the instance it's bound to won't be GC-ed
    class ObjBoundMethod : public Obj {
    public:
        Value receiver;
        Method method;
        ObjBoundMethod(Value _receiver, Method _method);
        ~ObjBoundMethod() = default;

        void trace();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        uInt64 getSize();
    };

    class ObjHashMap : public Obj{
    public:
        ankerl::unordered_dense::map<object::ObjString*, Value> fields;
        ObjHashMap();
        ~ObjHashMap() = default;

        void trace();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        uInt64 getSize();
    };

	class ObjFile : public Obj {
	public:
		std::fstream stream;
		string path;
        // 0: read, 1: write
        int openType;

		ObjFile(string& path, int _openType);
		~ObjFile();

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	// Language representation of a mutex object
	class ObjMutex : public Obj {
    public:
		std::shared_mutex mtx;

		ObjMutex();
		~ObjMutex();

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

	// Returned by "async func()" call, when the thread finishes it will populate returnVal and delete the vm
	class ObjFuture : public Obj {
	public:
		std::future<void> fut;
		Value val;
		runtime::Thread* thread;

		ObjFuture(runtime::Thread* t);
		~ObjFuture();

		void startParallelExecution();

		void trace();
		string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
		uInt64 getSize();
	};

    class ObjRange : public Obj{
    public:
        double start;
        double end;
        bool isEndInclusive;

        ObjRange(double _start, double _end, bool _isEndInclusive);
        ~ObjRange();

        void trace();
        string toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack);
        uInt64 getSize();
    };
}