#pragma once
#include "../common.h"
#include <mutex>
#include <atomic>
#include "../Includes/unorderedDense.h"

namespace runtime {
	class VM;
}

namespace compileCore {
	class Compiler;
}

namespace object {
	class Obj;
    class ObjString;
}


//Lisp style mark compact garbage collector with additional non moving allocations
namespace memory {
	class GarbageCollector {
	public:
		void* alloc(uInt64 size);
		void collect();
		void collect(compileCore::Compiler* compiler);
		GarbageCollector();
		void markObj(object::Obj* object);
		std::atomic<bool> shouldCollect;
        std::atomic<uInt64> heapSize;
        runtime::VM* vm;
        ankerl::unordered_dense::map<string, object::ObjString*> interned;
	private:
		std::mutex allocMtx;
		uInt64 heapSizeLimit;
		//static allocations that get transfered to heap at next 'collect'
		vector<object::Obj*> objects;

		vector<object::Obj*> markStack;

		void mark();
		void markRoots(runtime::VM* vm);
		void markRoots(compileCore::Compiler* compiler);
		void sweep();
	};

	extern GarbageCollector gc;
}