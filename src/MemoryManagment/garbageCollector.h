#pragma once
#include "../common.h"
#include <mutex>
#include <atomic>

namespace runtime {
	class VM;
}

namespace compileCore {
	class Compiler;
}

namespace object {
	class Obj;
}


//Lisp style mark compact garbage collector with additional non moving allocations
namespace memory {
	class GarbageCollector {
	public:
		void* alloc(uInt64 size);
		void collect(runtime::VM* vm);
		void collect(compileCore::Compiler* compiler);
		GarbageCollector();
		void markObj(object::Obj* object);
		std::atomic<bool> shouldCollect;
	private:
		std::mutex allocMtx;
		uInt64 heapSize;
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