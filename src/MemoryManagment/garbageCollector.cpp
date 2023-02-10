#include "garbageCollector.h"
#include "../ErrorHandling/errorHandler.h"
#include "../codegen/compiler.h"
#include "../Objects/objects.h"
#include "../Runtime/vm.h"
#include "../Includes/fmt/format.h"

//start size of heap in KB
#define HEAP_START_SIZE 1024


namespace memory {
	GarbageCollector gc = GarbageCollector();

	GarbageCollector::GarbageCollector() {
		heapSize = 0;
		heapSizeLimit = HEAP_START_SIZE*1024;

		shouldCollect.store(false);
	}

	void* GarbageCollector::alloc(uInt64 size) {
		std::scoped_lock<std::mutex> lk(allocMtx);
		heapSize += size;
		if (heapSize > heapSizeLimit) shouldCollect = true;
		byte* block = nullptr;
		try {
			block = new byte[size];
		}
		catch (const std::bad_alloc& e) {
			errorHandler::addSystemError(fmt::format("Failed allocation, tried to allocate {} bytes", size));
		}
		objects.push_back(reinterpret_cast<object::Obj*>(block));
		return block;
	}

	void GarbageCollector::collect(runtime::VM* vm) {
		markRoots(vm);
		mark();
		sweep();
		if (heapSize > heapSizeLimit) heapSizeLimit << 1;
		// After sweeping the heap all sleeping child threads are awakened
		{
			std::scoped_lock<std::mutex> lk(vm->pauseMtx);
			shouldCollect.store(false);
		}
		vm->childThreadsCv.notify_all();
	}

	void GarbageCollector::collect(compileCore::Compiler* compiler) {
		markRoots(compiler);
		mark();
		sweep();
		if (heapSize > heapSizeLimit) heapSizeLimit << 1;
		shouldCollect = false;
	}

	void GarbageCollector::mark() {
		//we use a stack to avoid going into a deep recursion(which might fail)
		while (!markStack.empty()) {
			object::Obj* ptr = markStack.back();
			markStack.pop_back();
			if (ptr->marked) continue;
			ptr->marked = true;
			ptr->trace();
		}
	}

	void GarbageCollector::markRoots(runtime::VM* vm) {
		vm->mark(this);
	}

	void GarbageCollector::markRoots(compileCore::Compiler* compiler) {
        for(Value& val : compiler->mainCodeBlock.constants) val.mark();
        for(auto func : compiler->nativeFuncs) func->marked = true;
        compiler->mainBlockFunc->marked = true;
	}

	void GarbageCollector::sweep() {
		heapSize = 0;
		for (int i = objects.size() - 1; i >= 0; i--) {
			object::Obj* obj = objects[i];
			if (!obj->marked) {
				obj->~Obj();
				objects.erase(objects.begin() + i);
				continue;
			}
			heapSize += obj->getSize();
			obj->marked = false;
		}
	}

	void GarbageCollector::markObj(object::Obj* object) {
		markStack.push_back(object);
	}
}
