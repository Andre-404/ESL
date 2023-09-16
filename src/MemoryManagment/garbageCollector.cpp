#include "garbageCollector.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Objects/objects.h"
#include "../Includes/fmt/format.h"
#include "../Codegen/valueHelpersInline.cpp"

using namespace valueHelpers;

//start size of heap in KB
#define HEAP_START_SIZE 1024

NOINLINE uintptr_t* getStackPointer(){
    return (uintptr_t*)(GET_FRAME_ADDRESS);
}

namespace memory {
	GarbageCollector* gc = new GarbageCollector();

	GarbageCollector::GarbageCollector() {
		heapSize = 0;
		heapSizeLimit = HEAP_START_SIZE*1024;

        threadsSuspended = 0;
	}

	void* GarbageCollector::alloc(const uInt64 size) {
        // No thread is marked as suspended while allocating, even though they have to lock the allocMtx
        // Every thread that enters this function is guaranteed to exit it or to crash the whole program
		std::scoped_lock<std::mutex> lk(allocMtx);
		heapSize += size;
		if (heapSize > heapSizeLimit) active = 1;
		byte* block = nullptr;
		try {
			block = new byte[size];
		}
		catch (const std::bad_alloc& e) {
			errorHandler::addSystemError(fmt::format("Failed allocation, tried to allocate {} bytes", size));
		}
        objects.insert(reinterpret_cast<Obj*>(block));
		return block;
	}

    // Collect can freely read and modify data because pauseMtx is under lock by lk
	void GarbageCollector::collect(std::unique_lock<std::mutex>& lk) {
		markRoots();
		mark();
		sweep();
		if (heapSize > heapSizeLimit) heapSizeLimit = heapSizeLimit << 1;
        // Tells all waiting threads that gc cycle is over
        active = 0;
        // This thread is no longer suspended
        threadsSuspended--;
        lk.unlock();
        STWcv.notify_all();
	}

	void GarbageCollector::mark() {
		//we use a stack to avoid going into a deep recursion(which might fail)
		while (!markStack.empty()) {
			object::Obj* ptr = markStack.back();
			markStack.pop_back();
			if (ptr->marked) continue;
			ptr->marked = true;
            switch(ptr->type){
                case +ObjType::ARRAY:{
                    ObjArray* arr = reinterpret_cast<ObjArray*>(ptr);
                    //small optimization: if numOfHeapPtrs is 0 then we don't even scan the array for objects
                    //and if there are objects we only scan until we find all objects
                    int temp = 0;
                    int i = 0;
                    uInt64 arrSize = arr->values.size();
                    while (i < arrSize && temp < arr->numOfHeapPtr) {
                        valueHelpers::mark(arr->values[i]);
                        if(isObj(arr->values[i])) temp++;
                    }
                    break;
                }
                case +ObjType::CLOSURE:{
                    ObjClosure* cl = reinterpret_cast<ObjClosure*>(ptr);
                    for(int i = 0; i < cl->freevarCount; i++){
                        markObj(cl->freevars[i]);
                    }
                    break;
                }
                case +ObjType::FREEVAR:{
                    ObjFreevar* upval = reinterpret_cast<ObjFreevar*>(ptr);
                    valueHelpers::mark(upval->val);
                    break;
                }
                case +ObjType::CLASS: {
                    ObjClass* klass = reinterpret_cast<ObjClass*>(ptr);
                    klass->name->marked = true;
                    if(klass->superclass) markObj(klass->superclass);
                    break;
                }
                case +ObjType::INSTANCE:{
                    ObjInstance* inst = reinterpret_cast<ObjInstance*>(ptr);

                    markObj(inst->klass);
                    break;
                }
                case +ObjType::HASH_MAP:{
                    ObjHashMap* map = reinterpret_cast<ObjHashMap*>(ptr);
                    for (auto & field : map->fields) {
                        field.first->marked = true;
                        valueHelpers::mark(field.second);
                    }
                    break;
                }
                case +ObjType::FUTURE: {
                    ObjFuture* fut = reinterpret_cast<ObjFuture*>(ptr);
                    // When tracing all threads other than the main one are suspended, so there's no way for anything to write to val
                    valueHelpers::mark(fut->val);
                    break;
                }
            }
		}
	}

	void GarbageCollector::markRoots() {
        // Have to mark the stack of each thread
        for(auto it = threadsStack.begin(); it != threadsStack.end(); it++){
            byte *start = reinterpret_cast<byte *>(it->second.start);
            byte *end = reinterpret_cast<byte *>(it->second.end);
            // The stack grows downward, so stack end is a smaller address than stack start
            while (end < start) {
                // Cast pointer to int64, check for the object flag, if it's present try to mark the object and push to mark stack
                Value address = *reinterpret_cast<Value *>(end);
                if (isObj(address)) {
                    Obj* object = decodeObj(address);
                    // There is a small chance that some random 64 bits of data on the stack appear as a NaN boxed object
                    // Because of that before accessing the object first we check if 'object' really points to an allocated object
                    if (objects.contains(object)) markObj(object);
                }else if(objects.contains(reinterpret_cast<Obj *>(end))){
                    markObj(reinterpret_cast<Obj *>(end));
                }
                end++;
            }
        }
        // Mark all globals
        for(int i = 0; i < globalRoots.size(); i++) {
            if(isObj(*globalRoots[i])) {
                markObj(decodeObj(*globalRoots[i]));
            }
        }
	}

	void GarbageCollector::sweep() {
		heapSize = 0;
        for(auto it = interned.cbegin(); it != interned.cend(); ){
            if(!it->second->marked) it = interned.erase(it);
            else {
                heapSize += it->second->getSize();
                it++;
            }
        }
        for(auto it = objects.cbegin(); it !=  objects.cend();){
            object::Obj* obj = *it;
            if (!obj->marked) {
                // Have to do this because we don't have access to virtual destructors,
                // however some objects allocate STL containers that need cleaning up
                switch(obj->type){
                    case +object::ObjType::STRING: delete reinterpret_cast<object::ObjString*>(obj); break;
                    case +object::ObjType::ARRAY: delete reinterpret_cast<object::ObjArray*>(obj); break;
                    case +object::ObjType::CLASS: delete reinterpret_cast<object::ObjClass*>(obj); break;
                    case +object::ObjType::FILE: delete reinterpret_cast<object::ObjFile*>(obj); break;
                    case +object::ObjType::FUTURE: delete reinterpret_cast<object::ObjFuture*>(obj); break;
                    case +object::ObjType::HASH_MAP: delete reinterpret_cast<object::ObjHashMap*>(obj); break;
                    case +object::ObjType::INSTANCE: delete reinterpret_cast<object::ObjInstance*>(obj); break;
                    case +object::ObjType::MUTEX: delete reinterpret_cast<object::ObjMutex*>(obj); break;
                    case +object::ObjType::CLOSURE: delete reinterpret_cast<object::ObjClosure*>(obj); break;
                    default: delete obj; break;
                }
                it = objects.erase(it);
                continue;
            }
            heapSize += obj->getSize();
            obj->marked = false;
            it++;
        }
	}

	void GarbageCollector::markObj(object::Obj* const object) {
        if(object->marked) return;
        byte ty = object->type;
        using objTy = object::ObjType;
        // No need to put untraceable objects on the trace stack at all
        if(ty == +objTy::STRING || ty == +objTy::MUTEX || ty == +objTy::FILE || ty == +objTy::RANGE) {
            object->marked = true;
            return;
        }
		markStack.push_back(object);
	}

    void GarbageCollector::addStackStart(const std::thread::id thread, uintptr_t* stackStart){
        {
            std::scoped_lock<std::mutex>lk(pauseMtx);
            threadsStack.insert_or_assign(thread, StackPtrEntry(stackStart));
        }
    }
    void GarbageCollector::setStackEnd(const std::thread::id thread, uintptr_t* stackEnd){
        {
            std::scoped_lock<std::mutex>lk(pauseMtx);
            threadsStack[thread].end = stackEnd;
        }
    }
    // Make sure to run this AFTER setting the val field in ObjFuture
    void GarbageCollector::removeStackStart(const std::thread::id thread){
        {
            std::scoped_lock<std::mutex>lk(pauseMtx);
            threadsStack.erase(thread);
        }
        // Only notify on deletion
        STWcv.notify_one();
    }

    void GarbageCollector::suspendMe(){
        // Mark this thread as suspended
        threadsSuspended.fetch_add(1);
        // First lock the mutex so that we certainly enter the wait queue of the cv
        std::unique_lock<std::mutex> lk(pauseMtx);
        // If this is the last running thread, run the GC, if not enter the wait queue
        if(threadsStack.size() == threadsSuspended){
            // Execute the gc cycle, we hold the lock to pauseMtx right now so no other thread can alter things
            collect(lk);
            return;
        }
        // The rhs condition is because (threadsStack - 1) threads could be suspended and the last thread is finishing execution
        // In that case the last thread will never enter suspendMe,
        // but will notify a random thread waiting on STWcv when it gets deleted via removeStackStart()
        STWcv.wait(lk, [&]{ return active == 0 || threadsStack.size() == threadsSuspended;});
        // Wait is over, either the gc cycle is over(active == 0) or this thread has been selected to run the gc cycle
        if(active == 0){
            // All waiting that are waiting because of suspendMe need to decrement threadsSuspended on their own
            // Some threads might be waiting on a use defined mutex and are thus suspended,
            // because of that simply setting threadsSuspended to 0 after a gc cycle is not possible
            threadsSuspended--;
            return;
        }
        // Execute the gc cycle
        collect(lk);
    }

    bool GarbageCollector::isValidPtr(object::Obj* const ptr){
        return objects.contains(ptr);
    }
    void GarbageCollector::addGlobalRoot(Value* ptr){
        globalRoots.push_back(ptr);
    }
}


#undef HEAP_START_SIZE