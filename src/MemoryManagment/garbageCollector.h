#pragma once
#include "../common.h"
#include "../Includes/unorderedDense.h"
#include <mutex>
#include <atomic>
#include <thread>
#include <stack>
#include <condition_variable>

namespace object {
	class Obj;
    class ObjString;
    class ObjFuture;
}

#ifdef _MSC_VER
#define ADDRESS_OF_RETURN_ADDRESS (_AddressOfReturnAddress())
#else
#define ADDRESS_OF_RETURN_ADDRESS (__builtin_frame_address(0))
#endif
// NOINLINE is just in case
extern "C" NOINLINE uintptr_t* getStackPointer(){
    return (uintptr_t*)(ADDRESS_OF_RETURN_ADDRESS);
}



// Non-moving, non-generational mark-sweep GC with support for multithreading
namespace memory {
    struct StackPtrEntry{
        uintptr_t* start;
        uintptr_t* end;

        StackPtrEntry(uintptr_t* _start){
            start = _start;
            end = nullptr;
        }
        StackPtrEntry(){
            start = nullptr;
            end = nullptr;
        }
    };
	class GarbageCollector {
	public:
		void* alloc(uInt64 size);
		GarbageCollector();
		void markObj(object::Obj* object);

        void addStackStart(std::thread::id thread, uintptr_t* stackStart);
        // Note: any place where threadsSuspended is incremented should have addStackEnd before it
        // If a thread is considered suspended its stack start and end must be valid pointers
        void setStackEnd(std::thread::id thread, uintptr_t* stackEnd);
        void removeStackStart(std::thread::id thread);
        // Called by each thread, last thread that suspends executes the collecting
        void suspendMe();
        // Checks if the provided ptr is a live object allocated by the gc
        bool isValidPtr(object::Obj* ptr);

        // threadsSuspended == threadStackStart.size() means all threads have stopped and the GC can run
        std::atomic<int64_t> threadsSuspended;
        // 0 means gc is off, 1 means it's waiting to collect, and all threads should pause
        std::atomic<byte> active;

        // Notify threads to wake up, or notify a single random thread to run the gc cycle
        std::condition_variable STWcv;
        std::mutex pauseMtx;

        std::atomic<uInt64> heapSize;
        ankerl::unordered_dense::map<string, object::ObjString*> interned;
	private:
		std::mutex allocMtx;
        std::mutex threadStackMtx;
		uInt64 heapSizeLimit;
		// List of all allocated objects
        ankerl::unordered_dense::set<object::Obj*> objects;

		vector<object::Obj*> markStack;
        // Start and end of stack pointer for every thread that's currently running
        ankerl::unordered_dense::map<std::thread::id, StackPtrEntry> threadsStack;

		void mark();
		void markRoots();
		void sweep();
        // Pass in the lock that holds the pauseMtx
        void collect(std::unique_lock<std::mutex>& lk);
	};
    extern GarbageCollector* gc;
}

