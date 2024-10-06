#pragma once
#include "../../common.h"
#include "../../Includes/unorderedDense.h"
#include "threadArena.h"
#include <mutex>
#include <atomic>
#include <thread>
#include <stack>
#include <condition_variable>
#include <array>
#include <variant>

namespace object {
	class Obj;
    class ObjString;
    class ObjFuture;
}

#ifdef _MSC_VER
// When getStackPointer is called the return address is rsp
#define GET_FRAME_ADDRESS ((uintptr_t*)_AddressOfReturnAddress())
#else
#define GET_FRAME_ADDRESS ((uintptr_t*)__builtin_frame_address(0))
#endif
// NOINLINE is just in case
NOINLINE uintptr_t* getStackPointer();


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
        GarbageCollector(byte& active);
        void checkHeapSize(const size_t size);
        void markObj(object::Obj* const object);

        void addStackStart(const std::thread::id thread, uintptr_t* stackStart);
        void removeStackStart(const std::thread::id thread);
        void setStackEnd(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena);
        // Called by each thread, last thread that suspends executes the collecting
        // Note: any place where threadsSuspended is incremented should have a defined stack end before it
        // If a thread is considered suspended its stack start and end must be valid pointers
        void suspendThread(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena);
        // Checks if the provided ptr is a live object allocated by the gc
        bool isValidPtr(object::Obj* const ptr);
        void addGlobalRoot(Value* ptr);
        // threadsSuspended == threadStackStart.size() means all threads have stopped and the GC can run
        std::atomic<int64_t> threadsSuspended;
        // 0 means gc is off, 1 means it's waiting to collect, and all threads should pause
        std::atomic_ref<byte> active;

        // Notify threads to wake up, or notify a single random thread to run the gc cycle
        std::condition_variable STWcv;
        std::mutex pauseMtx;

        ankerl::unordered_dense::map<std::string_view, object::ObjString*> interned;
	private:
		std::mutex allocMtx;
		uInt64 heapSizeLimit;
        std::atomic<uint64_t> heapSize;
        ankerl::unordered_dense::set<object::Obj*> largeObjects;
        vector<Value*> globalRoots;

		vector<object::Obj*> markStack;
        // Start and end of stack pointer for every thread that's currently running
        ankerl::unordered_dense::map<std::thread::id, StackPtrEntry> threadsStack;

        vector<PageData*> pages;
        // Gets all pages from an arena and puts them into vector
        void accumulatePages(ThreadArena& arena);
        // Reverts black objects that are alive back to white objects
        void revertBlockColor(PageData& page);
        // Binary search through pages
        bool isAllocedByMempools(object::Obj* ptr);

		void mark();
		void markRoots();
		void sweep();
        // Pass in the lock that holds the pauseMtx
        void collect(std::unique_lock<std::mutex>& lk);
	};
    extern GarbageCollector* gc;
}

