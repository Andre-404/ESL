#pragma once
#include "../../common.h"
#include "../../Includes/unorderedDense.h"
#include "threadArena.h"
#include "heapPageManager.h"
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
    enum class GCAllocRate{
        LOW,
        LOW_LONG,
        MEDIUM,
        HIGH
    };
    class HeapStatistics{
    public:
        uint32_t heapVer;
        std::atomic<uint64_t> collectionThreshold;
        std::atomic<uint64_t> currentHeapSize;
        GCAllocRate allocRate;
        static constexpr uint32_t allocRateThreshold = 16 * 1024; // 16Kb
        static constexpr uint32_t interval = 200; // In ms
        static constexpr uint64_t heapMaxSize = 1024ull * 1024ull * 1024ull * 8ull; // max size of heap is 8GB

        HeapStatistics();
        void adjustGCParams();
    private:
        uint64_t prevHeapSize;
    };



	class GarbageCollector {
	public:
        GarbageCollector(uint64_t& active);
        void checkHeapSize(const size_t size);

        void addStackStart(const std::thread::id thread, uintptr_t* stackStart);
        void removeStackStart(const std::thread::id thread);
        void setStackEnd(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena);
        void tryLockUserMutex(std::mutex& mtx);
        // Called by each thread, last thread that suspends executes the collecting
        // Note: any place where threadsSuspended is incremented should have a defined stack end before it
        // If a thread is considered suspended its stack start and end must be valid pointers
        void suspendThread(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena);
        void addGlobalRoot(Value* ptr);
        void markObj(object::Obj* const object);

        ankerl::unordered_dense::map<std::string_view, object::ObjString*> interned;
        HeapPageManager pageManager;
	private:
        // Notify threads to wake up, or notify a single random thread to run the gc cycle
        std::condition_variable STWcv;
        std::mutex pauseMtx;
        // threadsSuspended == threadStackStart.size() means all threads have stopped and the GC can run
        std::atomic<uint64_t> threadsSuspended;
        // 0 means gc is off, 1 means it's waiting to collect, and all threads should pause
        std::atomic_ref<uint64_t> active;

        vector<Value*> globalRoots;
        vector<object::Obj*> largeObjects;
		vector<object::Obj*> markStack;
        // Start and end of stack pointer for every thread that's currently running
        ankerl::unordered_dense::map<std::thread::id, StackPtrEntry> threadsStack;

        HeapStatistics statistics;

        // Binary search through pages
        object::Obj* isAllocedByMempools(object::Obj* ptr);
        // Also gets large objects from arena to gc
        void finishSweep(ThreadArena& arena);

		void mark();
        // Checks if the provided ptr is a live object allocated by the gc
        object::Obj* isValidPtr(object::Obj* const ptr);
		void markRoots();
		void sweep();
        // Pass in the lock that holds the pauseMtx
        void collect(std::unique_lock<std::mutex>& lk);
	};
    extern GarbageCollector* gc;
}

