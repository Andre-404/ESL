#include "garbageCollector.h"
#include "../../ErrorHandling/errorHandler.h"
#include "../../Includes/fmt/format.h"
#include "../Objects/objects.h"
#include "../Values/valueHelpersInline.cpp"
#include <algorithm>
#include <execution>
#include <sys/stat.h>
#include <variant>

using namespace valueHelpers;

// start size of heap in KB
#define HEAP_START_SIZE 1024

static constexpr uint32_t GCDataMask = 0xfe;
static constexpr uint32_t shouldDestructFlagMask = 1;
static constexpr int16_t blackObj = -3;
constexpr std::array<size_t, 6> mpBlockSizes = {48, 16, 32, 64, 128, 256};
enum GCAllocType{ MALLOC = mpBlockSizes.size(), CONSTANT = 128 };
inline int szToIdx(uint64_t x){
    if(x > 256) 
        return -1;
    if(x > 32 && x <= 48) 
        return 0;
    return std::bit_width(x-1) - 3;
}

NOINLINE uintptr_t *getStackPointer() {
  return (uintptr_t *)(GET_FRAME_ADDRESS);
}

static inline void runObjDestructor(object::Obj* obj){
    // Have to do this because we don't have access to virtual destructors,
    // however some objects allocate STL containers that need cleaning up
    obj->GCData = 0;
    switch(obj->type){
        case +object::ObjType::ARRAY: reinterpret_cast<object::ObjArray*>(obj)->~ObjArray(); return;
        case +object::ObjType::FILE: reinterpret_cast<object::ObjFile*>(obj)->~ObjFile(); return;
        case +object::ObjType::FUTURE: reinterpret_cast<object::ObjFuture*>(obj)->~ObjFuture(); return;
        case +object::ObjType::HASH_MAP: reinterpret_cast<object::ObjHashMap*>(obj)->~ObjHashMap(); return;
        case +object::ObjType::MUTEX: reinterpret_cast<object::ObjMutex*>(obj)->~ObjMutex(); return;
        default: return;
    }
}
#ifdef GC_DEBUG
static uint64_t numalloc = 0;
static uint64_t marked = 0;
#endif
namespace memory {
    GarbageCollector *gc = nullptr;

    GarbageCollector::GarbageCollector(byte &active) : active(active) {
        heapSize = 0;
        heapSizeLimit = HEAP_START_SIZE * 1024;
        tmpAlloc.reserve(4096);
        for(int i = 0; i < mpBlockSizes.size(); i++){
            memPools[i] = MemoryPool(mpBlockSizes[i]);
        }
        threadsSuspended = 0;
    }

    [[gnu::hot]] void *GarbageCollector::alloc(const uInt64 size) {
        // No thread is marked as suspended while allocating, even though they have to lock the allocMtx
        // Every thread that enters this function is guaranteed to exit it or to crash the whole program
        uint64_t tmpHeapSize = heapSize.load(std::memory_order_relaxed);
        if (tmpHeapSize <= heapSizeLimit && (tmpHeapSize += size) >= heapSizeLimit) {
            active = 1;
        }
        heapSize.store(tmpHeapSize, std::memory_order_relaxed);
        #ifdef GC_DEBUG
        numalloc++;
        #endif
        byte *block = nullptr;
        int idx = szToIdx(size);
        if (idx == -1) {
            try {
                block = new byte[size];
            }
            catch (const std::bad_alloc &e) {
                errorHandler::addSystemError(fmt::format("Failed allocation, tried to allocate {} bytes", size));
            }
            // Flags that this pointer was malloc-d
            reinterpret_cast<Obj *>(block)->allocType = GCAllocType::MALLOC;
            reinterpret_cast<Obj *>(block)->GCData = 1; // When alloc type == MALLOC GC data serves as a "marked" flag
            tmpAlloc.push_back(reinterpret_cast<Obj *>(block));
        } else {
            switch(idx){
                case 0: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[0]>()); break;
                case 1: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[1]>()); break;
                case 2: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[2]>()); break;
                case 3: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[3]>()); break;
                case 4: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[4]>()); break;
                case 5: block = reinterpret_cast<byte *>(memPools[idx].alloc<mpBlockSizes[5]>()); break;
            }
            Obj *obj = reinterpret_cast<Obj *>(block);
            // Lazy sweeping
            if (obj->GCData) runObjDestructor(obj);
            obj->allocType = idx;
            obj->GCData = 1;
        }
        return block;
    }

    // Collect can freely read and modify data because pauseMtx is under lock by lk
    void GarbageCollector::collect(std::unique_lock<std::mutex> &lk) {
        #ifdef GC_DEBUG
        double d = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        #endif
        largeObjects.reserve(largeObjects.size() + tmpAlloc.size());
        largeObjects.insert(tmpAlloc.begin(), tmpAlloc.end());
        tmpAlloc.clear();
        heapSize = 0;
        #ifdef GC_DEBUG
        std::cout<<"alloced since last gc: "<<numalloc<<"\n";
        numalloc = 0;
        marked = 0;
        #endif
        resetMemPools();
        #ifdef GC_DEBUG
        double d2 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Reseting pools took: "<<d2-d<<"\n";
        #endif
        markRoots();
        #ifdef GC_DEBUG
        double d3 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Root scanning took: "<<d3-d2<<"\n";
        #endif
        mark();
        #ifdef GC_DEBUG
        double d4 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Tracing took: "<<d4-d3<<"\n";
        #endif
        sweep();
        #ifdef GC_DEBUG
        double d5 = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"sweeping took: "<<d5-d4<<"\n";
        #endif
        while (heapSize > heapSizeLimit) heapSizeLimit = heapSizeLimit << 1;
        // Tells all waiting threads that gc cycle is over
        active = 0;
        // This thread is no longer suspended
        threadsSuspended--;
        lk.unlock();
        STWcv.notify_all();
    }

    void GarbageCollector::resetMemPools() {
        std::for_each(std::execution::par_unseq, memPools.begin(), memPools.end(), 
                [](MemoryPool& memoryPool){
                    memoryPool.resetPages();
                }
        );
    }

    // Returns true if this object was already marked
    static inline bool markIfNotMarked(object::Obj *ptr) {
        if (ptr->allocType == GCAllocType::CONSTANT) return true;
        if (ptr->allocType == GCAllocType::MALLOC) {
            if (ptr->GCData == 1) return true;
            ptr->GCData = true;
        } else {
            // This is mempool allocated
            int16_t *info = reinterpret_cast<int16_t *>(&ptr->padding);
            if (*info == blackObj) return true;
            *info = blackObj;
        }
        return false;
    }

    static inline bool isMarked(object::Obj *ptr) {
        if (ptr->allocType == GCAllocType::CONSTANT) return true;
        if (ptr->allocType == GCAllocType::MALLOC) {
            if (ptr->GCData == 1) return true;
        } else {
            // This is mempool allocated
            int16_t *info = reinterpret_cast<int16_t *>(&ptr->padding);
            if (*info == blackObj) return true;
        }
        return false;
    }

    void GarbageCollector::mark() {
        //we use a stack to avoid going into a deep recursion(which might fail)
        while (!markStack.empty()) {
            object::Obj *ptr = markStack.back();
            markStack.pop_back();
            if (markIfNotMarked(ptr)) continue;
            #ifdef GC_DEBUG
            marked++;
            #endif
            heapSize += ptr->getSize();
            switch (ptr->type) {
                case +ObjType::ARRAY: {
                    ObjArray *arr = reinterpret_cast<ObjArray *>(ptr);
                    //small optimization: if numOfHeapPtrs is 0 then we don't even scan the array for objects
                    //and if there are objects we only scan until we find all objects
                    int temp = 0;
                    int i = 0;
                    uInt64 arrSize = arr->values.size();
                    while (i < arrSize && temp < arr->numOfHeapPtr) {
                        valueHelpers::mark(arr->values[i]);
                        if (isObj(arr->values[i])) temp++;
                    }
                    break;
                }
                case +ObjType::CLOSURE: {
                    ObjClosure *cl = reinterpret_cast<ObjClosure *>(ptr);
                    for (int i = 0; i < cl->freevarCount; i++) {
                        markObj(cl->freevars[i]);
                    }
                    break;
                }
                case +ObjType::FREEVAR: {
                    ObjFreevar *upval = reinterpret_cast<ObjFreevar *>(ptr);
                    valueHelpers::mark(upval->val);
                    break;
                }
                case +ObjType::INSTANCE: {
                    ObjInstance *inst = reinterpret_cast<ObjInstance *>(ptr);
                    for (int i = 0; i < inst->fieldArrLen; i++) valueHelpers::mark(inst->fields[i]);
                    break;
                }
                case +ObjType::HASH_MAP: {
                    ObjHashMap *map = reinterpret_cast<ObjHashMap *>(ptr);
                    for (auto &field: map->fields) {
                        markObj(field.first);
                        valueHelpers::mark(field.second);
                    }
                    break;
                }
                case +ObjType::FUTURE: {
                    ObjFuture *fut = reinterpret_cast<ObjFuture *>(ptr);
                    // When tracing all threads other than the main one are suspended, so there's no way for anything to write to val
                    valueHelpers::mark(fut->val);
                    break;
                }
            }
        }
        #ifdef GC_DEBUG
        std::cout<<"Objects marked: "<<marked<<"\n";
        #endif
    }

    void GarbageCollector::markRoots() {
        // Have to mark the stack of each thread
        for (auto it = threadsStack.begin(); it != threadsStack.end(); it++) {
            byte *start = reinterpret_cast<byte *>(it->second.start);
            byte *end = reinterpret_cast<byte *>(it->second.end);
            // The stack grows downward, so stack end is a smaller address than stack start
            while (end < start) {
                // Cast pointer to int64, check for the object flag, if it's present try to mark the object and push to mark stack
                Value address = *reinterpret_cast<Value *>(end);
                if (isObj(address)) {
                    Obj *object = decodeObj(address);
                    // There is a small chance that some random 64 bits of data on the stack appear as a NaN boxed object
                    // Because of that before accessing the object first we check if 'object' really points to an allocated object
                    if (isValidPtr(object)) {
                        markObj(object);
                    }
                } else if (isValidPtr(*reinterpret_cast<Obj **>(end))) {
                    markObj(*reinterpret_cast<Obj **>(end));
                }
                end++;
            }
        }
        // Mark all globals
        for (int i = 0; i < globalRoots.size(); i++) {
            if (isObj(*globalRoots[i])) {
                markObj(decodeObj(*globalRoots[i]));
            }
        }
    }

    // TODO: can this be optimized? Running through every page of every mempool seems expensive?
    // Should work for now but might be a problem when the heap gets into GB teritory
    bool GarbageCollector::isAllocedByMempools(object::Obj *ptr) {
        #ifdef GC_DEBUG
        for(MP& memPool : memPools){
            if(std::visit([ptr](auto &&v){ return v.allocedByThisPool(reinterpret_cast<uintptr_t>(ptr)), memPool)) return true;
        }
        return false;
        #else
        return std::any_of(std::execution::par_unseq, memPools.begin(), memPools.end(), [ptr](MemoryPool& memPool) {
            return memPool.allocedByThisPool(reinterpret_cast<uintptr_t>(ptr));
        });
        #endif
    }

    void GarbageCollector::sweep() {
        for (auto it = interned.begin(); it != interned.end();) {
            if (!isMarked(it->second)) it = interned.erase(it);
            it++;
        }
        // Sweeps large objects, small objects are swept lazily
        for (auto it = largeObjects.cbegin(); it != largeObjects.cend();) {
            object::Obj *obj = *it;
            if (obj->GCData == 0) {
                runObjDestructor(obj);
                delete reinterpret_cast<byte *>(obj);
                it = largeObjects.erase(it);
                continue;
            }
            obj->GCData = 0;
            it++;
        }
    }

    [[gnu::always_inline]] void GarbageCollector::markObj(object::Obj *const ptr) {
        markStack.push_back(ptr);
    }

    void GarbageCollector::addStackStart(const std::thread::id thread, uintptr_t *stackStart) {
        {
            std::scoped_lock<std::mutex> lk(pauseMtx);
            threadsStack.insert_or_assign(thread, StackPtrEntry(stackStart));
        }
    }

    void GarbageCollector::setStackEnd(const std::thread::id thread, uintptr_t *stackEnd) {
        {
            std::scoped_lock<std::mutex> lk(pauseMtx);
            threadsStack[thread].end = stackEnd;
        }
    }

    // Make sure to run this AFTER setting the val field in ObjFuture
    void GarbageCollector::removeStackStart(const std::thread::id thread) {
        {
            std::scoped_lock<std::mutex> lk(pauseMtx);
            threadsStack.erase(thread);
        }
        // Only notify on deletion
        STWcv.notify_one();
    }

    void GarbageCollector::suspendMe() {
        // Mark this thread as suspended
        threadsSuspended.fetch_add(1);
        // First lock the mutex so that we certainly enter the wait queue of the cv
        std::unique_lock<std::mutex> lk(pauseMtx);
        // If this is the last running thread, run the GC, if not enter the wait queue
        if (threadsStack.size() == threadsSuspended) {
            // Execute the gc cycle, we hold the lock to pauseMtx right now so no other thread can alter things
            collect(lk);
            return;
        }
        // The rhs condition is because (threadsStack - 1) threads could be suspended and the last thread is finishing execution
        // In that case the last thread will never enter suspendMe,
        // but will notify a random thread waiting on STWcv when it gets deleted via removeStackStart()
        STWcv.wait(lk, [&] { return active == 0 || threadsStack.size() == threadsSuspended; });
        // Wait is over, either the gc cycle is over(active == 0) or this thread has been selected to run the gc cycle
        if (active == 0) {
            // All waiting that are waiting because of suspendMe need to decrement threadsSuspended on their own
            // Some threads might be waiting on a use defined mutex and are thus suspended,
            // because of that simply setting threadsSuspended to 0 after a gc cycle is not possible
            threadsSuspended--;
            return;
        }
        // Execute the gc cycle
        collect(lk);
    }

    bool GarbageCollector::isValidPtr(object::Obj *const ptr) {
        return largeObjects.contains(ptr) || isAllocedByMempools(ptr);
    }

    void GarbageCollector::addGlobalRoot(Value *ptr) {
        globalRoots.push_back(ptr);
    }
}

#undef HEAP_START_SIZE
