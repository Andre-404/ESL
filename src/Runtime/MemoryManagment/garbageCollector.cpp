#include "garbageCollector.h"
#include "../../ErrorHandling/errorHandler.h"
#include "../../Includes/fmt/format.h"
#include "../Objects/objects.h"
#include "../Values/valueHelpersInline.cpp"
#include "../../Includes/rpmalloc/rpmalloc.h"
#include <algorithm>
#include <execution>
#include <sys/stat.h>
#include <variant>

using namespace valueHelpers;

// start size of heap in KB
#define HEAP_START_SIZE 1024


NOINLINE uintptr_t *getStackPointer() {
  return (uintptr_t *)(GET_FRAME_ADDRESS);
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
        rpmalloc_initialize();
        threadsSuspended = 0;
    }

    void GarbageCollector::checkHeapSize(const size_t size){
        uint64_t tmpHeapSize = heapSize.load(std::memory_order_relaxed);
        if (tmpHeapSize <= heapSizeLimit && (tmpHeapSize += size) >= heapSizeLimit) {
#ifdef GC_DEBUG
            std::cout<<"Tmp heap size, limit: "<<tmpHeapSize<<", "<<heapSizeLimit<<"\n";
#endif
            active = 1;
        }
        heapSize.store(tmpHeapSize, std::memory_order_relaxed);
#ifdef GC_DEBUG
        numalloc++;
#endif
    }

    // Collect can freely read and modify data because pauseMtx is under lock by lk
    void GarbageCollector::collect(std::unique_lock<std::mutex> &lk) {
        heapSize = 0;
        #ifdef GC_DEBUG
        double d = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        marked = 0;
        #endif
        // Sort pages by address to make searching for valid pointers easier
        std::sort(pages.begin(), pages.end(), [](PageData* a, PageData* b){return a->basePtr < b->basePtr;});
        #ifdef GC_DEBUG
        double d2 = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Sorting "<<pages.size()<<" pages took: "<<d2-d<<"\n";
        #endif
        markRoots();
        #ifdef GC_DEBUG
        double d3 = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Marking roots took: "<<d3-d2<<"\n";
        #endif
        mark();
        #ifdef GC_DEBUG
        double d4 = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Tracing took: "<<d4-d3<<"\n";
        int numel = largeObjects.size();
        #endif
        sweep();
        #ifdef GC_DEBUG
        double d5 = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"sweeping took: "<<d5-d4<<" objects swept: "<<numel - largeObjects.size()<<"\n";
        numalloc = 0;
        #endif
        while (heapSize > heapSizeLimit) heapSizeLimit = heapSizeLimit << 1;
        // Reset the pages vector
        pages.clear();
        // Tells all waiting threads that gc cycle is over
        active = 0;
        // This thread is no longer suspended
        threadsSuspended--;
        lk.unlock();
        STWcv.notify_all();
    }

    // Returns true if this object was already marked
    static inline bool isMarked(object::Obj *ptr) {
        if (ptr->allocType == +GCAllocType::CONSTANT) return true;
        if (ptr->allocType == +GCAllocType::MALLOC) {
            if (ptr->GCData == 1) return true;
        } else {
            // This is mempool allocated
            int16_t *info = reinterpret_cast<int16_t *>(&ptr->padding);
            if (*info == +GCBlockColor::BLACK) return true;
        }
        return false;
    }
    // Additionally marks the not already marked object
    static inline bool markIfNotMarked(object::Obj *ptr) {
        if (ptr->allocType == +GCAllocType::CONSTANT) return true;
        if (ptr->allocType == +GCAllocType::MALLOC) {
            if (ptr->GCData == 1) return true;
            ptr->GCData = true;
        } else {
            // This is mempool allocated
            int16_t *info = reinterpret_cast<int16_t *>(&ptr->padding);
            if (*info == +GCBlockColor::BLACK) return true;
            *info = +GCBlockColor::BLACK;
        }
        return false;
    }
    static inline void markVal(Value x){
        if (isObj(x)) memory::gc->markObj(decodeObj(x));
    }

    void GarbageCollector::mark() {
        // We use a stack to avoid going into a deep recursion(which might fail)
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
                    // Small optimization: if numOfHeapPtrs is 0 then we don't even scan the array for objects
                    // and if there are objects we only scan until we find all objects
                    int temp = 0;
                    int i = 0;
                    uInt64 arrSize = arr->values.size();
                    while (i < arrSize && temp < arr->numOfHeapPtr) {
                        markVal(arr->values[i]);
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
                    markVal(upval->val);
                    break;
                }
                case +ObjType::INSTANCE: {
                    ObjInstance *inst = reinterpret_cast<ObjInstance *>(ptr);
                    for (int i = 0; i < inst->fieldArrLen; i++) markVal(inst->fields[i]);
                    break;
                }
                case +ObjType::HASH_MAP: {
                    ObjHashMap *map = reinterpret_cast<ObjHashMap *>(ptr);
                    for (auto &field: map->fields) {
                        markObj(field.first);
                        markVal(field.second);
                    }
                    break;
                }
                case +ObjType::FUTURE: {
                    ObjFuture *fut = reinterpret_cast<ObjFuture *>(ptr);
                    // When tracing all threads other than the main one are suspended, so there's no way for anything to write to val
                    markVal(fut->val);
                    break;
                }
            }
        }
        #ifdef GC_DEBUG
        std::cout<<"Object allocated: "<<numalloc<<"\n";
        std::cout<<"Objects marked: "<<marked<<"\n" << "Heap size, limit: "<<heapSize<<", "<<heapSizeLimit<<"\n";
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

    // Pushing to pages and adding to largeObjects is ok because this function is only ran in suspendThread and setStackEnd
    // which both lock pauseMty before doing work
    void GarbageCollector::accumulatePages(ThreadArena& arena){
        for(auto i = 0; i < MP_CNT; i++){
            vector<PageData>& pool = arena.getMemoryPool(i);
            PageData* firstFreePage = arena.getFirstFreePage(i);
            pages.reserve(pages.size() + pool.size());
            for (PageData& page: pool){
                if(&page < firstFreePage){
                    // Pages that come before firstFreePage have had all their block colors reverted to the correct one lazily
                    page.head = 0;
                }else{
                    // Expensive reset
                    revertBlockColor(page);
                }
                pages.push_back(&page);
            }
            arena.resetFirstFreePage(i);
        }

        vector<object::Obj*>& tempLargeObjects = arena.getTempStorage();
        largeObjects.reserve(largeObjects.size() + tempLargeObjects.size());
        largeObjects.insert(tempLargeObjects.begin(), tempLargeObjects.end());
        tempLargeObjects.clear();
    }
    void GarbageCollector::revertBlockColor(PageData& page){
        int16_t* obj = reinterpret_cast<int16_t *>(page.basePtr);
        int16_t* end = reinterpret_cast<int16_t *>(page.basePtr + page.numBlocks*page.blockSize);
        while(obj != end){
            if(*obj == +GCBlockColor::BLACK) *obj = +GCBlockColor::WHITE;
            obj = reinterpret_cast<int16_t *>((char *) (obj) + page.blockSize);
        }
        page.head = 0;
    }

    // Should work for now but might be a problem when the heap gets into GB teritory
    // O(log(n)) i dont think it can get better than this
    bool GarbageCollector::isAllocedByMempools(object::Obj *ptr) {
        auto it = std::lower_bound(pages.begin(), pages.end(), ptr, [](PageData* page, object::Obj* ptr){
            return page->basePtr < (char*)ptr;
        });
        if(it == pages.end()) return false;
        if((*it)->basePtr != (char*)ptr && it != pages.begin()) it--;
        int64_t diff = (char *) ptr - ((*it)->basePtr);
        int16_t* castPtr = reinterpret_cast<int16_t *>(ptr);
        return diff >= 0 && diff < PAGE_SIZE && diff % (*it)->blockSize == 0 && *castPtr == +GCBlockColor::WHITE;
    }

    void GarbageCollector::sweep() {
        // Sweeps large objects, small objects are swept lazily
        for (auto it = largeObjects.cbegin(); it != largeObjects.cend();) {
            object::Obj *obj = *it;
            if (obj->GCData == 0) {
                runObjDestructor(obj);
                rpfree(obj);
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

    void GarbageCollector::removeStackStart(const std::thread::id thread) {
        {
            std::scoped_lock<std::mutex> lk(pauseMtx);
            // TODO: put giving pages here
            threadsStack.erase(thread);
        }
        // Only notify on deletion
        STWcv.notify_one();
    }
    void GarbageCollector::setStackEnd(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena){
        {
            std::scoped_lock<std::mutex> lk(pauseMtx);
            // Sets stack end for this thread
            threadsStack[thread].end = stackEnd;
            // Accumulates pages and large objects from this threads arena
            accumulatePages(arena);
        }
    }

    void GarbageCollector::suspendThread(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena) {
        // First lock the mutex so that we certainly enter the wait queue of the cv
        std::unique_lock<std::mutex> lk(pauseMtx);
        // Mark this thread as suspended
        threadsSuspended.fetch_add(1);
        // Same stuff as in setStackEnd but doing it like this save us 1 mutex locking
        threadsStack[thread].end = stackEnd;
        accumulatePages(arena);
        // If this is the last running thread, run the GC, if not enter the wait queue
        if (threadsStack.size() == threadsSuspended) {
            // Execute the gc cycle, we hold the lock to pauseMtx right now so no other thread can alter things
            collect(lk);
            return;
        }
        // The rhs condition is because (threadsStack - 1) threads could be suspended and the last thread is finishing execution
        // In that case the last thread will never enter suspendThread,
        // but will notify a random thread waiting on STWcv when it gets deleted via removeStackStart()
        STWcv.wait(lk, [&] { return active == 0 || threadsStack.size() == threadsSuspended; });
        // Wait is over, either the gc cycle is over(active == 0) or this thread has been selected to run the gc cycle
        if (active == 0) {
            // All threads that are waiting because of suspendThread need to decrement threadsSuspended on their own
            // Some threads might be waiting on a user defined mutex and are thus suspended,
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
