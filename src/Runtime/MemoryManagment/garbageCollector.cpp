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
#include <csetjmp>

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

    HeapStatistics::HeapStatistics(){
        currentHeapSize = 0;
        collectionThreshold = HEAP_START_SIZE * 1024;
        heapVer = 0;
        prevHeapSize = 0;
    }

    void HeapStatistics::adjustGCParams(){
        // TODO: right now these are just some magic numbers, work on that
        if(currentHeapSize > collectionThreshold) collectionThreshold = currentHeapSize * 1.75;
        else if(currentHeapSize * 0.5 < collectionThreshold) {
            //collectionThreshold = std::min(collectionThreshold / 2, HEAP_START_SIZE * 1024ull);
        }
    }

    void StringInterning::internString(object::ObjString* str){
        interned.insert(str);
        if(str->size > largestStrSize) largestStrSize = str->size;
    }
    object::ObjString* StringInterning::checkInterned(object::ObjString* str){
        if(str->size > largestStrSize) return str;
        auto it = interned.find(str);
        if(it != interned.end()) return *it;
        return str;
    }

    GarbageCollector::GarbageCollector(uint64_t &active) : active(active) {
        rpmalloc_initialize();
        threadsSuspended = 0;
    }

    void GarbageCollector::checkHeapSize(const size_t size){
        // Eases up on atomicity
        uint64_t tmpHeapSize = statistics.currentHeapSize.fetch_add(size, std::memory_order_relaxed);
        uint64_t tmpHeapSizeLimit = statistics.collectionThreshold.load(std::memory_order_relaxed);
        if(tmpHeapSize-size <= tmpHeapSizeLimit && tmpHeapSize >= tmpHeapSizeLimit) [[unlikely]]{
            active = 1;
        }
#ifdef GC_DEBUG
        numalloc++;
#endif
    }


    // Collect can freely read and modify data because pauseMtx is under lock by lk
    void GarbageCollector::collect(std::unique_lock<std::mutex> &lk) {
        statistics.currentHeapSize = 0;
        #ifdef GC_DEBUG
        double d = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        marked = 0;
        #endif
        pageManager.prepForCollection();
        std::sort(largeObjects.begin(), largeObjects.end());
        #ifdef GC_DEBUG
        double d2 = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        std::cout<<"Sorting pages took: "<<d2-d<<"\n";
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
        statistics.adjustGCParams();
        // A change in empty buffer means some pages in arenas are no longer available, so update heap version to let arenas know
        if(pageManager.updateEmptyBuffer() > 0) statistics.heapVer++;
        // Tells all waiting threads that gc cycle is over
        active = 0;
        // This thread is no longer suspended
        threadsSuspended--;
        lk.unlock();
        STWcv.notify_all();
    }

    // Returns true if this object was already marked
    static inline bool isMarked(object::Obj *ptr) {
        int8_t *info = &ptr->GCInfo[0];
        return *info == +GCBlockColor::CONSTANT || *info == +GCBlockColor::WHITE;
    }
    // Additionally marks the not already marked object
    static inline bool markIfNotMarked(object::Obj *ptr) {
        int8_t *info = &ptr->GCInfo[0];
        if (*info == +GCBlockColor::CONSTANT || *info == +GCBlockColor::WHITE) return true;
        *info = +GCBlockColor::WHITE;
        // If info[1] is == 2 then this is a big object
        if(info[1] != 2){
            // Get the header of page, pages are 64kb aligned
            PageData* page = reinterpret_cast<PageData *>((uint64_t) ptr & 0xffffffffffff0000);
            page->numAllocBlocks++;
        }
        return false;
    }
    static inline void markVal(Value x){
        if (isObj(x)) memory::gc->markObj(decodeObj(x));
    }

    void GarbageCollector::mark() {
        // We use a stack to avoid going into a deep recursion(which might fail)
        // Use a local non atomic instead of currentHeapSize to speed things up
        uint64_t heapSize = 0;
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
                    Value* fields = inst->getFields();
                    for (int i = 0; i < inst->fieldArrLen; i++) markVal(fields[i]);
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
        statistics.currentHeapSize = heapSize;
        #ifdef GC_DEBUG
        std::cout<<"Object allocated: "<<numalloc<<"\n";
        std::cout<<"Objects marked: "<<marked<<"\n" << "Heap size, limit: "<<statistics.currentHeapSize<<", "<<statistics.collectionThreshold<<"\n";
        #endif
    }
    // There is a small chance that some random 64 bits of data on the stack appear as a NaN boxed object or a direct pointer
    // Because of that before accessing the object first we check if 'object' really points to an allocated object
    // This function also recognizes interior pointers that point to valid objects
    void GarbageCollector::markRoots() {
        // Have to mark the stack of each thread
        for (auto it = threadsStack.begin(); it != threadsStack.end(); it++) {
            byte *start = reinterpret_cast<byte *>(it->second.start);
            byte *end = reinterpret_cast<byte *>(it->second.end);
            // The stack grows downward, so stack end is a smaller address than stack start
            while (end < start) {
                // Cast pointer to int64, check for the object flag, if it's present try to mark the object and push to mark stack
                Value address = *reinterpret_cast<Value *>(end);
                // isValidPtr returns the pointer to the base address of the object that we then put in the mark stack
                // (needed in case of interior pointers)
                if (isObj(address)) {
                    Obj *object = decodeObj(address);
                    if ((object = isValidPtr(object))) {
                        markObj(object);
                    }
                } else if (Obj *basePtr = isValidPtr(*reinterpret_cast<Obj **>(end))) {
                    markObj(basePtr);
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
    // which both lock pauseMtx before doing work
    void GarbageCollector::finishSweep(ThreadArena& arena){
        for(auto i = 0; i < MP_CNT; i++){
            PageData* pool = arena.getMemoryPool(i);
            PageData* firstFreePage = arena.getFirstFreePage(i);
            // Pages that come before firstFreePage have had all their block colors reverted to the correct one lazily
            while(pool != nullptr && pool != firstFreePage){
                pool->head = 0;
                pool = pool->next;
            }
            while(pool != nullptr){
                // Finish sweep phase for pages which haven't been swept lazily
                sweepPage(*pool);
                pool = pool->next;
            }
        }
        // Arenas use tempLargeObjects to avoid locking when allocating large objects, but GC should manage these objects
        vector<object::Obj*>& tempLargeObjects = arena.getTempStorage();
        largeObjects.reserve(largeObjects.size() + tempLargeObjects.size());
        largeObjects.insert(largeObjects.end(), tempLargeObjects.begin(), tempLargeObjects.end());
        tempLargeObjects.clear();
    }

    // Large objects are eagerly swept to reclaim memory
    void GarbageCollector::sweep() {
        int j = 0;
        for(int i = 0; i < largeObjects.size(); i++){
            object::Obj *obj = largeObjects[i];
            if (obj->GCInfo[0] == +GCBlockColor::BLACK) {
                runObjDestructor(obj);
                rpfree(obj);
                continue;
            }
            obj->GCInfo[0] = +GCBlockColor::BLACK;
            largeObjects[j] = largeObjects[i];
            j++;
        }
        largeObjects.resize(j);
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
            finishSweep(arena);
        }
    }

    void GarbageCollector::tryLockUserMutex(std::mutex& mtx){
        // Get stack end(lowest address) and then spill the registers to the stack
        jmp_buf jb;
        setjmp(jb);
        uintptr_t* stackEnd;
        __asm__ volatile("movq %%rsp, %0" : "=r"(stackEnd));
        setStackEnd(std::this_thread::get_id(), stackEnd, getLocalArena());
        // Let GC know this thread will now try to lock mtx and could block
        threadsSuspended++;
        mtx.lock();
        // Makes sure that this thread doesn't start executing code while GC is running after it locks mtx
        // Even though threadSuspended is atomic pauseMtx is needed because it's taken while GC is running
        // (so locking it here will block until GC finishes)
        std::scoped_lock<std::mutex> lk(pauseMtx);
        threadsSuspended--;
        getLocalArena().updateMemoryPools(statistics.heapVer);
    }

    void GarbageCollector::suspendThread(const std::thread::id thread, uintptr_t *stackEnd, ThreadArena& arena) {
        // First lock the mutex so that we certainly enter the wait queue of the cv
        std::unique_lock<std::mutex> lk(pauseMtx);
        // Mark this thread as suspended
        threadsSuspended++;
        // Same stuff as in setStackEnd but doing it like this save us 1 mutex locking
        threadsStack[thread].end = stackEnd;
        finishSweep(arena);
        // If this is the last running thread, run the GC, if not enter the wait queue
        if (threadsStack.size() == threadsSuspended) {
            // Execute the gc cycle, we hold the lock to pauseMtx right now so no other thread can alter things
            collect(lk);
            arena.updateMemoryPools(statistics.heapVer);
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
            arena.updateMemoryPools(statistics.heapVer);
            return;
        }
        // Execute the gc cycle
        collect(lk);
        arena.updateMemoryPools(statistics.heapVer);
    }
    // O(log n)
    // Credit for faster binary search: https://en.algorithmica.org/hpc/data-structures/binary-search/
    static object::Obj* isLargeObject(vector<object::Obj*>& objects, object::Obj *const ptr){
        if(objects.empty()) return nullptr;
        object::Obj** base = objects.data();
        size_t len = objects.size();
        while(len > 1){
            size_t half = len / 2;
            len -= half;
            __builtin_prefetch(&base[len / 2 - 1]);
            __builtin_prefetch(&base[half + len / 2 - 1]);
            base += (base[half - 1] < ptr) * half;
        }
        object::Obj* potentialObj = *base;
        // This check handles interior pointers(if difference is less than the object size than this is surely an interior pointer)
        return reinterpret_cast<Obj *>((size_t)potentialObj * (ptr - potentialObj < potentialObj->getSize()));
    }
    // Should work for now but might be a problem when the heap gets into GB teritory
    // getPageFromPtr is O(log(n)), i dont think it can get better than this
    object::Obj* GarbageCollector::isAllocedByMempools(object::Obj *ptr) {
        PageData* page = pageManager.getPageFromPtr((char*)ptr);
        if(page == nullptr) return nullptr;
        int64_t diff = (char *) ptr - (page->basePtr);
        // If ptr is an interior pointer this gets the base address of the object it points to
        object::Obj* baseObjPtr = reinterpret_cast<Obj *>((char *) ptr - diff % page->blockSize);
        // Objects is accessed only after we've confirmed that its within page bounds
        if((diff >= 0 && diff < (PAGE_SIZE - sizeof(PageData)) && baseObjPtr->GCInfo[1] == 1)){
            page->numAllocBlocks++; // Lets the page manager know this page shouldn't be put into the free list
            return baseObjPtr;
        }
        return nullptr;
    }
    // Returns null if ptr is not a valid pointer, if it is then return the base address of the objects ptr points to
    object::Obj* GarbageCollector::isValidPtr(object::Obj *const ptr) {
        return reinterpret_cast<Obj *>((size_t) isLargeObject(largeObjects, ptr) + (size_t) isAllocedByMempools(ptr));
    }

    [[gnu::always_inline]] void GarbageCollector::addGlobalRoot(Value *ptr) {
        globalRoots.push_back(ptr);
    }
    [[gnu::always_inline]] void GarbageCollector::markObj(object::Obj *const ptr) {
        markStack.push_back(ptr);
    }
}

#undef HEAP_START_SIZE
