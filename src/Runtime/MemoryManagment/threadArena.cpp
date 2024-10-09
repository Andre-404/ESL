#include "threadArena.h"
#include "../../ErrorHandling/errorHandler.h"
#include "../../Includes/fmt/format.h"
#include "../Objects/objects.h"
#include "garbageCollector.h"
#include "../../Includes/rpmalloc/rpmalloc.h"
#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

using namespace memory;

// Have to instantiate thread arena here because linker complains if you put a thread_local variable in .h file
thread_local ThreadArena threadArena;
ThreadArena& memory::getLocalArena(){
    return threadArena;
}
#define SMALL_GRANULARITY 16u
#define SMALL_SIZE_CLASSES 32u
#define MEDIUM_GRANULARITY 128u
#define MEDIUM_SIZE_CLASSES (MP_CNT - SMALL_SIZE_CLASSES)

constexpr std::array<size_t, MP_CNT> generateBlockSizes(size_t shift1, size_t n1, size_t shift2, size_t n2){
    std::array<size_t, MP_CNT> arr;
    for(int i = 0; i < n1; i++){
        arr[i] = (i+1)*shift1;
    }
    for(int i = 0; i < n2; i++){
        arr[n1+i] = (i+1)*shift2 + n1*shift1;
    }

    return arr;
}

constexpr std::array<size_t, MP_CNT> mpBlockSizes = generateBlockSizes(SMALL_GRANULARITY, SMALL_SIZE_CLASSES,
                                                                       MEDIUM_GRANULARITY, MEDIUM_SIZE_CLASSES);

inline int szToIdx(size_t x){
    // Rounds up to the nearest multiple of GRANULARITY and divides by GRANULARITY to get position in mpBlockSizes
    int smallclassIdx = (x + (SMALL_GRANULARITY - 1)) >> std::countr_zero(SMALL_GRANULARITY);
    // For medium class subtract the highest small granularity block to get relative position from start of medium granularity
    int mediumclassIdx = (x - SMALL_GRANULARITY*SMALL_SIZE_CLASSES + (MEDIUM_GRANULARITY - 1)) >> std::countr_zero(MEDIUM_GRANULARITY);
    int smallCondition = (x <= mpBlockSizes[SMALL_SIZE_CLASSES-1]); // If x is below small class size return this index
    int mediumCondition = (x <= mpBlockSizes.back() && (1 - smallCondition));
    return smallclassIdx*smallCondition  +
           (SMALL_SIZE_CLASSES + mediumclassIdx)*mediumCondition // For medium blocks since idx is relative add SMALL_SIZE_CLASSES
           - 1; // Sub 1 since array starts with size 16 so for eg. x = 15 this returns 1(but index is 0)
}

ThreadArena::ThreadArena(){
    firstFreePage.fill(nullptr);
}

ThreadArena::~ThreadArena(){
    // TODO: do this
}

[[gnu::hot]] void *ThreadArena::alloc(const size_t size) {
    // Allocating is lockless, each arena does its own thing
    // TODO: should this also check if the gc pointer isn't null? seems kinda dangerous
    gc->checkHeapSize(size);
    byte *block = nullptr;
    int idx = szToIdx(size);
    if (idx == -1) {
        block = static_cast<byte *>(rpmalloc(size));
        reinterpret_cast<object::Obj *>(block)->GCInfo[0] = +GCBlockColor::BLACK;
        tempLargeObjectStorage.push_back(reinterpret_cast<object::Obj *>(block));
    } else {
        block = reinterpret_cast<byte*>(fastAlloc(idx));
        object::Obj *obj = reinterpret_cast<object::Obj *>(block);
        // Lazy sweeping, some "free" blocks are dead objects that have not been destructed properly
        runObjDestructor(obj);
        obj->GCInfo[0] = +GCBlockColor::BLACK;
        obj->GCInfo[1] = 1;
    }
    return block;
}

vector<PageData>& ThreadArena::getMemoryPool(size_t idx){
    return pools[idx];
}

PageData* ThreadArena::getFirstFreePage(size_t idx){
    return firstFreePage[idx];
}

void ThreadArena::resetFirstFreePage(size_t idx){
    firstFreePage[idx] = &pools[idx].front();
}

vector<object::Obj*>& ThreadArena::getTempStorage(){
    return tempLargeObjectStorage;
}

//template<size_t blockSize>
[[gnu::hot]] static void* pageTryAlloc(PageData* page){
    int16_t numBlocks = PAGE_SIZE / page->blockSize;
    int8_t* obj = reinterpret_cast<int8_t *>(page->basePtr + page->head * page->blockSize);
    // After a GC collection colors are reversed: free blocks are white, and occupied ones are black
    // While looking for a free slot we mark the black blocks as occupied again(white)
    // This works because page->head never moves backwards
    while(page->head < numBlocks && *obj == +GCBlockColor::WHITE){
        // If we find a black block reset its marked flag
        __builtin_prefetch(obj + page->blockSize);
        *obj = +GCBlockColor::BLACK;
        page->head++;
        obj += page->blockSize;
    }
    // If the loop exited because every block was taken, bail out
    if(page->head == numBlocks) return nullptr;
    page->head++;
    return reinterpret_cast<char *>(obj);
}

[[gnu::hot]] void* ThreadArena::fastAlloc(size_t poolIdx){
    void *ptr = nullptr;
    if(!firstFreePage[poolIdx]) allocNewPage(poolIdx);
    while (!(ptr = pageTryAlloc(firstFreePage[poolIdx]))) {
        if (firstFreePage[poolIdx] == &pools[poolIdx].back()) {
            allocNewPage(poolIdx);
        } else firstFreePage[poolIdx]++;
    }
    return ptr;
}


void ThreadArena::allocNewPage(size_t poolIdx){
    // VirtualAlloc returns zeroed memory
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, PAGE_SIZE, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
    void *page;
    posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
    // In most cases this should be faster than memset and calloc
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3) // Have to use junk outputs to let gcc know there registers are clobbered
            : "D"(page), "c"(PAGE_SIZE/8), "a"(0)
            : "memory"// Clobbered registers
            );
#endif
    pools[poolIdx].emplace_back((char*)page, mpBlockSizes[poolIdx]);
    firstFreePage[poolIdx] = &pools[poolIdx].back();
}

void ThreadArena::freePage(uint32_t pid){
    // TODO:
}