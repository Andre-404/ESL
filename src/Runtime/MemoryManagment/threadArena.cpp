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

inline int szToIdx(uint64_t x){
    if(x > 256)
        return -1;
    if(x > 32 && x <= 48)
        return 0;
    return std::bit_width(x-1) - 3;
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
        try {
            block = static_cast<byte *>(rpmalloc(size));
        }
        catch (const std::bad_alloc &e) {
            errorHandler::addSystemError(fmt::format("Failed allocation, tried to allocate {} bytes", size));
        }
        // Flags that this pointer was malloc-d
        reinterpret_cast<object::Obj *>(block)->allocType = +GCAllocType::MALLOC;
        reinterpret_cast<object::Obj *>(block)->GCData = 1; // When alloc type == MALLOC GC data serves as a "marked" flag
        tempLargeObjectStorage.push_back(reinterpret_cast<object::Obj *>(block));
    } else {
        // Each fastAlloc needs to have the correct block size, using macro loop for convenience
        switch(idx){
            #define MP_SWITCH_CASE(X) case X: block = reinterpret_cast<byte*>(fastAlloc<X>()); break;
            M_LOOP(MP_CNT, MP_SWITCH_CASE, 0)
            #undef MP_SWITCH_CASE
            default: __builtin_unreachable();
        }
        object::Obj *obj = reinterpret_cast<object::Obj *>(block);
        // Lazy sweeping, some "free" blocks are dead objects that have not been destructed properly
        if (obj->GCData) runObjDestructor(obj);
        obj->allocType = idx;
        obj->GCData = 1;
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

template<size_t blockSize>
[[gnu::hot]] static void* pageTryAlloc(PageData* page){
    constexpr int16_t constNumBlocks = PAGE_SIZE / blockSize;
    int16_t* obj = reinterpret_cast<int16_t *>(page->basePtr + page->head * blockSize);
    // After a GC collection colors are reversed: free blocks are white, and occupied ones are black
    // While looking for a free slot we mark the black blocks as occupied again(white)
    // This works because page->head never moves backwards
    while(page->head < constNumBlocks && *obj == +GCBlockColor::BLACK){
        // If we find a black block reset its marked flag
        *obj = +GCBlockColor::WHITE;
        page->head++;
        obj = reinterpret_cast<int16_t *>((char *) (obj) + blockSize);
    }
    // If the loop exited because every block was taken, bail out
    if(page->head == constNumBlocks) return nullptr;
    *obj = +GCBlockColor::WHITE;
    page->head++;
    return reinterpret_cast<char *>(obj);
}

// Function is templated to (hopefully) speed up pageTryAlloc and array access
template<size_t poolIdx>
[[gnu::hot]] void* ThreadArena::fastAlloc(){
    void *ptr = nullptr;
    if(!firstFreePage[poolIdx]) allocNewPage(poolIdx);
    while (!(ptr = pageTryAlloc<mpBlockSizes[poolIdx]>(firstFreePage[poolIdx]))) {
        if (firstFreePage[poolIdx] == &pools[poolIdx].back()) {
            allocNewPage(poolIdx);
        } else
            firstFreePage[poolIdx]++;
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