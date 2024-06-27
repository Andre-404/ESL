#include "memoryPool.h"
#include "../Objects/objects.h"
#include "garbageCollector.h"
#include <algorithm>
#include <atomic>
#include <bit>
#include <cassert>
#include <execution>
#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

using namespace memory;

PageData::PageData(char *basePtr, int blockSize): basePtr(basePtr), head(0), blockSize(blockSize) {
    numBlocks = PAGE_SIZE / blockSize;
}

PageData::PageData(): basePtr(nullptr), head(0) {
    numBlocks = 0;
    blockSize = 0;
}

// Has to be run BEFORE main GC loop to finish the page resetting from previous GC invocation
[[gnu::always_inline, gnu::hot]] void PageData::resetPage(){
    int16_t* obj = reinterpret_cast<int16_t *>(basePtr);
    int16_t* end = reinterpret_cast<int16_t *>(basePtr + numBlocks*blockSize);
    while(obj != end){
        if(*obj == blackBlock) *obj = whiteAndAllocatedBlock;
        obj = reinterpret_cast<int16_t *>((char *) (obj) + blockSize);
    }
    head = 0;
}

MemoryPool::MemoryPool(uint32_t blockSize) : blockSize(blockSize){
    firstNonFullPage = nullptr;
    allocNewPage();
}

MemoryPool::MemoryPool() {
    firstNonFullPage = nullptr;
    blockSize = 0;
}

bool MemoryPool::allocedByThisPool(uintptr_t ptr){
#ifdef GC_DEBUG
    for(PageData& page : pages){
        int64_t diff = (char*)ptr - (page.basePtr);
        int16_t* castPtr = reinterpret_cast<int16_t *>(ptr);
        if(diff >= 0 && diff < PAGE_SIZE && diff%blockSize == 0 && *castPtr == whiteAndAllocatedBlock) {
            return true;
        }
    }
    return false;
#else
    return std::any_of(std::execution::par_unseq, pages.begin(), pages.end(), [this, ptr](PageData& page){
            int64_t diff = (char *) ptr - (page.basePtr);
            int16_t* castPtr = reinterpret_cast<int16_t *>(ptr);
            return diff >= 0 && diff < PAGE_SIZE && diff % blockSize == 0 && *castPtr == whiteAndAllocatedBlock;
            });
#endif
}

void MemoryPool::resetPages(){
#ifdef GC_DEBUG
    if(pages.size() > 1) std::cout<<"N of cleared pages: "<<pages.size()<<"\n";
    PageData* pg = &pages.front();
    // Cheap reset
    while(pg != firstNonFullPage){
        pg->head = 0;
        pg++;
    }
    // Expensive reset
    while(pg <= &pages.back()){
        pg->resetPage();
        pg++;
    }
#else
    PageData* pg = &pages.front();
    // Cheap reset
    while(pg != firstNonFullPage){
        pg->head = 0;
        pg++;
    }
    // Expensive resets are done using multithreading
    std::for_each(std::execution::par_unseq, pg, &pages.back()+1, [](PageData& page){
            page.resetPage();
            });
#endif
    firstNonFullPage = &pages.front();
}

void MemoryPool::allocNewPage(){
    // VirtualAlloc returns zeroed memory
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, PAGE_SIZE, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
    void *page;
    posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3) // Have to use junk outputs to let gcc know there registers are clobbered
            : "D"(page), "c"(PAGE_SIZE/8), "a"(0)
            : "memory"// Clobbered registers
            );
#endif
    pages.emplace_back((char*)page, blockSize);
    firstNonFullPage = &pages.back();
}

// TODO: Make this actually work?
void MemoryPool::freePage(uint32_t pid) {
    PageData& data = pages[pid];
#ifdef _WIN32
    VirtualFree((void *)data.basePtr, 0, MEM_RELEASE);
#else
    free((void *)data.basePtr);
#endif
    pages.erase(pages.begin() + pid);
}
