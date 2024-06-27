#include "memoryPool.h"
#include "../Objects/objects.h"
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

PageData::PageData(char *basePtr, uint64_t blockSize)
    : basePtr(basePtr), blockSize(blockSize){
    numBlocks = (PAGE_SIZE/blockSize);
    head = 0;
}
PageData::PageData() {
  basePtr = nullptr;
  blockSize = 0;
  numBlocks = 0;
  head = 0;
}

static constexpr uint64_t u64Mask = 0xffffffffffffffff;
static constexpr uint16_t u16Mask = 0xffff;
static constexpr uint8_t u8Mask = 0xff;
static constexpr int16_t whiteAndAllocatedBlock = -2;
static constexpr int16_t blackBlock = -3;
/*
[[gnu::always_inline, gnu::hot]] inline char* PageData::alloc(){
    if(head == numBlocks) return nullptr;
    // Go until you find and non-black block, only black blocks are not free after a gc
    int16_t* obj = reinterpret_cast<int16_t *>(basePtr + head * blockSize);
    while(head < numBlocks && *obj == blackBlock){
        // If we find a black block reset its marked flag
        *obj = whiteAndAllocatedBlock;
        head++;
        obj = reinterpret_cast<int16_t *>((char *) (obj) + blockSize);
    }
    // If the loop exited because every block was taken, bail out
    if(head == numBlocks) return nullptr;
    *obj = whiteAndAllocatedBlock;
    head++;
    return reinterpret_cast<char *>(obj);
}*/
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
MemoryPool::MemoryPool(uint64_t blockSize) : blockSize(blockSize) {
  // Calculates number of objects that can be allocated in a single page such
  // that the page can still fit the bitmap info
  firstNonFullPage = nullptr;
  allocNewPage();
}

MemoryPool::MemoryPool() {
  blockSize = 0;
  firstNonFullPage = nullptr;
}
/*
[[gnu::hot]] void *MemoryPool::alloc() {
  void *ptr = nullptr;
  while (!(ptr = firstNonFullPage->alloc())) {
    if (firstNonFullPage == &pages.back()) {
      allocNewPage();
    } else
      firstNonFullPage++;
  }
  return ptr;
}*/

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
    // TODO: right now bitmapSize is not equal to the amount of blocks that can be placed but rather blocksPerPage - blocksPerPage mod 8, fix this
    // There might be some unused bytes before block start, we do this to have 8 byte alignment for blocks
    pages.emplace_back((char*)page, blockSize);
    firstNonFullPage = &pages.back();
}

// TODO: Make this actually work?
void MemoryPool::freePage(uint32_t pid) {
  PageData &data = pages[pid];
#ifdef _WIN32
  VirtualFree((void *)data.basePtr, 0, MEM_RELEASE);
#else
  free((void *)data.basePtr);
#endif
  pages.erase(pages.begin() + pid);
}
