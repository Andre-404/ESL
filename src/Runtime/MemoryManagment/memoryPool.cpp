#include "memoryPool.h"
#include "../Objects/objects.h"
#include <cassert>
#include <bit>
#include <atomic>
#include <algorithm>
#include <execution>
#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

using namespace memory;

PageData::PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize)
: basePtr(basePtr), blockStart(blockStart),blockSize(blockSize), bitmapSize(bitmapSize) {
    end64 = reinterpret_cast<uint64_t *>(basePtr) + bitmapSize / 8;
    end256 = reinterpret_cast<__m256i *>(basePtr) + bitmapSize / 32;
    lastBitmapPos = reinterpret_cast<uint8_t *>(basePtr);
}
PageData::PageData(){
    basePtr = nullptr;
    blockStart = nullptr;
    end64 = nullptr;
    blockSize = 0;
    bitmapSize = 0;
    lastBitmapPos = nullptr;
}

static constexpr uint64_t u64Mask = 0xffffffffffffffff;
static constexpr uint16_t u16Mask = 0xffff;
static constexpr uint8_t u8Mask = 0xff;
/*
// Finds first free block or returns nullptr
[[gnu::always_inline]] char* PageData::firstFreeBlock(){
    // These 2 loops follow the same principle as the first one but use smaller granularity
    // Scan 256 bits at once, assumes little endian
    __m256i* start256 = reinterpret_cast<__m256i*>(lastBitmapPos);
    while(start256 != end256){
        __m256i vec = _mm256_loadu_si256(start256);
        // Compare each byte in parallel
        __m256i vnonzero = _mm256_cmpeq_epi8(vec, _mm256_set1_epi64x(0xFFFFFFFFFFFFFFFF));
        // One bit for each byte in ymm register that represents if that byte is equal to 0xFF(no free blocks)
        uint32_t nzmask = ~_mm256_movemask_epi8(vnonzero);
        // Nzmask is negated to allow for ctzl
        if(nzmask != 0){
            // Can safely call the builtin since we know that nazmask isn't 0
            unsigned tzbytes = __builtin_ctzl(nzmask);
            uint8_t *nz_elem = (uint8_t*)start256 + tzbytes;
            uint8_t before = *nz_elem;

            // Count trailing ones is first non 0xFF byte
            uint8_t trailingOnes = std::countr_one(before);
            *nz_elem |= ( 1 << trailingOnes);
            // Store the last bitmap pos
            lastBitmapPos = reinterpret_cast<uint8_t *>(start256);
            // Calculate total number of trailing ones in the original register
            uint32_t trones = 8 * tzbytes + trailingOnes;
            return blockStart + (((uint8_t*) start256 - basePtr) * 8 + trones)*blockSize;
        }
        start256++;
    }

    // These 2 loops follow the same principle as the first one but use smaller granularity
    uint64_t* start64 = reinterpret_cast<uint64_t*>(end256);
    while(start64 != end64){
        if (*start64 != u64Mask) {
            uint64_t before = *start64;
            uint8_t trones = std::countr_one(before);
            // Update the bitmap
            *start64 |= 1 << trones;
            return blockStart + (((uint8_t*) start64 - basePtr) * 8 + trones)*blockSize;
        }
        start64++;
    }
    uint8_t* start8 = reinterpret_cast<uint8_t*>(end64);
    // Last few bytes of bitmap
    while(start8 != reinterpret_cast<uint8_t*>(basePtr + bitmapSize)){
        if (*start8 != u8Mask) {
            uint8_t before = *start8;
            // Count trailing ones is first non 0xFF byte
            uint8_t trones = std::countr_one(before);
            // Update the bitmap
            *start8 |= 1 << trones;
            return blockStart + ((start8 - basePtr) * 8 + trones)*blockSize;
        }
        start8++;
    }

    // Returns 0 upon failure
    return nullptr;
}*/

[[gnu::always_inline]] void PageData::resetHead() {
    head = -1;
    char* obj = blockStart + bitmapSize * 8 * blockSize;
    for(int16_t i = bitmapSize*8; i >= 0; i--){
        if(!testAllocatedBit(i)) {
            *reinterpret_cast<int64_t *>(obj) = head;
            head = i;
        }
        obj-=blockSize;
    }
}
[[gnu::always_inline]] char* PageData::alloc(){
    if(head == -1) return nullptr;
    int16_t * obj = reinterpret_cast<int16_t *>(blockStart + head * blockSize);
    setAllocatedBit(head);
    head = *obj;
    return reinterpret_cast<char *>(obj);
}

void PageData::clearFreeBitmap(){
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3) // Have to use junk outputs to let gcc know there registers are clobbered
            : "D"(basePtr), "c"((blockStart-basePtr)/8), "a"(0)
            : "memory"// Clobbered registers
            );
}

[[gnu::always_inline]] void PageData::setAllocatedBit(uint64_t offset){
    // Assumes little endian
    uint64_t byteOffset = offset >> 3;
    uint8_t bitMask = 1 << (offset & 7);
    *(basePtr+byteOffset) |= bitMask;
}

[[gnu::always_inline]] bool PageData::testAllocatedBit(uint64_t offset){
    // Assumes little endian
    uint64_t byteOffset = offset / 8;
    uint8_t bitMask = 1 << (offset%8);
    return (*(basePtr+byteOffset)) & bitMask;
}

MemoryPool::MemoryPool(uint64_t pageSize, uint64_t blockSize) : pageSize(pageSize), blockSize(blockSize) {
    // Calculates number of objects that can be allocated in a single page such that the page can still fit the bitmap info
    blocksPerPage = (8*pageSize - 64) / (1+8*blockSize);
    blockStartOffset = blocksPerPage/8 + (8 - (blocksPerPage/8)%8);
    firstNonFullPage = nullptr;
    allocNewPage();
}

MemoryPool::MemoryPool() {
    pageSize = 0;
    blockSize = 0;
    blocksPerPage = 0;
    firstNonFullPage = 0;
    blockStartOffset = 0;
}

void* MemoryPool::alloc() {
    void* ptr = nullptr;
    while(!(ptr = firstNonFullPage->alloc())){
        if(firstNonFullPage == &pages.back()){
            allocNewPage();
        }else firstNonFullPage++;
    }
    return ptr;
}

bool MemoryPool::allocedByThisPool(uintptr_t ptr){
    #ifdef GC_DEBUG
    for(PageData& page : pages){
        int64_t diff = (char*)ptr - (page.blockStart);
        if(diff >= 0 && diff < pageSize-page.bitmapSize && diff%blockSize == 0 && page.testAllocatedBit(diff/blockSize)) {
            return true;
        }
    }
    return false;
    #else
    return std::any_of(std::execution::par_unseq, pages.begin(), pages.end(), [this, ptr](PageData& page){
            int64_t diff = (char *) ptr - (page.blockStart);
            return diff >= 0 && diff < pageSize-page.bitmapSize && diff % blockSize == 0 && page.testAllocatedBit(diff/blockSize);
    });
    #endif
}
void MemoryPool::clearFreeBitmap(){
    for(PageData& page : pages) page.clearFreeBitmap();
    #ifdef GC_DEBUG
    if(pages.size() > 1) std::cout<<"N of cleared pages: "<<pages.size()<<"\n";
    #endif
    firstNonFullPage = &pages.front();
}

// Page idx is stored in objects at ptr, but memory pools treat all pointers as being opaque so we pass this from outside
void MemoryPool::markBlock(uintptr_t ptr){
    uint8_t* basePtr = reinterpret_cast<uint8_t *>(ptr & (u64Mask << 16));
    // Byte offset to start of blocks, divided by block size it gives the bit position
    uint64_t offset = (uint8_t*)ptr - (basePtr + blockStartOffset);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    offset /= blockSize;
    uint64_t byteOffset = offset / 8;
    uint8_t bitMask = 1 << (offset%8);
    *(basePtr+byteOffset) |= bitMask;
}

bool MemoryPool::isFree( uintptr_t ptr){
    uint8_t* basePtr = reinterpret_cast<uint8_t *>(ptr & (u64Mask << 16));
    // Byte offset to start of blocks, divided by block size it gives the bit position
    uint64_t offset = (uint8_t*)ptr - (basePtr + blockStartOffset);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    offset /= blockSize;
    // Assumes little endian
    uint64_t byteOffset = offset / 8;
    uint8_t bitMask = 1 << (offset%8);
    return !((*(basePtr+byteOffset)) & bitMask);
}

void MemoryPool::resetHead(){
    #ifdef GC_DEBUG
    for(int i = 0; i < pages.size(); i++){
        pages[i].resetHead();
    }
    #else
    std::for_each(std::execution::par_unseq, pages.begin(), pages.end(), [](PageData& page){
        page.resetHead();
    });
    #endif
}

void MemoryPool::allocNewPage(){
    // VirtualAlloc and mmap return zeroed memory
    #ifdef _WIN32
        void* page = VirtualAlloc(nullptr, pageSize, MEM_COMMIT, PAGE_READWRITE);
    #else
        void* page = mprotect(NULL, pageSize, PROT_READ | PROT_WRITE);
    #endif
    // TODO: right now bitmapSize is not equal to the amount of blocks that can be placed but rather blocksPerPage - blocksPerPage mod 8, fix this
    // There might be some unused bytes before block start, we do this to have 8 byte alignment for blocks
    pages.emplace_back((char*)page, (char*)page + blocksPerPage/8 + (8 - (blocksPerPage/8)%8), blockSize, blocksPerPage/8);
    firstNonFullPage = &pages.back();
    firstNonFullPage->resetHead();

}

// TODO: Make this actually work?
void MemoryPool::freePage(uint32_t pid) {
    PageData& data = pages[pid];
    #ifdef _WIN32
    VirtualFree((void*)data.basePtr, 0, MEM_RELEASE);
    #else
    munmap((void*)data.basePtr, pageSize);
    #endif
    pages.erase(pages.begin() + pid);
}
