#include "memoryPool.h"
#include <cassert>
#include <bit>
#include <atomic>
#include <algorithm>
#include <execution>
#include <unistd.h>

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

static constexpr uint64_t i64Mask = 0xffffffffffffffff;
static constexpr uint8_t i8Mask = 0xff;

// Finds first free block or returns nullptr
char* PageData::firstFreeBlock(){
    uint8_t* start8 = reinterpret_cast<uint8_t *>(lastBitmapPos);
    while(*start8 == 0xFF && start8 != reinterpret_cast<uint8_t*>(basePtr + bitmapSize)){
        start8++;
    }
    if (start8 != reinterpret_cast<uint8_t*>(basePtr + bitmapSize)) {
        uint8_t before = *start8;
        // Count trailing ones is first non 0xFF byte
        uint8_t trones = std::countr_one(before);
        // Update the bitmap
        *start8 |= 1 << trones;
        lastBitmapPos = start8;
        return blockStart + (((char *) start8 - basePtr) * 8 + trones)*blockSize;
    }
    return nullptr;
    /*
    // These 2 loops follow the same principle as the first one but use smaller granularity
    // Scan 256 bits at once, assumes little endian
    __m256i* start256 = reinterpret_cast<__m256i *>(lastBitmapPos);
    while(start256 != end256){
        __m256i vec = _mm256_load_si256(start256);
        // Compare each byte in parallel
        __m256i vnonzero = _mm256_cmpeq_epi8(vec, _mm256_set1_epi64x(0xFFFFFFFFFFFFFFFF));
        // One bit for each byte in ymm register that represents if that byte is equal to 0xFF(no free blocks)
        uint32_t nzmask = ~_mm256_movemask_epi8(vnonzero);
        // Nzmask is negated to allow for ctzl
        if(nzmask != 0){
            // Can safely call the builtin since we know that nazmask isn't 0
            unsigned tzbytes = __builtin_ctzl(nzmask);
            uint8_t *nz_elem = (uint8_t *)start256 + tzbytes;
            uint8_t before = *nz_elem;

            // Count trailing ones is first non 0xFF byte
            uint8_t trailingOnes = std::countr_one(before);
            *nz_elem |= ( 1 << trailingOnes);
            // Store the last bitmap pos
            lastBitmapPos = reinterpret_cast<uint8_t *>(start256);
            // Calculate total number of trailing ones in the original register
            uint32_t trones = 8 * tzbytes + trailingOnes;
            return blockStart + (((char *) start256 - basePtr) * 8 + trones)*blockSize;
        }
        start256++;
    }

    // These 2 loops follow the same principle as the first one but use smaller granularity
    uint64_t* start64 = reinterpret_cast<uint64_t *>(end256);
    while(start64 != end64){
        if (*start64 != i64Mask) {
            uint64_t before = *start64;
            uint8_t trones = std::countr_one(before);
            // Update the bitmap
            *start64 |= 1 << trones;
            return blockStart + (((char *) start64 - basePtr) * 8 + trones)*blockSize;
        }
        start64++;
    }
    uint8_t* start8 = reinterpret_cast<uint8_t *>(end64);
    // Last few bytes of bitmap
    while(start8 != reinterpret_cast<uint8_t*>(basePtr + bitmapSize)){
        if (*start8 != i8Mask) {
            uint8_t before = *start8;
            // Count trailing ones is first non 0xFF byte
            uint8_t trones = std::countr_one(before);
            // Update the bitmap
            *start8 |= 1 << trones;
            return blockStart + (((char *) start8 - basePtr) * 8 + trones)*blockSize;
        }
        start8++;
    }

    // Returns 0 upon failure
    return nullptr;*/
}

void PageData::clearFreeBitmap(){
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3)// Have to use junk outputs to let gcc know there registers are clobbered
            : "D"(basePtr), "c"((blockStart-basePtr)/8), "a"(0)
            : "memory"// Clobbered registers
            );
    lastBitmapPos = reinterpret_cast<uint8_t *>(basePtr);
}

void PageData::setAllocatedBit(uint64_t offset){
    // Assumes little endian
    uint64_t byteOffset = offset / 8;
    uint8_t bitMask = 1 << (offset%8);
    *(basePtr+byteOffset) |= bitMask;
}

bool PageData::testAllocatedBit(uint64_t offset){
    // Assumes little endian
    uint64_t byteOffset = offset / 8;
    uint8_t bitMask = 128 >> (offset&8);
    return (*(basePtr+offset)) & bitMask;
}

MemoryPool::MemoryPool(uint64_t pageSize, uint64_t blockSize) : pageSize(pageSize), blockSize(blockSize) {
    // Calculates number of objects that can be allocated in a single page such that the page can still fit the bitmap info
    blocksPerPage = (8*pageSize - 64) / (1+8*blockSize);
    firstNonFullPage = 0;
    allocNewPage();
}

MemoryPool::MemoryPool() {
    pageSize = 0;
    blockSize = 0;
    blocksPerPage = 0;
    firstNonFullPage = 0;
}

void* MemoryPool::alloc(uint32_t* pageIdx) {
    void* ptr = nullptr;
    while(!(ptr = pages[firstNonFullPage].firstFreeBlock())){
        firstNonFullPage++;
        if(firstNonFullPage == pages.size()){
            allocNewPage();
        }
    }
    *pageIdx = firstNonFullPage;
    return ptr;
}

bool MemoryPool::allocedByThisPool(uintptr_t ptr){
    return std::any_of(std::execution::par_unseq, pages.begin(), pages.end(), [this, ptr](PageData& page){
        int64_t diff = (char*)ptr - (page.blockStart);
        return diff >= 0 && diff < pageSize && diff%blockSize == 0 && page.testAllocatedBit(diff);
    });
}
void MemoryPool::clearFreeBitmap(){
    for(PageData& page : pages) page.clearFreeBitmap();
    firstNonFullPage = 0;
}

// Page idx is stored in objects at ptr, but memory pools treat all pointers as being opaque so we pass this from outside
void MemoryPool::markBlock(uint32_t pageIdx, uintptr_t ptr){
    PageData& page = pages[pageIdx];
    // Byte offset to start of blocks, divided by block size it gives the bit position
    uint64_t offset = (char*)ptr - (page.blockStart);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    page.setAllocatedBit(offset/blockSize);
}

bool MemoryPool::isFree(uint32_t pageIdx, uintptr_t ptr){
    PageData& page = pages[pageIdx];
    uint64_t offset = (char*)ptr - (page.blockStart);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    return !page.testAllocatedBit(offset/blockSize);
}

void MemoryPool::allocNewPage(){
    void* page = _aligned_malloc(pageSize, 32);
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3)// No output operands
            : "D"(page), "c"(pageSize/8), "a"(0) // Input operands
            : "memory"// Clobbered registers
            );
    // TODO: right now bitmapSize is not equal to the amount of blocks that can be placed but rather blocksPerPage - blocksPerPage mod 8, fix this
    // There might be some unused bytes before block start, we do this to have 8 byte alignment for blocks
    pages.emplace_back((char*)page, (char*)page + blocksPerPage/8 + (8 - (blocksPerPage/8)%8), blockSize, blocksPerPage/8);
    firstNonFullPage = pages.size()-1;
}

void MemoryPool::freePage(int pageIdx) {
    PageData& data = pages[pageIdx];
    free(data.basePtr);
    pages.erase(pages.begin() + pageIdx);
}
