#include "memoryPool.h"
#include <string.h>
#include <cassert>
#include <bit>

using namespace memory;

PageData::PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize)
: basePtr(basePtr), blockStart(blockStart),blockSize(blockSize), bitmapSize(bitmapSize) {}
PageData::PageData(){
    basePtr = nullptr;
    blockStart = nullptr;
    blockSize = 0;
    bitmapSize = 0;
}

static constexpr uint64_t i64Mask = 0xffffffffffffffff;
static constexpr char i8Mask = 0xff;

// Finds first free block or returns nullptr
char* PageData::firstFreeBlock(){
    uint64_t * pbi = (uint64_t*) basePtr;
    // Last 8 bytes of a bitmap contain end of bitmap + padding for blockStart
    uint64_t* pbiUpper = (uint64_t*)blockStart - 1;
    // Assumes little endian, doesn't work otherwise
    while(pbi + 2 < pbiUpper) {
        if (*pbi != i64Mask) {
            uint64_t freeBlocks = *pbi;
            uint64_t offset = ((char *) pbi - basePtr) * 8 + std::countr_one(freeBlocks);
            setAllocatedBit(offset);
            return blockStart + offset*blockSize;
        }
        pbi++;
        if (*pbi != i64Mask) {
            uint64_t freeBlocks = *pbi;
            uint64_t offset = ((char *) pbi - basePtr) * 8 + std::countr_one(freeBlocks);
            setAllocatedBit(offset);
            return blockStart + offset*blockSize;
        }
        pbi++;
    }
    // Last few bytes of bitmap
    for (char* p = (char*) pbi; p < ((char*) basePtr) + bitmapSize; p++) {
        if (*p != i8Mask) {
            uint8_t freeBlocks = *p;
            uint64_t offset = ((char *) p - basePtr) * 8 + std::countr_one(freeBlocks);
            setAllocatedBit(offset);
            return blockStart + offset*blockSize;
        }
    }
    // Returns 0 upon failure
    return nullptr;
}

void PageData::clearFreeBitmap(){
    // TODO: optimize with custom(parallelized) memset?
    memset(basePtr, 0, bitmapSize);
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
}

MemoryPool::MemoryPool() {
    pageSize = 0;
    blockSize = 0;
    blocksPerPage = 0;
    firstNonFullPage = 0;
}

void* MemoryPool::alloc(uint32_t* pageIdx) {
    void* ptr = nullptr;
    if(pages.empty() || (firstNonFullPage == pages.size()-1 && !(ptr = pages[firstNonFullPage].firstFreeBlock()))){
        allocNewPage();
    }
    if(!ptr) ptr = pages[firstNonFullPage].firstFreeBlock();
    // If firstFreeBlock doesn't return nullptr it automatically sets the allocated bit
    while(!ptr) {
        firstNonFullPage++;
        ptr = pages[firstNonFullPage].firstFreeBlock();
    }
    *pageIdx = firstNonFullPage;
    return ptr;
}
bool MemoryPool::allocedByThisPool(uintptr_t ptr){
    for(PageData& page : pages){
        if((uintptr_t)(page.blockStart) <= ptr && ptr < (uintptr_t)(page.basePtr+pageSize)){
            uint64_t diff = (char*)ptr - (page.blockStart);
            if(diff%blockSize == 0) return true;
        }
    }
    return false;
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
    void* page = calloc(pageSize, 1);
    // TODO: right now bitmapSize is not equal to the amount of blocks that can be placed but rather blocksPerPage - blocksPerPage mod 8, fix this
    // There might be some unused bytes before block start, we do this to have 8 byte alignment for blocks
    pages.emplace_back((char*)page, (char*)page + blocksPerPage/8 + (blocksPerPage/8)%8, blockSize, blocksPerPage/8);
    firstNonFullPage = pages.size()-1;
}

void MemoryPool::freePage(int pageIdx) {
    PageData& data = pages[pageIdx];
    free(data.basePtr);
    pages.erase(pages.begin() + pageIdx);
}
