#include "memoryPool.h"
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

// PageData::PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize)
// : basePtr(basePtr), blockStart(blockStart),blockSize(blockSize), bitmapSize(bitmapSize) {
//     end64 = reinterpret_cast<uint64_t *>(basePtr) + bitmapSize / 8;
//     end256 = reinterpret_cast<__m256i *>(basePtr) + bitmapSize / 32;
//     lastBitmapPos = reinterpret_cast<uint8_t *>(basePtr);
// }
// PageData::PageData(){
//     basePtr = nullptr;
//     blockStart = nullptr;
//     end64 = nullptr;
//     blockSize = 0;
//     bitmapSize = 0;
//     lastBitmapPos = nullptr;
// }

static constexpr uint64_t i64Mask = 0xffffffffffffffff;
static constexpr uint8_t i8Mask = 0xff;

[[gnu::always_inline]] uint8_t* MemoryPool::getPageBasePtr(uint32_t pid){
    return mpStart + pageSize * pid;
}

[[gnu::always_inline]] uint8_t* MemoryPool::getPageBlockStart(uint32_t pid){
    return getPageBasePtr(pid) + blockStartOffset;
}

[[gnu::always_inline]] uint64_t* MemoryPool::getPageEnd64(uint32_t pid){
    return reinterpret_cast<uint64_t*>(getPageBasePtr(pid)) + (bitmapSize >> 3);
}

[[gnu::always_inline]] __m256i* MemoryPool::getPageEnd256(uint32_t pid){
    return reinterpret_cast<__m256i*>(getPageBasePtr(pid)) + (bitmapSize >> 5);
}

// Finds first free block or returns nullptr
uint8_t* MemoryPool::firstFreeBlock(uint32_t pid){
    // Regenerate page details
    auto end64 = getPageEnd64(pid);
    auto end256 = getPageEnd256(pid);
    auto basePtr = getPageBasePtr(pid);
    auto blockStart = getPageBlockStart(pid);
    
    // uint64_t * pbi = (uint64_t*) basePtr;
    // uint64_t* pbiUpper = ((uint64_t*) (((char*) basePtr) + bitmapSize)) - 1;
    // for (;pbi <= pbiUpper; pbi++) {
    //     if (*pbi != i64Mask) {
    //         uint64_t freeBlocks = *pbi;
    //         uint64_t offset = ((char *) pbi - basePtr) * 8 + std::countr_one(freeBlocks);
    //         setAllocatedBit(offset);
    //         return blockStart + offset*blockSize;
    //     }
    // }
    // for (char* p = (char*) pbi; p < ((char*) basePtr) + bitmapSize; p++) {
    //     if (*p != i8Mask) {
    //         uint8_t freeBlocks = *p;
    //         uint64_t offset = ((char *) p - basePtr) * 8 + std::countr_one(freeBlocks);
    //         setAllocatedBit(offset);
    //         return blockStart + offset*blockSize;
    //     }
    // }
    // return nullptr;

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
        if (*start64 != i64Mask) {
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
        if (*start8 != i8Mask) {
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
}

void MemoryPool::clearFreeBitmap(uint32_t pid){
    auto basePtr = getPageBasePtr(pid);
    auto blockStart = getPageBlockStart(pid);
    void* junkVar1; uint64_t junkVar2; uint64_t junkVar3;
    asm volatile (
            "rep stosq"
            : "=D"(junkVar1), "=c"(junkVar2), "=a"(junkVar3) // Have to use junk outputs to let gcc know there registers are clobbered
            : "D"(basePtr), "c"((blockStart-basePtr)/8), "a"(0)
            : "memory"// Clobbered registers
            );
}

void MemoryPool::setAllocatedBit(uint32_t pid, uint32_t offset){
    auto basePtr = getPageBasePtr(pid);
    // Assumes little endian
    uint64_t byteOffset = offset >> 3;
    uint8_t bitMask = 1 << (offset & 7);
    *(basePtr+byteOffset) |= bitMask;
}

bool MemoryPool::testAllocatedBit(uint32_t pid, uint32_t offset){
    auto basePtr = getPageBasePtr(pid);
    // Assumes little endian
    uint64_t byteOffset = offset >> 3;
    uint8_t bitMask = 1 << (offset & 7);
    return (*(basePtr+byteOffset)) & bitMask;
}

MemoryPool::MemoryPool(uint64_t pageSize, uint64_t blockSize) : pageSize(pageSize), blockSize(blockSize) {
    // Calculates number of objects that can be allocated in a single page such that the page can still fit the bitmap info
    blocksPerPage = ((pageSize << 3) - 64) / (1 + (blockSize << 3));
    // TODO: right now bitmapSize is not equal to the amount of blocks that can be placed but rather blocksPerPage - blocksPerPage mod 8, fix this
    // There might be some unused bytes before block start, we do this to have 8 byte alignment for blocks
    bitmapSize = blocksPerPage >> 3;
    firstNonFullPage = 0;
    blockStartOffset = bitmapSize + (8 - (bitmapSize & 7));
    pageCnt = 0;
    #ifdef _WIN32
        mpStart = reinterpret_cast<uint8_t*>(VirtualAlloc(nullptr, static_cast<int64_t>(pageSize) * MAX_PAGE_CNT, MEM_RESERVE, PAGE_READWRITE));
    #else
        mpStart = reinterpret_cast<uint8_t*>(mmap(NULL, static_cast<int64_t>(pageSize) * MAX_PAGE_CNT, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0));
    #endif
    lastBitmapPos = mpStart;
    allocNewPage();
}

MemoryPool::MemoryPool() {
    pageSize = 0;
    blockSize = 0;
    blocksPerPage = 0;
    firstNonFullPage = 0;
    pageCnt = 0;
}

void* MemoryPool::alloc(uint32_t& pid) {
    void* ptr = nullptr;
    while(!(ptr = firstFreeBlock(firstNonFullPage))){
        firstNonFullPage++;
        lastBitmapPos = getPageBasePtr(firstNonFullPage);
        if(firstNonFullPage == pageCnt){
            allocNewPage();
        }
    }
    pid = firstNonFullPage;
    return ptr;
}

bool MemoryPool::allocedByThisPool(uintptr_t ptr){
    // #ifdef GC_DEBUG
    // // for(PageData& page : pages){
    // //     int64_t diff = (char*)ptr - (page.blockStart);
    // //     if(diff >= 0 && diff < pageSize-page.bitmapSize && diff%blockSize == 0 && page.testAllocatedBit(diff)) {
    // //         return true;
    // //     }
    // // }
    // return false;
    // #else
    // return std::any_of(std::execution::par_unseq, pages.begin(), pages.end(), [this, ptr](PageData& page){
    //     int64_t diff = (char*)ptr - (page.blockStart);
    //     return diff >= 0 && diff < pageSize-page.bitmapSize && diff%blockSize == 0 && page.testAllocatedBit(diff);
    // });
    int64_t pid = (ptr - reinterpret_cast<uintptr_t>(mpStart)) / pageSize; // raw dog?
    if (pid < 0 || pid >= pageCnt) { return false; }
    int64_t bid = (ptr - reinterpret_cast<uintptr_t>(getPageBlockStart(pid))) / blockSize;
    if (bid < 0 || bid >= blocksPerPage) { return false; }
    return testAllocatedBit(pid, bid);
    // #endif
}
void MemoryPool::clearFreeBitmaps(){
    for(uint32_t pid = 0; pid < pageCnt; pid++) { clearFreeBitmap(pid); }
    lastBitmapPos = getPageBasePtr(0);
    firstNonFullPage = 0;
}

// Page idx is stored in objects at ptr, but memory pools treat all pointers as being opaque so we pass this from outside
void MemoryPool::markBlock(uint32_t pid, uintptr_t ptr){
    // Byte offset to start of blocks, divided by block size it gives the bit position
    uint64_t offset = reinterpret_cast<uint8_t*>(ptr) - getPageBlockStart(pid);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    setAllocatedBit(pid, offset / blockSize);
}

bool MemoryPool::isFree(uint32_t pid, uintptr_t ptr){
    uint64_t offset = reinterpret_cast<uint8_t*>(ptr) - getPageBlockStart(pid);
    assert(offset%blockSize == 0 && "Offset isn't multiple of block size?");
    return !testAllocatedBit(pid, offset/blockSize);
}

void MemoryPool::allocNewPage(){
    // VirtualAlloc and mmap return zeroed memory
    #ifdef _WIN32
        VirtualAlloc(mpStart + pageSize * pageCnt, pageSize, MEM_COMMIT, PAGE_READWRITE);
    #else
        mprotect(mpStart + static_cast<int64_t>(pageSize) * pageCnt, pageSize, PROT_READ | PROT_WRITE);
    #endif
    pageCnt++;
    firstNonFullPage = pageCnt - 1;
}

// TODO: Make this actually work?
void MemoryPool::freePage(uint32_t pid) {
    assert(0 <= pid && pid < pageCnt);
    pageCnt--;
}
