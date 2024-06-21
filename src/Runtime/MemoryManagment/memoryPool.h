#pragma once
#include "../../common.h"
#include <immintrin.h>
#include <atomic>

// Maximum number of pages in a memory pool. Note that maximum heap size = 6 * PAGE_SIZE * MAX_PAGE_CNT as we host 6 memory pools.
#define MAX_PAGE_CNT 4096

namespace memory{
    struct BlockHeader{
        BlockHeader* next;
    };
    // struct PageData{
    //     char* blockStart;
    //     uint64_t* end64;
    //     __m256i* end256;
    //     uint8_t* lastBitmapPos;

    //     PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize);
    //     PageData();
    // };
    class MemoryPool{
    public:
        MemoryPool(uint64_t pageSize, uint64_t blockSize);
        MemoryPool();

        void* alloc(uint32_t& pid);
        bool allocedByThisPool(uintptr_t ptr);
        void clearFreeBitmaps();
        void markBlock(uint32_t pid, uintptr_t ptr);
        bool isFree(uint32_t pid, uintptr_t ptr);
    private:
        uint8_t* mpStart;
        uint32_t pageCnt;

        size_t blockStartOffset; // blockStart = pageStart + blockStartOffset
        size_t blockSize;
        size_t bitmapSize;
        uint32_t blocksPerPage;
        size_t firstNonFullPage;
        size_t pageSize;
        vector<uint8_t*> lastBitmapPoss;
        void allocNewPage();
        void freePage(uint32_t pid);

        uint8_t* getPageBasePtr(uint32_t pid);
        uint8_t* getPageBlockStart(uint32_t pid);
        uint64_t* getPageEnd64(uint32_t pid); 
        __m256i* getPageEnd256(uint32_t pid);

        uint8_t* firstFreeBlock(uint32_t pid);
        void clearFreeBitmap(uint32_t pid);
        void setAllocatedBit(uint32_t pid, uint32_t offset);
        bool testAllocatedBit(uint32_t pid, uint32_t offset);
    };
}
