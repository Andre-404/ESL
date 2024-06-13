#pragma once
#include "../../common.h"

namespace memory{
    struct BlockHeader{
        BlockHeader* next;
    };
    struct PageData{
        int bitmapSize;
        char* basePtr;
        char* blockStart;
        uint64_t blockSize;

        PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize);
        PageData();

        char* firstFreeBlock();
        void clearFreeBitmap();
        void setAllocatedBit(uint64_t offset);
        bool testAllocatedBit(uint64_t offset);
    };
    class MemoryPool{
    public:
        MemoryPool(uint64_t pageSize, uint64_t blockSize);
        MemoryPool();

        void* alloc(uint32_t* pageIdx);
        bool allocedByThisPool(uintptr_t ptr);
        void clearFreeBitmap();
        void markBlock(uint32_t pageIdx, uintptr_t ptr);
        bool isFree(uint32_t pageIdx, uintptr_t ptr);
    private:
        vector<PageData> pages;
        uint64_t pageSize;
        uint64_t blockSize;
        uint64_t blocksPerPage;
        uint32_t firstNonFullPage;
        void allocNewPage();
        void freePage(int pageIdx);
    };
}