#pragma once
#include "../../common.h"
#include <immintrin.h>
#include <atomic>

namespace memory{
    struct PageData{
        int bitmapSize;
        int blockSize;
        char* basePtr;
        char* blockStart;
        int16_t head;

        uint64_t* end64;
        __m256i* end256;
        uint8_t* lastBitmapPos;


        PageData(char *basePtr, char* blockStart, uint64_t blockSize, int bitmapSize);
        PageData();

        char* firstFreeBlock();
        void clearFreeBitmap();
        void setAllocatedBit(uint64_t offset);
        bool testAllocatedBit(uint64_t offset);
        void resetHead();
        char* alloc();
    };
    class MemoryPool{
    public:
        MemoryPool(uint64_t pageSize, uint64_t blockSize);
        MemoryPool();

        void* alloc();
        bool allocedByThisPool(uintptr_t ptr);
        void clearFreeBitmap();
        void markBlock(uintptr_t ptr);
        bool isFree(uintptr_t ptr);
        void resetHead();
    private:
        int blockSize;
        int blocksPerPage;
        int blockStartOffset;
        PageData* firstNonFullPage;
        vector<PageData> pages;
        uint64_t pageSize;
        void allocNewPage();
        void freePage(int pageIdx);
    };
}