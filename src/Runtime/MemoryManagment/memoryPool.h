#pragma once
#include "../../common.h"

namespace memory{
    struct BlockHeader{
        BlockHeader* next;
    };
    struct PageData{
        PageData(void *basePtr, uint64_t size);
        PageData();

        void* basePtr;
        uint64_t size;

    };
    class MemoryPool{
    public:
        MemoryPool(uint64_t pageSize, uint64_t blockSize);
        MemoryPool();

        void* alloc();
        void free(void* ptr);
    private:
        BlockHeader* head;
        vector<PageData> pages;
        vector<BlockHeader*> v;
        uint64_t pageSize;
        uint64_t blockSize;
        void allocNewPage();
        void freePage(int pageIdx);
    };
}