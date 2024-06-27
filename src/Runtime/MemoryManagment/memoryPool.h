#pragma once
#include "../../common.h"
#include <atomic>
#include <immintrin.h>

// Maximum number of pages in a memory pool. Note that maximum heap size = 6 *
// PAGE_SIZE * MAX_PAGE_CNT as we host 6 memory pools.
#define MAX_PAGE_CNT 4096

namespace memory {
    static constexpr int16_t whiteAndAllocatedBlock = -2;
    static constexpr int16_t blackBlock = -3;
    struct PageData {
        int blockSize;
        char *basePtr;
        int16_t head;
        int16_t numBlocks;

        PageData(char *basePtr, int blockSize);
        PageData();
        void resetPage();

        template<size_t blockSize>
            char* alloc();
    };
    class MemoryPool {
        public:
            MemoryPool(uint32_t blockSize);
            MemoryPool();
            template<size_t blockSize>
                void* alloc();
            bool allocedByThisPool(uintptr_t ptr);
            void resetPages();
        private:
            uint32_t blockSize;
            PageData* firstNonFullPage;
            vector<PageData> pages;
            void allocNewPage();
            void freePage(uint32_t pid);
    };
} // namespace memory
