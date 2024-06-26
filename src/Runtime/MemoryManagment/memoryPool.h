#pragma once
#include "../../common.h"
#include <atomic>
#include <immintrin.h>

// Maximum number of pages in a memory pool. Note that maximum heap size = 6 *
// PAGE_SIZE * MAX_PAGE_CNT as we host 6 memory pools.
#define MAX_PAGE_CNT 4096

namespace memory {
    template<size_t blockSize>
    struct PageData {
        static constexpr uint16_t numBlocks = PAGE_SIZE / blockSize;
        char *basePtr;
        uint16_t head;

        PageData(char *basePtr);
        PageData();

        void resetPage();
        char *alloc();
    };

    template<size_t blockSize>
    class MemoryPool {
        public:
            MemoryPool();
            void *alloc();
            bool allocedByThisPool(uintptr_t ptr);
            void resetPages();

        private:
            PageData<blockSize>* firstNonFullPage;
            vector<PageData<blockSize>> pages;
            void allocNewPage();
            void freePage(uint32_t pid);
    };
} // namespace memory
