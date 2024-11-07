#pragma once
#include "../../common.h"
#include <mutex>
#include <array>

namespace memory{
#define SMALL_GRANULARITY 16u
#define SMALL_SIZE_CLASSES 32u
#define MEDIUM_GRANULARITY 128u
#define MEDIUM_SIZE_CLASSES (MP_CNT - SMALL_SIZE_CLASSES)

    constexpr std::array<size_t, MP_CNT> generateBlockSizes(size_t shift1, size_t n1, size_t shift2, size_t n2){
        std::array<size_t, MP_CNT> arr;
        for(int i = 0; i < n1; i++){
            arr[i] = (i+1)*shift1;
        }
        for(int i = 0; i < n2; i++){
            arr[n1+i] = (i+1)*shift2 + n1*shift1;
        }

        return arr;
    }

    constexpr std::array<size_t, MP_CNT> mpBlockSizes = generateBlockSizes(SMALL_GRANULARITY, SMALL_SIZE_CLASSES,
                                                                           MEDIUM_GRANULARITY, MEDIUM_SIZE_CLASSES);
    struct PageData {
        char *basePtr;
        PageData* next;
        uint16_t blockSize;
        // How many blocks were alive during the last GC collection
        // Decreased every time an allocated block is encountered to hopefully speed up allocation
        uint16_t numAllocBlocks;
        // Allocator uses sequential-fit when searching through page, this keeps track of where it stopped
        uint16_t head;
        uint16_t numBlocks;
    };

    class ThreadArena;

    class HeapPageManager{
    public:
        HeapPageManager();
        PageData* allocatePage(uint32_t sizeClassIdx);
        PageData* getPageFromPtr(char* ptr);
        uint32_t updateEmptyBuffer();
        void prepForCollection();
        void moveArenaPagesToGraveyard(ThreadArena& arena);
    private:
        // This needs to be a vector to allow for efficient binary search
        vector<PageData*> inUse;
        vector<PageData*> empty;
        // When a thread ends its execution it's pages get put in the graveyard since they might have objects allocated on them
        std::array<PageData*, MP_CNT> graveyard;
        std::mutex allocMtx;
        void cleanUpPage(PageData* page, uint32_t newSizeClass);
    };
}
