#pragma once
#include "../../common.h"
#include <array>

namespace object{
    class Obj;
}

namespace memory {
    enum class GCAllocType{ MALLOC = MP_CNT, CONSTANT = 128 };
    inline constexpr byte operator+ (GCAllocType const val) { return static_cast<byte>(val); }
    enum GCBlockColor{ WHITE = -2, BLACK = -3 };
    inline constexpr int16_t operator+ (GCBlockColor const val) { return static_cast<int16_t>(val); }
    struct PageData {
        int blockSize;
        char *basePtr;
        int16_t head;
        int16_t numBlocks;

        PageData(char *basePtr, int blockSize) : basePtr(basePtr), head(0), blockSize(blockSize), numBlocks(PAGE_SIZE / blockSize) {}
        PageData() : basePtr(nullptr), head(0), numBlocks(0), blockSize(0) {}
    };

    class ThreadArena {
    public:
        ThreadArena();
        ~ThreadArena();
        void *alloc(const size_t size);

        vector<PageData>& getMemoryPool(size_t idx);
        PageData* getFirstFreePage(size_t idx);
        void resetFirstFreePage(size_t idx);
        vector<object::Obj*>& getTempStorage();
    private:
        std::array<PageData*, MP_CNT> firstFreePage;
        std::array<vector<PageData>, MP_CNT> pools;
        vector<object::Obj*> tempLargeObjectStorage;

        void allocNewPage(size_t poolIdx);

        void freePage(uint32_t pid);

        //template<size_t poolIdx>
        void* fastAlloc(size_t poolIdx);
    };
    ThreadArena& getLocalArena();
}