#pragma once
#include "../../common.h"
#include "heapPageManager.h"
#include <array>

namespace object{
    class Obj;
}

namespace memory {
    enum class GCAllocType{ MALLOC = MP_CNT, CONSTANT = 128 };
    inline constexpr byte operator+ (GCAllocType const val) { return static_cast<byte>(val); }
    enum GCBlockColor{ WHITE = 1, BLACK = 2, CONSTANT = 3 };
    inline constexpr int16_t operator+ (GCBlockColor const val) { return static_cast<int16_t>(val); }

    class ThreadArena {
    public:
        ThreadArena();
        ~ThreadArena();
        void *alloc(const size_t size);

        PageData* getMemoryPool(size_t idx);
        PageData* getFirstFreePage(size_t idx);
        void resetFirstFreePage(size_t idx);
        vector<object::Obj*>& getTempStorage();
        void updateMemoryPools(uint32_t gcHeapVer);
    private:
        // In essence, pool is the tail, and firstFreePage is the head of a list of page data
        std::array<PageData*, MP_CNT> firstFreePage;
        std::array<PageData*, MP_CNT> pools;
        // Allows for lock free allocation of large objects
        vector<object::Obj*> tempLargeObjectStorage;
        // At any point in time during the program run, except for during GC collection,
        // this heapVersion and GC heapVersion need to match
        uint32_t heapVersion;

        void* fastAlloc(size_t poolIdx);
    };
    ThreadArena& getLocalArena();
    // Reverts white objects that are alive back to black objects
    void sweepPage(PageData& page);
}
