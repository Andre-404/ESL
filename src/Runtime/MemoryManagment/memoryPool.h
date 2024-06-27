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

  PageData(char *basePtr, uint64_t blockSize);
  PageData();

  void resetPage();

  template<int bSize>
  [[gnu::hot]] char *alloc(){
      if(head == numBlocks) return nullptr;
      // Go until you find and non-black block, only black blocks are not free after a gc
      int16_t* obj = reinterpret_cast<int16_t *>(basePtr + head * bSize);
      while(head < numBlocks && *obj == blackBlock){
          // If we find a black block reset its marked flag
          *obj = whiteAndAllocatedBlock;
          head++;
          obj = reinterpret_cast<int16_t *>((char *) (obj) + bSize);
      }
      // If the loop exited because every block was taken, bail out
      if(head == numBlocks) return nullptr;
      *obj = whiteAndAllocatedBlock;
      head++;
      return reinterpret_cast<char *>(obj);
  }
};
class MemoryPool {
public:
  MemoryPool(uint64_t blockSize);
  MemoryPool();
  template<int blockSize>
  [[gnu::hot]] void *alloc(){
      void *ptr = nullptr;
      while (!(ptr = firstNonFullPage->alloc<blockSize>())) {
          if (firstNonFullPage == &pages.back()) {
              allocNewPage();
          } else
              firstNonFullPage++;
      }
      return ptr;
  }
  bool allocedByThisPool(uintptr_t ptr);
  void resetPages();

private:
  int blockSize;
  PageData* firstNonFullPage;
  vector<PageData> pages;
  void allocNewPage();
  void freePage(uint32_t pid);
};
} // namespace memory
