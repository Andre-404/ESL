#pragma once
#include "../../common.h"
#include <atomic>
#include <immintrin.h>

// Maximum number of pages in a memory pool. Note that maximum heap size = 6 *
// PAGE_SIZE * MAX_PAGE_CNT as we host 6 memory pools.
#define MAX_PAGE_CNT 4096

namespace memory {
struct PageData {
  int blockSize;
  char *basePtr;
  int16_t head;
  int16_t numBlocks;

  PageData(char *basePtr, uint64_t blockSize);
  PageData();

  void resetPage();
  char *alloc();
};
class MemoryPool {
public:
  MemoryPool(uint64_t blockSize);
  MemoryPool();
  void *alloc();
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
