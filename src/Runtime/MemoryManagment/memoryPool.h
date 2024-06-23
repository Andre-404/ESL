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

  PageData(char *basePtr, uint64_t blockSize);
  PageData();

  void resetHead();
  char *alloc();
};
class MemoryPool {
public:
  MemoryPool(uint64_t blockSize);
  MemoryPool();
  void *alloc();
  bool allocedByThisPool(uintptr_t ptr);
  void resetPages();
  void resetHead();

private:
  int blockSize;
  int blocksPerPage;
  int blockStartOffset;
  PageData *firstNonFullPage;
  vector<PageData> pages;
  void allocNewPage();
  void freePage(uint32_t pid);

  uint8_t *firstFreeBlock(uint32_t pid);
  void clearFreeBitmap(uint32_t pid);
};
} // namespace memory
