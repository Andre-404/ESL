#pragma once
#include "../../common.h"
#include <atomic>
#include <immintrin.h>

// Maximum number of pages in a memory pool. Note that maximum heap size = 6 *
// PAGE_SIZE * MAX_PAGE_CNT as we host 6 memory pools.
#define MAX_PAGE_CNT 4096

namespace memory {
struct PageData {
  int bitmapSize;
  int blockSize;
  char *basePtr;
  char *blockStart;
  int16_t head;

  uint64_t *end64;
  __m256i *end256;
  uint8_t *lastBitmapPos;

  PageData(char *basePtr, char *blockStart, uint64_t blockSize, int bitmapSize);
  PageData();

  char *firstFreeBlock();
  void clearFreeBitmap();
  void setAllocatedBit(uint64_t offset);
  bool testAllocatedBit(uint64_t offset);
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

  uint8_t *getPageBasePtr(uint32_t pid);
  uint8_t *getPageBlockStart(uint32_t pid);
  uint64_t *getPageEnd64(uint32_t pid);
  __m256i *getPageEnd256(uint32_t pid);

  uint8_t *firstFreeBlock(uint32_t pid);
  void clearFreeBitmap(uint32_t pid);
  void setAllocatedBit(uint32_t pid, uint32_t offset);
  bool testAllocatedBit(uint32_t pid, uint32_t offset);
};
} // namespace memory
