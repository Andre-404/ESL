#include "threadArena.h"
#include "../../Includes/fmt/format.h"
#include "../Objects/objects.h"
#include "garbageCollector.h"
#include "../../Includes/rpmalloc/rpmalloc.h"

#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif


#define LOCAL_BYTE_CACHE 1024*16 //16Kb

#if ((defined(__APPLE__) || defined(__HAIKU__)) && ENABLE_PRELOAD) || defined(__TINYC__)
static pthread_key_t _memory_thread_heap;
#else
#  ifdef _MSC_VER
#    define _Thread_local __declspec(thread)
#    define TLS_MODEL
#  else
#    ifndef __HAIKU__
#      define TLS_MODEL __attribute__((tls_model("initial-exec")))
#    else
#      define TLS_MODEL
#    endif
#    if !defined(__clang__) && defined(__GNUC__)
#      define _Thread_local __thread
#    endif
#  endif
static _Thread_local memory::ThreadArena* arena TLS_MODEL = nullptr;
#endif

using namespace memory;

[[clang::always_inline]] memory::ThreadArena& memory::getLocalArena(){
    return *arena;
}

void memory::initLocalArena(){
    arena = new memory::ThreadArena();
    rpmalloc_thread_initialize();
}

// Upon thread death, if it allocated some pages move them to a global graveyard and remove the stack start
[[clang::always_inline]] void memory::deleteLocalArena(){
    if(arena) {
        // TODO: Both of these operations needs to lock their respective mutexes, maybe try making it use only 1?
        gc->pageManager.moveArenaPagesToGraveyard(*arena);
        gc->removeStackStart(std::this_thread::get_id());
        delete arena;
        rpmalloc_thread_finalize(1);
    }
}


// Branchless
inline int szToIdx(size_t x){
    // Rounds up to the nearest multiple of GRANULARITY and divides by GRANULARITY to get position in mpBlockSizes
    int smallclassIdx = (x + (SMALL_GRANULARITY - 1)) >> std::countr_zero(SMALL_GRANULARITY);
    // For medium class subtract the highest small granularity block to get relative position from start of medium granularity
    int mediumclassIdx = (x - SMALL_GRANULARITY*SMALL_SIZE_CLASSES + (MEDIUM_GRANULARITY - 1)) >> std::countr_zero(MEDIUM_GRANULARITY);
    int smallCondition = (x <= mpBlockSizes[SMALL_SIZE_CLASSES-1]); // If x is below small class size return this index
    int mediumCondition = (x <= mpBlockSizes.back() && (1 - smallCondition));
    return smallclassIdx*smallCondition  +
           (SMALL_SIZE_CLASSES + mediumclassIdx)*mediumCondition // For medium blocks since idx is relative add SMALL_SIZE_CLASSES
           - 1; // Sub 1 since array starts with size 16 so for eg. x = 15 this returns 1(but index is 0)
}

ThreadArena::ThreadArena(){
    std::fill(firstFreePage.begin(), firstFreePage.end(), nullptr);
    std::fill(pools.begin(), pools.end(), nullptr);
    heapVersion = 0;
    localBytesAllocated = 0;
}
ThreadArena::~ThreadArena(){

}

[[gnu::hot]] void *ThreadArena::alloc(const size_t size) {
    // Only update gc after LOCAL_BYTE_CACHE bytes have been allocated,
    // removes pressure from atomic add instruction in checkHeapSize
    localBytesAllocated += size;
    if(localBytesAllocated >= LOCAL_BYTE_CACHE){
        // TODO: should this also check if the gc pointer isn't null? seems kinda dangerous
        gc->checkHeapSize(localBytesAllocated);
        localBytesAllocated = 0;
    }
    byte *block = nullptr;
    int idx = szToIdx(size);
    if (idx == -1) {
        block = static_cast<byte *>(rpmalloc(size));
        reinterpret_cast<object::Obj *>(block)->GCInfo[0] = +GCBlockColor::BLACK;
        reinterpret_cast<object::Obj *>(block)->GCInfo[1] = 2;
        tempLargeObjectStorage.push_back(reinterpret_cast<object::Obj *>(block));
    } else {
        block = reinterpret_cast<byte*>(fastAlloc(idx));
        object::Obj *obj = reinterpret_cast<object::Obj *>(block);
        // Lazy sweeping, some "free" blocks are dead objects that have not been destructed properly
        runObjDestructor(obj);
        obj->GCInfo[0] = +GCBlockColor::BLACK;
        obj->GCInfo[1] = 1;
    }
    return block;
}

PageData* ThreadArena::getMemoryPool(size_t idx){
    return pools[idx];
}
PageData* ThreadArena::getFirstFreePage(size_t idx){
    return firstFreePage[idx];
}
vector<object::Obj*>& ThreadArena::getTempStorage(){
    return tempLargeObjectStorage;
}

[[gnu::hot]] static void* pageTryAlloc(PageData* page){
    // Optimize for extreme cases
    // Having a full page is rare, and even if we hit a string of full pages, marking this branch as unlikely is still better overall
    if(page->numAllocBlocks == page->numBlocks) [[unlikely]]{
        sweepPage(*page);
        return nullptr;
    }else if(page->numAllocBlocks == 0 && page->head != page->numBlocks){
        return page->basePtr + (page->head++)*page->blockSize;
    }
    int8_t* obj = reinterpret_cast<int8_t *>(page->basePtr + page->head * page->blockSize);
    // Go until you find a free block, reverting marking bytes in the process
    // This works because page->head never moves backwards
    while(page->head < page->numBlocks && *obj == +GCBlockColor::WHITE){
        *obj = +GCBlockColor::BLACK;
        page->head++;
        page->numAllocBlocks--;
        obj += page->blockSize;
    }
    // If the loop exited because every block was taken, bail out
    if(page->head == page->numBlocks) return nullptr;
    page->head++;
    return reinterpret_cast<char *>(obj);
}

[[gnu::hot]] void* ThreadArena::fastAlloc(size_t poolIdx){
    void *ptr = nullptr;
    // This only happens at the start of a thread or when literally all the objects of a single size class get swept
    if(!firstFreePage[poolIdx]) [[unlikely]] {
        pools[poolIdx] = gc->pageManager.allocatePage(poolIdx);
        firstFreePage[poolIdx] = pools[poolIdx];
    }
    while (!(ptr = pageTryAlloc(firstFreePage[poolIdx]))) {
        if (!firstFreePage[poolIdx]->next){
            firstFreePage[poolIdx]->next = gc->pageManager.allocatePage(poolIdx);
        }
        firstFreePage[poolIdx] = firstFreePage[poolIdx]->next;
    }
    return ptr;
}
// If the heap version of GC changed then there is a chance some pages are no longer available to this arena
void ThreadArena::updateMemoryPools(uint32_t gcHeapVer){
    // Always reset roving pointer
    for(int i = 0; i < MP_CNT; i++) firstFreePage[i] = pools[i];
    // GC finds out how much live memory it has during mark phase, reset localBytesAllocated to avoid overreporting
    localBytesAllocated = 0;
    if(heapVersion == gcHeapVer) return;
    heapVersion = gcHeapVer;
    for(int i = 0; i < MP_CNT; i++){
        // First adjust tail
        while(pools[i] && pools[i]->numAllocBlocks == 0) pools[i] = pools[i]->next;
        // If there are pages left set the "head" of the list to the first page, head will advance over time
        firstFreePage[i] = pools[i];
        if(pools[i] == nullptr) continue;
        PageData* page = pools[i];
        while(page){
            PageData* pn = page->next;
            while(pn && pn->numAllocBlocks == 0){
                pn = pn->next;
            }
            page->next = pn;
            page = pn;
        }
    }
}
// Called before starting a GC collection, makes sure that the mark flag for all blocks has been reset
void memory::sweepPage(PageData& page){
    int8_t* obj = reinterpret_cast<int8_t *>(page.basePtr + page.head*page.blockSize);
    int8_t* end = reinterpret_cast<int8_t *>(page.basePtr + page.numBlocks*page.blockSize);
    while(obj != end){
        *obj = +GCBlockColor::BLACK;
        obj += page.blockSize;
    }
    page.head = 0;
    page.numAllocBlocks = 0;
}