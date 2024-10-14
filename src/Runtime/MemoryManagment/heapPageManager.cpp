#include "heapPageManager.h"
#include "../Objects/objects.h"
#include <algorithm>
#ifdef _WIN32
#include <memoryapi.h>
#else
#include <sys/mman.h>
#endif

using namespace memory;

HeapPageManager::HeapPageManager(){
    for(int i = 0; i < MP_CNT; i++){
        graveyard[i] = nullptr;
    }
}

PageData* HeapPageManager::allocatePage(uint32_t sizeClassIdx){
    // Need mutex because data structures get changes,
    // should be too bad since pages are allocated much more rarely then objects
    std::scoped_lock<std::mutex> lk(allocMtx);
    // First check through the graveyard to see if there is a page here, this is unlikely to happen though
    if(graveyard[sizeClassIdx]) [[unlikely]]{
        PageData* page = graveyard[sizeClassIdx];
        graveyard[sizeClassIdx] = graveyard[sizeClassIdx]->next;
        page->next = nullptr;
        return page;
    }
    if(empty.size() > 0){
        PageData* page = empty.back();
        empty.pop_back();
        page->next = nullptr;
        // Empty pages have not been swept yet and the request might be for a different block size page
        // This ensures every object on "empty" is destructed properly
        cleanUpPage(page, sizeClassIdx);
        inUse.push_back(page);
        return page;
    }
#ifdef _WIN32
    void* page = VirtualAlloc(nullptr, PAGE_SIZE, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
    // posix_memalign can return non zeroed memory so we have to zero it first
        void *page;
        posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
        memset(page, 0, PAGE_SIZE);
#endif
    // Page data is stored as a header in the page
    PageData* pageData = static_cast<PageData *>(page);
    pageData->basePtr = (char*)page + sizeof(PageData);
    pageData->blockSize = mpBlockSizes[sizeClassIdx];
    pageData->numAllocBlocks = 0;
    pageData->head = 0;
    pageData->next = nullptr;
    pageData->numBlocks = (PAGE_SIZE - sizeof(PageData)) / pageData->blockSize;
    inUse.push_back(pageData);
    return pageData;
}

// Returns page that this ptr belongs to or null
PageData* HeapPageManager::getPageFromPtr(char* ptr){
    // Usually lower bound uses '<' operator and can return equal element, here we use '<=' operator to save us an if check later
    auto it = std::lower_bound(inUse.begin(), inUse.end(), ptr, [](PageData* page, char* ptr){
        return page->basePtr <= ptr;
    });
    if(it == inUse.begin() || it == inUse.end()) return nullptr;
    else return *(--it);
}
uint32_t HeapPageManager::updateEmptyBuffer(){
    int j = 0;
    int nEmpty = 0;
    // Overwrites empty pages in the vector and stores it in a list
    for(int i = 0; i < inUse.size(); i++){
        if(inUse[i]->numAllocBlocks == 0){
            empty.push_back(inUse[i]);
            nEmpty++;
        }else{
            inUse[j] = inUse[i];
            j++;
        }
    }
    inUse.resize(j);
    return nEmpty;
}

void HeapPageManager::prepForCollection(){
    // Sort pages by address to make searching for valid pointers easier
    std::sort(inUse.begin(), inUse.end());
    std::for_each(inUse.begin(), inUse.end(), [](PageData* page){ page->numAllocBlocks = 0;});
}

void HeapPageManager::cleanUpPage(PageData* page, uint32_t newSizeClass){
    // If block sizes are the same no need to destruct every objects now, defer it to lazy sweeping
    page->head = 0;
    if(page->blockSize == mpBlockSizes[newSizeClass]) return;
    // If block sizes dont match destruct objects and update page header
    int8_t* obj = reinterpret_cast<int8_t *>(page->basePtr);
    int8_t* end = reinterpret_cast<int8_t *>(page->basePtr + page->numBlocks*page->blockSize);
    while(obj != end){
        object::runObjDestructor(reinterpret_cast<object::Obj *>(obj));
        obj += page->blockSize;
    }
    page->blockSize = mpBlockSizes[newSizeClass];
    page->numBlocks = (PAGE_SIZE - sizeof(PageData)) / mpBlockSizes[newSizeClass];
    memset(page->basePtr, 0, PAGE_SIZE - sizeof(PageData));
}