#include "memoryPool.h"

using namespace memory;

PageData::PageData(void *basePtr, uint64_t size) : basePtr(basePtr), size(size) {}
PageData::PageData(){
    basePtr = nullptr;
    size = 0;
}

MemoryPool::MemoryPool(uint64_t pageSize, uint64_t blockSize) : pageSize(pageSize), blockSize(blockSize) {
    head = nullptr;
}

MemoryPool::MemoryPool() {
    head = nullptr;
    pageSize = 0;
    blockSize = 0;
}

void* MemoryPool::alloc() {
    if(!head){
        allocNewPage();
    }
    void* tmp = head;
    head = head->next;
    char* ptr = reinterpret_cast<char*>(tmp) + sizeof(BlockHeader);
    return reinterpret_cast<void*>(ptr);
}
void MemoryPool::free(void* ptr){
    char* p = reinterpret_cast<char*>(ptr);
    p -= sizeof(BlockHeader);
    BlockHeader* block = reinterpret_cast<BlockHeader*>(p);
    block->next = head;
    head = block;
}

void MemoryPool::allocNewPage(){
    void* page = malloc(pageSize);
    pages.emplace_back(page, pageSize);
    char* tmp = reinterpret_cast<char*>(page);
    char* end = tmp + pageSize;
    while(tmp < end){
        BlockHeader* header = reinterpret_cast<BlockHeader*>(tmp);
        if(end - tmp < 2*(sizeof(BlockHeader) + blockSize)){
            header->next = head;
            break;
        }
        tmp += sizeof(BlockHeader) + blockSize;
        header->next = reinterpret_cast<BlockHeader*>(tmp);
    }
    head = reinterpret_cast<BlockHeader*>(page);
}

void MemoryPool::freePage(int pageIdx) {
    PageData& data = pages[pageIdx];
    BlockHeader* start = reinterpret_cast<BlockHeader*>(data.basePtr);
    BlockHeader* end = reinterpret_cast<BlockHeader*>(reinterpret_cast<char*>(start) + data.size);
    while(head > start && head < end){
        head = head->next;
    }
    BlockHeader* prev = head;
    BlockHeader* cur = head->next;
    while(cur){
        if(cur > start && cur < end){
            prev->next = cur->next;
            cur = cur->next;
        }else{
            prev = cur;
            cur = cur->next;
        }
    }
    free(data.basePtr);
    pages.erase(pages.begin() + pageIdx);
}
