#include <atomic>
#include <cassert>

#include "ref-slot.h"
#include "customization.h"
#include "allocator/reloc-hdrs.h"

using namespace gc::detail;

namespace {
    // Constants compiled into the program live outside the heap, in_heap does an address check
    [[gnu::always_inline]] gc::managed* forward(gc::managed* obj) {
        if (!heap_layout::in_heap(obj)) return obj;
        return (gc::managed*)reloc::forward((uint8_t*)obj);
    }
    // CAS loses only to another healer writing the same word or to a store of a newer value
    [[gnu::always_inline]] size_t install(size_t* slot, size_t seen, size_t fresh) {
        std::atomic_ref { *slot }.compare_exchange_strong(seen, fresh | gc::good_color(), std::memory_order_relaxed);
        return fresh;
    }
}

size_t gc::heal_ptr(size_t* slot, size_t seen) {
    auto ptr = (managed*)(seen & ~config::color_bit);
    return install(slot, seen, ptr ? (size_t)forward(ptr) : 0);
}
size_t gc::heal_word(size_t* slot, size_t seen) {
    auto ptr = to_accurate_ptr(seen);
    assert(ptr && "only references carry a color");
    return install(slot, seen, ptr_to_word(forward(ptr)));
}
