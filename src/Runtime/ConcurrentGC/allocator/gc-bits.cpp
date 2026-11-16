#include <algorithm>
#include <cstring>

#include "gc-bits.h"

using namespace gc;
using namespace gc::detail;

gc_bits::gc_bits(uint8_t* base)
    : _halves{ { base, config::bits_arena_sz }, { base + config::bits_arena_sz, config::bits_arena_sz } }, _live(0) {}

void gc_bits::clear_mark(uint64_t *watermark) {
    auto h = _live.load(std::memory_order_acquire) ^ 1;
    auto b = _halves[h].base();
    auto n = (uint8_t*)watermark - b;
    if (n <= 0) return;
    memset(b, 0, n);
}

void gc_bits::flip() {
    auto live = _live.load(std::memory_order_relaxed);
    // Reset before the swap: until _live moves, nothing will claim out of the half whose
    // cursor is being rewound.
    _halves[live].reset();
    _live.store(live ^ 1, std::memory_order_release);
}

std::size_t gc_bits::used() const {
    return std::max(_halves[0].used(), _halves[1].used());
}
