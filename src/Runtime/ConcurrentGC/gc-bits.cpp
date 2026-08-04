#include <algorithm>
#include <cstring>
#include <new>

#include "gc-bits.h"
#include "os-memory.h"

using namespace gc;
using namespace gc::detail;

namespace {
    constexpr std::size_t round_up(std::size_t n, std::size_t to) {
        return (n + to - 1) / to * to;
    }
}

gc_bits::gc_bits() : _base(nullptr), _live(0) {
    _base = (uint8_t*)os::reserve(config::bits_region_sz, config::bits_commit_sz);
    if (!_base) throw std::bad_alloc{};

    static_assert(
        config::bits_arena_sz % config::bits_commit_sz == 0, "a half must be a whole number of commit granules"
    );
}

gc_bits::~gc_bits() {
    os::release(_base, config::bits_region_sz);
}

bool gc_bits::ensure_committed(half& h, uint8_t* base, std::size_t end) {
    auto lk = std::lock_guard { _mtx };
    // Another thread may have covered this range while we waited for the lock.
    auto from = h.committed.load(std::memory_order_relaxed);
    if (end <= from) return true;

    auto to = round_up(end, config::bits_commit_sz);
    if (!os::commit(base + from, to - from)) return false;

    // Release, paired with the acquire in bump(): a thread that skips the lock because it
    // sees this value must also see the range as backed.
    h.committed.store(to, std::memory_order_release);
    return true;
}

uint8_t* gc_bits::bump(half& h, uint8_t* base, std::size_t bits) {
    if (bits == 0) return nullptr;
    // Bitmaps are read and written a word at a time through atomic_ref<uint64_t>,
    // so every slot has to land 8-aligned
    auto n = (bits + 63) / 64 * sizeof(uint64_t);

    auto off = h.cursor.fetch_add(n, std::memory_order_relaxed);
    // TODO: this mathematically shouldn't be possible? can we drop the check
    if (off + n > config::bits_arena_sz) [[unlikely]] return nullptr;

    auto commit_fail = off + n > h.committed.load(std::memory_order_acquire)
                           && !ensure_committed(h, base, off + n);
    
    if (commit_fail) [[unlikely]] return nullptr;

    auto p = base + off;
    memset(p, 0, n);
    return p;
}

void gc_bits::flip() {
    auto live = _live.load(std::memory_order_relaxed);
    // Reset before the swap: until _live moves, nothing will claim out of the half whose
    // cursor is being rewound.
    _halves[live].cursor.store(0, std::memory_order_relaxed);
    _live.store(live ^ 1, std::memory_order_release);
}

std::size_t gc_bits::used() const {
    auto at = [&](int i) {
        return std::min(_halves[i].cursor.load(std::memory_order_relaxed), config::bits_arena_sz);
    };
    return std::max(at(0), at(1));
}
