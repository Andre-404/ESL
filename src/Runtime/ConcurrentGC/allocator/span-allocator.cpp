#include "../gc-config.h"
#include "radix-tree.h"
#include <atomic>
#include <new>

#include <algorithm>

#include "os-memory.h"
#include "span-allocator.h"

using namespace gc;
using namespace gc::detail;

namespace {
    constexpr std::size_t alloc_words = heap_geometry::words;
    constexpr std::size_t committed_words = heap_geometry::total_pages / config::commit_granule / 64;
    constexpr std::size_t meta_bytes = (alloc_words + committed_words) * sizeof(uint64_t)
                                     + heap_geometry::total_entries * sizeof(summary);

    constexpr auto G = config::commit_granule;
    constexpr auto granule_bytes = G * config::page_sz;
    std::size_t granule_of(std::size_t page) { return page / G; }
    bool granule_all_free(std::size_t g, const radix_tree& tree) {
        return !tree.bits().any_set(g * G, G);
    }
    std::size_t granules_unalloced(std::size_t start, std::size_t n, const radix_tree& tree) {
        auto end = start + n;
        std::size_t d = 0;
        for (auto g = granule_of(start); g <= granule_of(end - 1); ++g) {
            auto lo = std::max(start, g * G);
            auto hi = std::min(end, (g + 1) * G);
            if (hi - lo == G || granule_all_free(g, tree)) ++d;
        }
        return d;
    }
}

span_allocator::span_allocator(uint8_t* base) :
    _base(base), _meta(nullptr), _in_use(0), _resident(0), _scavenge_idx(0) {
    _meta = os::map_rw(meta_bytes);
    if (!_meta) throw std::bad_alloc{};

    auto cursor = (uint8_t*)_meta;

    auto words = os::carve<uint64_t>(cursor, alloc_words);
    auto sums = os::carve<summary>(cursor, heap_geometry::total_entries);
    _tree = radix_tree { sums, words };

    _committed = bitmap { os::carve<uint64_t>(cursor, committed_words) };

    assert(cursor == (uint8_t*)_meta + meta_bytes && "meta_bytes disagrees with the layout");
}

span_allocator::~span_allocator() {
    os::release(_meta, meta_bytes);
}

bool span_allocator::ensure_committed(std::size_t start, std::size_t n) {
    auto lo = granule_of(start);
    auto end = granule_of(start + n - 1);

    while (lo <= end) {
        if (_committed.test(lo)) { ++lo; continue; }
        // Coalesce the uncommitted granules so a large run costs one syscall, not one each
        auto hi = lo;
        while (hi <= end && !_committed.test(hi)) ++hi;

        if (!os::commit(_base + lo * granule_bytes, (hi - lo) * granule_bytes))
            return false;

        _committed.set_range(lo, hi-lo);
        _resident.fetch_add(hi - lo, std::memory_order_relaxed);

        lo = hi;
    }
    return true;
}

void* span_allocator::claim_span(std::size_t start, std::size_t npages) {
    auto need_alloc = granules_unalloced(start, npages, _tree);
    _tree.mark_alloc(start, npages);

    if (!ensure_committed(start, npages)) [[unlikely]] {
        _tree.mark_free(start, npages);
        _tree.flush_free();
        return nullptr;
    }

    auto now = _in_use.fetch_add(need_alloc, std::memory_order_relaxed) + need_alloc;
    if (now > _in_use_peak.load(std::memory_order_relaxed)) _in_use_peak.store(now, std::memory_order_relaxed);
    return _base + start * config::page_sz;
}

// TODO: right now scavenge locks the mutex even though it doesn't do any operations on the tree
// (it just reads the bits) try making it lock free(?) or defer tree freeing to scavenge as well
std::size_t span_allocator::scavenge(std::size_t max_granules) {
    auto lk = std::lock_guard { _mtx };
    
    auto can_free = [&](std::size_t g) {
        return _committed.test(g) && granule_all_free(g, _tree);
    };

    std::size_t released = 0;
    auto hi = _scavenge_idx + 1;
    while (hi > 0 && released < max_granules) {
        if (!can_free(hi - 1)) { 
            --hi;
            continue;
        }
        // lo points to first nonfreeable granule
        auto lo = hi;
        while (lo > 0 && can_free(lo - 1) && (hi - lo) < (max_granules - released)) --lo;

        auto n = hi - lo;
        if (os::decommit(_base + lo * granule_bytes, n * granule_bytes)) {
            released += n;
            _resident.fetch_sub(n, std::memory_order_relaxed);
            _committed.clear_range(lo, n);
        }
        hi = lo;
    }

    _scavenge_idx = std::min(hi, _scavenge_idx);
    return released;
}

void* span_allocator::alloc(std::size_t npages) {
    if (npages == 0) return nullptr;
    auto lk = std::lock_guard{ _mtx };
    auto s = _tree.find(npages);
    return s ? claim_span(*s, npages) : nullptr;
}

void* span_allocator::alloc_aligned(std::size_t npages, std::size_t align) {
    if (npages == 0 || npages > align || align > 64 || (align & (align - 1))) return nullptr;
    auto lk = std::lock_guard{ _mtx };
    auto s = _tree.find_aligned(npages, align);
    return s ? claim_span(*s, npages) : nullptr;
}

void span_allocator::free(void* addr, std::size_t npages) {
    auto idx = page_idx(addr);
    assert(_tree.bits().all_set(idx, npages) && "freeing pages that are not allocated");

    _tree.mark_free(idx, npages);
    _in_use.fetch_sub(granules_unalloced(idx, npages, _tree), std::memory_order_relaxed);
}
