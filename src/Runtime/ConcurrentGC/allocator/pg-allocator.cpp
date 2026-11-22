#include <new>

#include "pg-allocator.h"

using namespace gc;
using namespace gc::detail;

pg_meta* pg_allocator::alloc_pg(size_t block_sz, size_t num_pgs, bool aligned) {
    auto* page = aligned ? _spans.alloc_aligned(num_pgs, num_pgs) : _spans.alloc(num_pgs);
    if (!page) [[unlikely]] return nullptr;

    auto pg_off = heap_layout::page_idx(page);
    if (!_tables.ensure(pg_off, num_pgs)) [[unlikely]] {
        _spans.begin_free().add(page, num_pgs);
        return nullptr;
    }

    // When allocating new pages both halves come back zeroed
    auto blocks = config::blocks_in_pg(config::sz_to_class(block_sz));
    auto alloc = _bits.alloc_bits(blocks);
    auto mark = _bits.mark_bits(blocks);
    if (alloc.empty() || mark.empty()) [[unlikely]] {
        _spans.begin_free().add(page, num_pgs);
        return nullptr;
    }

    auto head = new(meta_base() + pg_off) pg_meta(block_sz, alloc.data(), mark.data());
    for (std::size_t i = 1; i < num_pgs; ++i)
        pg_meta::emplace_continuation(head + i, int32_t(i), num_pgs);

    head->set_active(true);
    return head;
}

void pg_allocator::free_pgs(pg_meta* start) {
    auto b = _spans.begin_free();
    while (start) {
        auto* head = start;
        start = start->next();
        b.add(head->get_data(), head->num_pages());
    }
}
