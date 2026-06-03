#include "../../Includes/rpmalloc/rpmalloc.h"
#include "pg-manager.h"
#include "pruner.h"

using namespace gc::detail;

ts_cache::l2_cache& ts_cache::get_or_create_l2(size_t idx)  {
    auto ref = std::atomic_ref { _top_level.at(idx) };
    auto existing = ref.load(std::memory_order_acquire);
    if (existing) return *existing;

    auto new_cache = new (rpmalloc(sizeof(l2_cache))) l2_cache();

    l2_cache* expected = nullptr;
    if (ref.compare_exchange_strong(expected, new_cache, std::memory_order_release, std::memory_order_acquire)) {
        return *new_cache;
    }
    // Lost the race, another thread already created l2 table for this memory region
    rpfree(new_cache);
    return *expected;
}

pg_meta *pg_manager::get_new_pg(uint8_t sz_class)  {
    auto pg = _partial[sz_class].lfpop();
    if (pg) return pg;

    pg = _empty.lfpop();
    if (pg) {
        --_empty_cnt;
        if (!pg->is_pod()) _cleanup(pg);
        pg = new (pg) pg_meta(config::sz_classes[sz_class]);
        _active.add(pg);
        pg->recycle();
        return pg;
    }
    pg = _allocator.alloc_pg(config::sz_classes[sz_class]);
    _active.add(pg);
    return pg;
}

void pg_manager::dealloc_pgs(pg_meta *head, size_t empty_limit)  {
    if (!head) return;
    auto tail = head;
    auto deactivate = [&](pg_meta* pg) {
        _active.remove(pg);
        if (!pg->is_pod()) _cleanup(pg);
    };

    for (;; tail = tail->next()) {
        deactivate(tail);
        ++_empty_cnt;
        if (!tail->next() || _empty_cnt >= empty_limit) break;
    }
    auto to_del = tail->next();
    _empty.lfpush_range(head, tail);

    for (auto cur = to_del; cur;) {
        auto* next = cur->next();
        deactivate(cur);
        _allocator.dealloc_pg(cur);
        cur = next;
    }
}
