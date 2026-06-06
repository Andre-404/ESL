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
    auto pg = _partial[sz_class].pop();
    if (pg) return pg;

    pg = _empty.lfpop();
    if (pg) {
        --_empty_cnt;
        pg = new (pg) pg_meta(config::sz_classes[sz_class]);
        _active.add((uintptr_t)pg, pg);
        pg->recycle();
        return pg;
    }

    pg = _allocator.alloc_pg(config::sz_classes[sz_class], 1);
    if (pg) _active.add((uintptr_t)pg, pg);
    return pg;
}

pg_meta *pg_manager::get_big_pg(size_t obj_sz) {
    auto pg = _allocator.alloc_pg(obj_sz, obj_sz / config::page_sz + 1);

    if (pg) {
        for (auto i = 0; i < obj_sz / config::page_sz + 1; i++) {
            auto ptr = (uintptr_t)((uint8_t*)pg + i * config::page_sz);
            _active.add(ptr, pg);
        }
    }
    return pg;
}

void pg_manager::dealloc_pgs(pg_meta *head)  {
    if (!head) return;
    auto tail = head;
    if (head->is_large_pg()) {
        while (tail->next()) {
            for (auto i = 0; i < tail->block_sz() / config::page_sz + 1; i++) {
                auto offset = (pg_meta*)((uint8_t*)tail + i * config::page_sz);
                _active.remove(offset);
            }
            tail = tail->next();
        }
        // Eagerly decommit big objs to free up memory
        _allocator.dealloc_pgs(head, tail);
        return;
    }

    for (;; tail = tail->next()) {
        _active.remove(tail);
        _empty_cnt.fetch_add(1, std::memory_order_relaxed);
        if (!tail->next() || _empty_cnt >= _empty_limit) break;
    }
    auto to_del = tail->next();
    _empty.lfpush_range(head, tail);

    if (!to_del) return;

    auto del_tail = to_del;
    while (del_tail->next()) {
        _active.remove(del_tail);
        del_tail = del_tail->next();
    }
    _allocator.dealloc_pgs(to_del, del_tail);

}
