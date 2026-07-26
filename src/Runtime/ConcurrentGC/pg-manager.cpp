#include "../../Includes/rpmalloc/rpmalloc.h"
#include "pg-manager.h"

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

    pg = _allocator.alloc_pg(config::sz_classes[sz_class], 1);
    if (pg) _active.add((uintptr_t)pg, pg);
    return pg;
}

pg_meta *pg_manager::get_big_pg(size_t obj_sz) {
    auto pg = _allocator.alloc_pg(obj_sz, large_pg_num(obj_sz));

    if (pg) {
        for (size_t i = 0; i < pg->num_pages(); i++) {
            auto ptr = (uintptr_t)((uint8_t*)pg + i * config::page_sz);
            _active.add(ptr, pg);
        }
    }
    return pg;
}

// TODO: move removing from active pgs to free_pgs -> it always happens before next mark phase
void pg_manager::schedule_free(pg_meta* pg)  {
    if (!pg) return;
    for (size_t i = 0; i < pg->num_pages(); i++) {
        auto offset = (pg_meta*)((uint8_t*)pg + i * config::page_sz);
        _active.remove(offset);
    }
    _pending_free.lfpush(pg);
    
}

void pg_manager::free_pgs() {
    auto worklist = _pending_free.lf_reset_head(nullptr);
    _allocator.free_pgs(worklist);
}
