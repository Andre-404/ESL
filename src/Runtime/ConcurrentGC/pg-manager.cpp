#include <utility>

#include "pg-manager.h"
#include "gc-config.h"

using namespace gc::detail;

pg_meta* pg_manager::new_pg(size_t obj_sz) {
    auto cl = config::sz_to_class(obj_sz);
    if (cl == config::large_class) [[unlikely]] {
        auto n = (obj_sz + config::page_sz - 1) / config::page_sz;
        return _allocator.alloc_pg(obj_sz, n);
    }

    if (!_partial[cl].approx_empty()) {
        auto _ = std::lock_guard { _part_mtx };
        auto pg = _partial[cl].pop();
        if (pg) return pg;
    }

    return _allocator.alloc_pg(config::sz_classes[cl], config::small_obj_pgs);
}

static size_t compute_idx(pg_meta* pg, pg_meta* base) {
    auto offset = pg - base;
    return offset * gc::config::page_sz / gc::config::free_batch_sz;
}

void pg_manager::free_pgs() {
    auto wl = _pending_free.lf_reset_head(nullptr);
    size_t lo = ~size_t(0), hi = 0;
    while (wl) {
        auto pg = wl;
        wl = wl->next();
        pg->mark_inactive();
        auto idx = compute_idx(pg, _allocator.meta_base());
        lo = std::min(lo, idx);
        hi = std::max(hi, idx);
        pg->link(_buckets[idx]);
        _buckets[idx] = pg;
    }

    for (auto i = lo; i <= hi; i++) {
        auto pg = std::exchange(_buckets[i], nullptr);
        if (pg) _allocator.free_pgs(pg);
    }
}
