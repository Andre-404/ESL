#include "mark-buf.h"
#include "gc-config.h"
#include "../../Includes/rpmalloc/rpmalloc.h"


using namespace gc::detail;

// TODO(critical): potential use after free:
/*
    thdA: pop empty   thdB pop empty
    thdA: success     thdB: pop empty
    thdA: push empty  thdB: pop empty
    thdA: rpfree      thdB: deref what thdA just freed
*/
void mark_buf_manager::push_empty(mark_buf* buf) {
    if (_empty_cnt.fetch_add(1, std::memory_order_relaxed) < config::empty_mark_bufs_limit) {
        return _empty.lfpush(buf);
    }
    _empty_cnt.fetch_sub(1, std::memory_order_relaxed);
    rpfree(buf);
}

mark_buf *mark_buf_manager::pop_empty() {
    if (auto buf = _empty.lfpop()) {
        --_empty_cnt;
        return buf;
    }
    return new(rpmalloc(sizeof(mark_buf))) mark_buf();
}
