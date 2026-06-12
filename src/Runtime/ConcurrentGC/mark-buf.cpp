#include "mark-buf.h"
#include "gc-config.h"
#include "../../Includes/rpmalloc/rpmalloc.h"


using namespace gc::detail;

void mark_buf_manager::push_empty(mark_buf* buf) {
    if (_empty_cnt.fetch_add(1, std::memory_order_relaxed) < config::empty_mark_bufs_limit) {
        return _empty.lfpush(buf);
    }
    rpfree(buf);
}

mark_buf *mark_buf_manager::pop_empty() {
    if (auto buf = _empty.lfpop()) {
        --_empty_cnt;
        return buf;
    }
    return new(rpmalloc(sizeof(mark_buf))) mark_buf();
}
