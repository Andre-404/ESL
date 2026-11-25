#include "marker.h"


using namespace gc::detail;


void marker::flush_wbbuf(mark_buf* buf) {
    if (buf->empty()) return;
    auto to_send = _bufs.pop_empty();
    // to_send holds as many as buf, so it never fills before the last one
    while (auto val = buf->pop()) if (mark_obj(val)) (void)to_send->push(val);
    push_buf(to_send);
}

[[gnu::hot]] size_t marker::trace_n(size_t bytes)  {
    auto main = _bufs.pop_full();
    if (!main) return 0;

    auto mark = [&](managed* obj) { push_obj(main, obj); };

    size_t cnt = 0;
    while (cnt < bytes) {
        auto obj = main->pop();
        if (!obj) [[unlikely]] {
            _bufs.push_empty(main);
            main = _bufs.pop_full();
            if (!main) [[unlikely]] return cnt;
            continue;
        }
        assert(obj_traceable(obj));
        cnt += obj_size(obj);
        obj_trace(obj, mark);
    }
    push_buf(main);
    
    return cnt;
}