#include "marker.h"


using namespace gc::detail;


void marker::flush_wbbuf(mark_buf* buf, bool copying) {
    if (buf->empty()) return;
    auto to_send = _bufs.pop_empty();
    while (auto val = buf->pop()) {
        auto container = buf->pop();
        assert(container && "the write buffer holds (container, value) pairs");

        auto res = push_obj(to_send, val);

        if (copying && res.movable_obj)
            pg_meta::head_from_ptr(container)->dirty_card(container);
    }
    push_buf(to_send);
}

void marker::scan_globals(std::span<size_t*> roots)  {
    auto buf = _bufs.pop_empty();
    auto mark = [&](managed* obj) {
        if (push_obj(buf, obj).buf_full) [[unlikely]] buf = replace_buf(buf);
    };
    for (auto root : roots) {
        if (auto ptr = (managed*)to_accurate_ptr(*root)) mark(ptr);
    }

    push_buf(buf);
}

[[gnu::hot]] size_t marker::trace_n(size_t bytes, bool copying)  {
    auto main = _bufs.pop_full();
    if (!main) return 0;

    auto dirtied = false;
    auto mark = [&](managed* obj) {
        auto res = push_obj(main, obj);
        if (copying && res.movable_obj) dirtied = true;
        if (res.buf_full) [[unlikely]] main = replace_buf(main);
    };

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
        dirtied = false;
        obj_trace(obj, mark);
        if (dirtied)
            pg_meta::head_from_ptr(obj)->dirty_card(obj);
    }
    push_buf(main);
    
    return cnt;
}