#include "marker.h"


using namespace gc::detail;


void marker::flush_wbbuf(mark_buf* buf) {
    if (buf->empty()) return;
    auto to_send = _bufs.pop_empty();
    while (auto obj = buf->pop()) {
        auto res = push_obj(to_send, obj);
        assert(!(res && !buf->empty()));
    }
    push_buf(to_send);
}

void marker::scan_globals(std::span<size_t*> roots)  {
    auto buf = _bufs.pop_empty();
    auto mark = [&](managed* obj) {
        if (push_obj(buf, obj)) [[unlikely]] buf = replace_buf(buf);
    };
    for (auto root : roots) {
        if (auto ptr = (managed*)to_accurate_ptr(*root)) mark(ptr);
    }

    push_buf(buf);
}

size_t marker::trace_n(size_t bytes)  {
    auto main = _bufs.pop_full();
    if (!main) return 0;

    auto mark = [&](managed* obj) {
        if (push_obj(main, obj)) [[unlikely]]
            main = replace_buf(main);
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
        obj_trace(obj, mark);
    }
    push_buf(main);
    
    return cnt;
}