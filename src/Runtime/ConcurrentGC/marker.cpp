#pragma once
#include "marker.h"
#include "pg-meta.h"
#include <functional>


using namespace gc::detail;


[[gnu::always_inline]] void marker::push_obj(mark_buf *buf, managed *obj) {
    if (obj->get_move_state() == obj_move_state::unmanaged) [[unlikely]] return;
    if (buf->full()) [[unlikely]] return;

    auto pg = pg_from_obj(obj);
    auto won = pg->record_mark(obj, obj->get_move_state() != obj_move_state::none);
    if (!won || !obj_traceable(obj)) return;
    buf->push(obj);
}

void marker::scan_globals()  {
    auto buf = _bufs.pop_empty();
    auto mark = [&](managed* obj) {
        if (buf->full()) [[unlikely]] buf = replace_buf(buf);
        push_obj(buf, obj);
    };
    for (auto root : _roots) {
        if (auto ptr = word_to_ptr(*root)) mark(ptr);
    }

    buf->empty() ? _bufs.push_empty(buf) : _bufs.push_full(buf);
}

size_t marker::trace_n(size_t bytes)  {
    auto main = _bufs.pop_full();
    if (!main) return 0;

    auto side = _bufs.pop_empty();
    std::function mark = [&](managed* obj) {
        if (main->full()) [[unlikely]] {
            if (side->full()) [[unlikely]] main = replace_buf(main);
            else std::swap(side, main);
        }
        push_obj(main, obj);
    };

    size_t cnt = 0;
    while (cnt < bytes) {
        auto obj = main->pop();
        if (!obj) [[unlikely]] {
            if (!side->empty()) {
                std::swap(main, side);
            } else {
                _bufs.push_empty(main);
                main = _bufs.pop_full();
                if (!main) [[unlikely]] {
                    _bufs.push_empty(side);
                    return cnt;
                }
            }
            continue;
        }

        cnt += obj_size(obj);
        obj_trace(obj, mark);
    }
    main->empty() ? _bufs.push_empty(main) : _bufs.push_full(main);
    side->empty() ? _bufs.push_empty(side) : _bufs.push_full(side);

    return cnt;
}
