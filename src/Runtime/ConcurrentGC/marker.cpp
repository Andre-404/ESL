#pragma once
#include "marker.h"
#include "pg-meta.h"
#include <functional>


using namespace gc::detail;


[[gnu::always_inline, nodiscard]] bool marker::push_obj(mark_buf *buf, managed *obj) {
    if (obj->state() == move_state::unmanaged) [[unlikely]] return false;

    auto pg = pg_from_obj(obj);
    auto won = pg->record_mark(obj, obj->state() != move_state::none);
    if (!won || !obj_traceable(obj)) return false;
    // Don't push buf if we're full
    return buf->push(obj);
}

void marker::scan_globals(std::span<size_t*> roots)  {
    auto buf = _bufs.pop_empty();
    auto mark = [&](managed* obj) {
        if (push_obj(buf, obj)) [[unlikely]] buf = replace_buf(buf);
    };
    for (auto root : roots) {
        if (auto ptr = (managed*)to_accurate_ptr(*root)) mark(ptr);
    }

    buf->empty() ? _bufs.push_empty(buf) : _bufs.push_full(buf);
}

size_t marker::trace_n(size_t bytes)  {
    auto main = _bufs.pop_full();
    if (!main) return 0;

    auto side = _bufs.pop_empty();
    std::function mark = [&](managed* obj) {
        if (!push_obj(main, obj)) [[likely]] return;
        if (side->full()) [[unlikely]] main = replace_buf(main);
        else std::swap(side, main);
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
