#pragma once
#include <vector>

#include "mark-buf.h"
#include "customization.h"
#include "pg-meta.h"
#include "TCB.h"

namespace gc::detail {
    class marker {
        mark_buf_manager _bufs;

        // Must stay in the header: scan_stack is a template instantiated in other TUs and
        // always_inline needs the body available at every call site.
        [[gnu::always_inline, nodiscard]] bool push_obj(mark_buf* buf, managed* obj) {
            auto state = obj->state();
            auto is_traceable = obj_traceable(obj);
            if (state == move_state::unmanaged) [[unlikely]] return false;

            auto pg = pg_from_obj(obj);
            auto won = pg->record_mark(obj, state != move_state::none);
            if (!won || !is_traceable) return false;

            return buf->push(obj);
        }
        [[nodiscard]] mark_buf* replace_buf(mark_buf* buf) {
            if (!buf->empty()) _bufs.push_full(buf);
            else return buf;

            return _bufs.pop_empty();
        }
    public:
        marker() : _bufs() {}

        [[nodiscard]] mark_buf* get_buf() {
            return _bufs.pop_empty();
        }

        void push_buf(mark_buf* buf) {
            if (!buf->empty()) _bufs.push_full(buf);
            else _bufs.push_empty(buf);
        }

        void flush_wbbuf(mark_buf* buf);

        void scan_globals(std::span<size_t*> globals);

        template<typename F>
        void scan_stack(thd_mark_info& info, bool pin, F get_base) {
            auto [stack, regs] = info.get_ctx();
            auto buf = _bufs.pop_empty();
            auto mark = [&](managed* obj) {
                // Regardless of whether this object was already marked or not, if it's on the stack or in registers in needs to be pinned
                if (pin && obj->state() == move_state::none) obj->set_state(move_state::temp_pinned);
                if (push_obj(buf, obj)) buf = replace_buf(buf);
            };
            // Assumes stack grows downwards, also assumes every value on the stack is 8byte aligned
            for (auto word : stack)
                if (auto base_ptr = get_base(to_possible_ptr(word))) mark(base_ptr);

            for (const auto word : regs)
                if (auto base_ptr = get_base(to_possible_ptr(word))) mark(base_ptr);
            
            push_buf(buf);
        }

        size_t trace_n(size_t bytes);
    };
}
