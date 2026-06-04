#pragma once
#include <vector>

#include "mark-buf.h"
#include "customization.h"
#include "TCB.h"

namespace gc::detail {
    class marker {
        mark_buf_manager _bufs;
        std::vector<size_t*> _roots;

        [[gnu::always_inline]] void push_obj(mark_buf* buf, managed* obj);
        [[nodiscard]] mark_buf* replace_buf(mark_buf* buf) {
            if (!buf->empty()) _bufs.push_full(buf);
            else return buf;

            return _bufs.pop_empty();
        }
    public:
        marker() {}

        [[nodiscard]] mark_buf* get_buf() {
            return _bufs.pop_empty();
        }

        void push_buf(mark_buf* buf) {
            if (!buf->empty()) _bufs.push_full(buf);
            else _bufs.push_empty(buf);
        }

        void register_root(size_t* root) { _roots.push_back(root); }

        void scan_globals();

        template<typename F>
        void scan_stack(thd_mark_info* info, bool pin, F get_base) {
            auto [stack, regs] = info->get_ctx();
            auto buf = _bufs.pop_empty();
            auto mark = [&](managed* obj) {
                if (buf->full()) buf = replace_buf(buf);
                if (pin && obj->get_move_state() == obj_move_state::none) obj->set_move_state(obj_move_state::temp_pinned);
                push_obj(buf, obj);
            };
            // Assumes stack grows downwards, also assumes every value on the stack is 8byte aligned
            for (auto word : stack)
                if (auto base_ptr = get_base(word_to_ptr(word))) mark(base_ptr);

            for (const auto word : regs)
                if (auto base_ptr = get_base(word_to_ptr(word))) mark(base_ptr);

            buf->empty() ? _bufs.push_empty(buf) : _bufs.push_full(buf);
        }

        size_t trace_n(size_t bytes);
    };
}
