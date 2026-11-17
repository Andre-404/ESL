#pragma once
#include "mark-buf.h"
#include "customization.h"
#include "pg-meta.h"
#include "TCB.h"

namespace gc::detail {
    class marker {
        mark_buf_manager _bufs;

        // Returns true if buf is full
        [[gnu::always_inline, gnu::hot, nodiscard]] bool push_obj(mark_buf* buf, managed* obj, bool force_pin = false) {
            auto state = obj->state();
            if (state == move_state::unmanaged) [[unlikely]] return false;

            auto pg = pg_meta::head_from_ptr(obj);
            auto won = pg->record_mark(obj, force_pin || state != move_state::none);
            if (__builtin_unpredictable(!won || !obj_traceable(obj))) return false;

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
        void remove_empty() { _bufs.remove_empty(); }

        void flush_wbbuf(mark_buf* buf);

        void scan_globals(std::span<size_t*> globals);

        template<typename F>
        void scan_temp(std::span<size_t> tmp, F get_base) {
            auto buf = _bufs.pop_empty();
            auto mark = [&](managed* obj) {
                // Regardless of whether this object was already marked or not,
                // if it's on the stack or in registers in needs to be pinned
                if (push_obj(buf, obj, true)) buf = replace_buf(buf);
            };
            // Assumes stack grows downwards, also assumes every value on the stack is 8byte aligned
            for (auto word : tmp)
                if (auto base_ptr = get_base(to_possible_ptr(word))) mark(base_ptr);
            
            push_buf(buf);
        }

        template<typename F>
        void scan_stack(thd_mark_info& info, bool pin, F get_base) {
            auto [stack, regs] = info.get_ctx();
            auto buf = _bufs.pop_empty();
            auto mark = [&](managed* obj) {
                // Regardless of whether this object was already marked or not,
                // if it's on the stack or in registers in needs to be pinned
                if (push_obj(buf, obj, pin)) buf = replace_buf(buf);
            };
            // Assumes stack grows downwards, also assumes every value on the stack is 8byte aligned
            for (auto word : stack)
                if (auto base_ptr = get_base(to_possible_ptr(word))) mark(base_ptr);

            for (const auto word : regs)
                if (auto base_ptr = get_base(to_possible_ptr(word))) mark(base_ptr);
            
            push_buf(buf);
        }

        [[gnu::hot]] size_t trace_n(size_t bytes);
    };
}
