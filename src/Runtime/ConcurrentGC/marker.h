#pragma once
#include "mark-buf.h"
#include "customization.h"
#include "pg-meta.h"
#include "TCB.h"

namespace gc::detail {
    class marker {
        mark_buf_manager _bufs;

        // True when obj won its mark and has slots to trace
        [[gnu::always_inline, gnu::hot]] bool mark_obj(managed* obj, bool force_pin = false) {
            // Constants compiled into the program live outside the heap
            if (!heap_layout::in_heap(obj)) [[unlikely]] return false;

            auto pg = pg_meta::head_from_ptr(obj);
            auto won = pg->record_mark(obj);
            // Only a won mark reads the object
            if (force_pin || (won && obj_is_pinned(obj))) [[unlikely]] pg->pin();
            return !__builtin_unpredictable(!won || !obj_traceable(obj));
        }
        // Swaps buf for an empty one once it fills
        [[gnu::always_inline, gnu::hot]] void push_obj(mark_buf*& buf, managed* obj, bool force_pin = false) {
            auto cur = buf;
            if (!mark_obj(obj, force_pin)) return;
            if (cur->push(obj)) [[unlikely]] {
                _bufs.push_full(cur);
                buf = _bufs.pop_empty();
            }
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

        // Conservative, every word is 8 byte aligned. With pin, an object found here is pinned
        // whether or not it was already marked
        template<typename F>
        void scan_words(std::initializer_list<std::span<size_t>> parts, bool pin, F get_base) {
            auto buf = _bufs.pop_empty();
            for (auto words : parts)
                for (auto word : words)
                    if (auto obj = get_base(to_possible_ptr(word))) push_obj(buf, obj, pin);
            push_buf(buf);
        }

        template<typename F>
        void scan_stack(thd_mark_info& info, bool pin, F get_base) {
            auto [stack, regs] = info.get_ctx();
            scan_words({ stack, regs }, pin, get_base);
        }

        [[gnu::hot]] size_t trace_n(size_t bytes);
    };
}
