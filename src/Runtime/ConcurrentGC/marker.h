#pragma once
#include <ranges>

#include "mark-buf.h"
#include "customization.h"
#include "pg-meta.h"
#include "ref-slot.h"

namespace gc::detail {
    // Mutators may be storing to root and the marker reads the object behind it, hence acquire
    // Heals a bad color, returns the object or null
    inline managed* read_root(size_t& root) {
        auto word = load_raw(root, std::memory_order_acquire);
        auto obj = to_accurate_ptr(word);
        if (obj && ((word ^ good_color()) & config::color_bit)) obj = to_accurate_ptr(heal_word(&root, word));
        return obj;
    }
    // This stores ANY word to the root
    // Doesn't matter if its a pointer or not BUT pointers get color tagged
    inline void store_root(size_t& root, size_t word) {
        store_raw(root, to_accurate_ptr(word) ? word | good_color() : word);
    }

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
            return __builtin_unpredictable(won && obj_traceable(obj));
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

        // roots are references to words so that marker can heal
        void scan_roots(auto&& roots) {
            auto buf = _bufs.pop_empty();
            for (size_t& root : roots)
                if (auto obj = read_root(root)) push_obj(buf, obj);
            push_buf(buf);
        }
        void scan_globals(std::span<size_t*> globals) {
            scan_roots(globals | std::views::transform([](size_t* g) -> size_t& { return *g; }));
        }

        // Conservative, every word is 8 byte aligned. With pin, an object found here is pinned
        // whether or not it was already marked. Also handles one-past-end pointers
        template<typename F>
        void scan_words(std::initializer_list<std::span<size_t>> parts, bool pin, F&& get_base) {
            auto buf = _bufs.pop_empty();
            for (auto words : parts)
                for (auto word : words) {
                    auto p = to_possible_ptr(word);
                    auto obj = get_base(p);
                    auto prev = p ? get_base(p - 1) : nullptr;

                    if (obj) push_obj(buf, obj, pin);
                    // prev + size to help filter out atleast some false reports
                    if (prev && prev != obj && (uint8_t*)prev + obj_size(prev) == p)
                        push_obj(buf, prev, pin);
                }
            push_buf(buf);
        }

        [[gnu::hot]] size_t trace_n(size_t bytes);
    };
}
