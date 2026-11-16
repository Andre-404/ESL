#pragma once
#include <array>

#include "tstack.h"
#include "managed.h"

namespace gc::detail {
    class mark_buf : public tnode<mark_buf> {
        size_t _cnt;
        std::array<managed*, 128> _data;
    public:
        mark_buf() : _cnt(0), _data(){};

        bool push(managed* obj) {
            _data[_cnt++] = obj;
            return _cnt == _data.size();
        }
        managed* pop() {
            if (_cnt == 0) return nullptr;
            return _data[--_cnt];
        }

        bool full() const {
            return _cnt == _data.size();
        }
        bool empty() const {
            return _cnt == 0;
        }
    };

    class mark_buf_manager {
        tstack<mark_buf> _full;
        tstack<mark_buf> _empty;
        std::atomic<size_t> _empty_cnt;
    public:
        mark_buf_manager() : _empty_cnt(0) {}

        void push_full(mark_buf* buf) { _full.lfpush(buf); }
        mark_buf* pop_full() { return _full.lfpop(); }

        void push_empty(mark_buf*);
        mark_buf* pop_empty();

        // Empty bufs are freed after stw to prevent use after free in tstack
        void remove_empty();

        // Used for tests
        size_t pooled() const { return _empty_cnt.load(std::memory_order_relaxed); }
    };
}