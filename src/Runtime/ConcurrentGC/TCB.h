#pragma once

#include <span>

#include "mark-buf.h"
#include "platform-specific.h"
#include "arena.h"

namespace gc {
    class tcb_handle {
        size_t* _start_args;
        uint8_t _args_cnt;
    public:
        tcb_handle(size_t* start_args, uint8_t args_cnt) : _start_args(start_args), _args_cnt(args_cnt) {}

        std::pair<size_t*, uint8_t> get_start_args() { return { _start_args, _args_cnt }; }
        void clear_start_args() {
            _start_args = nullptr;
            _args_cnt = 0;
        }
    };
    namespace detail {
        class thd_mark_info {
            mark_buf* _wbbuf;
            size_t* _stack_start;
            size_t* _stack_end;
            std::array<uint8_t, platform::reg_storage_sz()> _regs;
        public:
            thd_mark_info() : _wbbuf(nullptr), _stack_start(nullptr), _stack_end(nullptr), _regs() {}

            void set_stack_start(size_t start) { _stack_start = (size_t*)start; }
            void capture_ctx() {
                _stack_end = (size_t*)platform::read_stack();
                platform::store_regs(_regs.data());
            }

            std::pair<std::span<size_t>, std::span<size_t>> get_ctx() {
                if (!_stack_start) return { {}, {} };
                return { { _stack_end, (size_t)(_stack_start - _stack_end) }, { (size_t*)_regs.data(), _regs.size() / 8 } };
            }

            void set_wbbuf(mark_buf* wbbuf) { _wbbuf = wbbuf; }
            mark_buf* get_wbbuf() const { return _wbbuf; }
        };

        enum class thd_state : uint8_t { blocked = 0, at_safepoint = 1, running = 2 };
        class tcb : public tcb_handle {
            thd_mark_info _mark_info;
            std::atomic<thd_state> _thd_state;
            arena _arena;
        public:
            tcb(size_t* start_args, uint8_t args_cnt) : tcb_handle(start_args, args_cnt), _thd_state(thd_state::blocked) {}

            thd_mark_info& get_mark_info() { return _mark_info; }
            arena& get_arena() { return _arena; }

            thd_state get_state() const {
                return _thd_state.load(std::memory_order_relaxed);
            }
            void set_state(thd_state state) {
                _thd_state.store(state, std::memory_order_relaxed);
            }

        };
    }



}