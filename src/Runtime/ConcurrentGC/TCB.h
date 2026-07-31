#pragma once

#include <span>

#include "mark-buf.h"
#include "platform-specific.h"
#include "arena.h"

namespace gc {
    class tcb_handle {
    protected:
        size_t* _start_args;
        size_t _args_cnt;
    public:
        tcb_handle(size_t* start_args, uint8_t args_cnt) : _start_args(start_args), _args_cnt(args_cnt) {}

        std::pair<size_t*, uint8_t> take_start_args() {
            auto tmp = std::pair { _start_args, _args_cnt };
            _start_args = nullptr;
            _args_cnt = 0;
            return tmp;
        }
    };
    namespace detail {
        class thd_mark_info {
            mark_buf* _wbbuf;
            size_t* _stack_start;
            size_t* _stack_end;
            std::array<size_t, platform::regs_to_store()> _regs;
        public:
            thd_mark_info() : _wbbuf(nullptr), _stack_start(nullptr), _stack_end(nullptr), _regs() {}

            void track_stack(size_t start) { _stack_start = (size_t*)start; }
            void untrack_stack() { _stack_start = nullptr; }
            void capture_ctx() {
                _stack_end = (size_t*)platform::read_stack();
                platform::store_regs(_regs.data());
            }
            std::pair<std::span<size_t>, std::span<size_t>> get_ctx() {
                if (!_stack_start) return { {}, {} };
                return { { _stack_end, (size_t)(_stack_start - _stack_end) }, { (size_t*)_regs.data(), _regs.size() } };
            }

            void set_wbbuf(mark_buf* wbbuf) { _wbbuf = wbbuf; }
            mark_buf* get_wbbuf() const { return _wbbuf; }
        };

        // TODO: maybe implement an intrusive dll to store tcbs in registry
        enum class thd_state : uint8_t { running = 0, has_pending = 1, blocked = 2, handshaking = 3, need_start = 4, dead = 5 };
        class tcb : public tcb_handle {
            std::atomic<thd_state> _thd_state;
            thd_mark_info _mark_info;
            uint8_t _opcode;
            arena _arena;
        public:
            tcb(size_t* start_args, uint8_t args_cnt) : tcb_handle(start_args, args_cnt), _thd_state(thd_state::running), _opcode(0) {}

            thd_mark_info& get_mark_info() { return _mark_info; }
            arena& get_arena() { return _arena; }

            // Always read AFTER and acq load, and set before a rel store on thd_state
            void set_opcode(uint8_t opcode) { _opcode = opcode; }
            // Opcode == 0 is reserved
            uint8_t get_opcode() const { return _opcode; }

            thd_state load_state() const { return _thd_state.load(std::memory_order_acquire); }
            void wait_transition(thd_state old) const { _thd_state.wait(old); }
            void transition(thd_state new_state) {
                _thd_state.store(new_state, std::memory_order_release);
                _thd_state.notify_one();
            }
            // acq because we are reading the old state, rel because we are usually publishing opcode before this
            bool try_transition(thd_state expected, thd_state desired) {
                return _thd_state.compare_exchange_strong(expected, desired, std::memory_order_acq_rel);
            }

            std::pair<size_t*, uint8_t> peek_start_args() {
                return { _start_args, _args_cnt };
            }
        };
    }



}