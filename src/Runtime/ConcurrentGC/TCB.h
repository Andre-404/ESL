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
            void set_ctx_unusable() { _stack_start = nullptr; }
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

        // TODO: maybe implement an intrusive dll to store tcbs in registry
        enum class thd_state : uint8_t { running = 0, running_pending = 1, blocked = 2, handshaking = 3, need_start = 4, dead = 5 };
        class tcb : public tcb_handle {
            thd_mark_info _mark_info;
            std::atomic<thd_state> _thd_state;
            uint8_t _opcode;
            arena _arena;
        public:
            tcb(size_t* start_args, uint8_t args_cnt) : tcb_handle(start_args, args_cnt), _thd_state(thd_state::running), _opcode(0) {}

            thd_mark_info& get_mark_info() { return _mark_info; }
            arena& get_arena() { return _arena; }

            void add_pending(uint8_t opcode) { _opcode = opcode; }
            // Opcode == 0 is reserved
            uint8_t get_opcode() const { return _opcode; }

            thd_state load_state() const { return _thd_state.load(std::memory_order_acquire); }
            void safe_transition(thd_state new_state) { _thd_state.store(new_state, std::memory_order_release); }
            void notify_transition() { _thd_state.notify_one(); }
            void wait_transition(thd_state old) const { _thd_state.wait(old); }
            // acq because we are reading the old state, rel because we are usually publishing opcode before this
            bool try_transition(thd_state expected_old, thd_state _new) {
                return _thd_state.compare_exchange_strong(expected_old, _new, std::memory_order_acq_rel);
            }
        };
    }



}