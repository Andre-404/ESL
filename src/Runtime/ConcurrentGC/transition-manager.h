#pragma once

#include "TCB.h"

namespace gc::detail {
    class post_manager {
        std::atomic<size_t> _unack;
    public:
        post_manager() : _unack(0) {}

        // Returns handshaked threads so that work can be done on them
        // [possible transitions]: running -> running_pending, blocked -> handshaking (neither safe)
        std::vector<tcb*> post(auto snapshot, uint8_t opcode) {
            assert(_unack == 0);
            // Must set size here since some threads could complete their pending tasks while we're still iterating
            _unack = snapshot.size();
            auto vec = std::vector<tcb*> { };
            for (tcb* t : snapshot) {
                while (true) {
                    auto state = t->load_state();
                    if (state == thd_state::running) {
                        assert(t->get_opcode() == 0);
                        t->set_opcode(opcode);
                        // Lost CAS here means we went to blocked or dead, as that is the only other legal transition
                        if (t->try_transition(state, thd_state::has_pending)) break;
                        continue;
                    }
                    if (state == thd_state::blocked) {
                        // Lost CAS here means we went to running
                        if (!t->try_transition(state, thd_state::handshaking)) continue;
                        vec.push_back(t);
                        break;
                    }
                    // Only thing a dead thread can do is remove itself from the registry, don't count it in unack
                    if (state == thd_state::dead) {
                        ack();
                        break;
                    }
                    assert(false && "Unreachable, invariant (running, blocked, dead) violated on post.");
                }
            }
            return vec;
        }

        // Threads that were allocated during stw will need to be manually started, and handshaked threads need to return to blocked
        // [possible transitions]: handshaking -> blocked, need_start -> running (both safe)
        void finish_stw(auto snapshot) {
            for (tcb* t : snapshot) {
                auto state = t->load_state();
                if (state == thd_state::handshaking) t->transition(thd_state::blocked);
                else if (state == thd_state::need_start) t->transition(thd_state::running);
                assert(state == thd_state::handshaking || state == thd_state::running || state == thd_state::need_start);
            }
        }

        // These function don't depend on registry lock, functions above do
        // [possible transitions]: handshaking -> blocked
        // TCB can't be in handshake if it ack-ed
        void complete_handshake(std::vector<tcb*>& snapshot) {
            for (auto t : snapshot) {
                t->transition(thd_state::blocked);
                ack();
            }
        }
        // Check if we need to run should happen outside
        // [possible transitions]: running_pending -> running(safe)
        template<typename F>
        void execute_pending(tcb* t, F exec) {
            assert(t->load_state() == thd_state::has_pending);
            // transition to running_pending has release so opcode is surely visible
            exec(t);
            t->set_opcode(0); // reset opcode for sanity
            t->transition(thd_state::running);
        }

        // [possible transitions]: running_pending -> running (safe), running -> blocked (not safe)
        template<typename F>
        void enter_blocked(tcb* t, F&& exec) {
            while (true) {
                auto state = t->load_state();
                // GC cycles are rare compared to the number of time we will be doing something blocking
                if (state == thd_state::has_pending) [[unlikely]] {
                    execute_pending(t, std::forward<F>(exec));
                    continue;
                }
                // Only way to enter this function is from running_pending or running, everything else violates invariant
                assert(state == thd_state::running);
                if (t->try_transition(state, thd_state::blocked)) break;
            }
        }

        // Could be moved outside but i'd rather keep everything in one spot
        // [possible transitions]: blocked -> running (not safe)
        void exit_blocked(tcb* t) {
            while (true) {
                auto state = t->load_state();
                // GC cycles are rare compared to the number of time we will be doing something blocking
                if (state == thd_state::handshaking) [[unlikely]] {
                    // While this can theoretically go: handshaking -> blocked -> handshaking this is fine
                    // thd is paused and nothing is violated
                    t->wait_transition(thd_state::handshaking); continue;
                }
                assert(state == thd_state::blocked);
                if (t->try_transition(state, thd_state::running)) break;
            }
        }

        // [possible transitions]: running_pending -> running (safe), running -> dead (not safe)
        template<typename F>
        void thread_exit(tcb* t, F&& exec) {
            t->get_mark_info().untrack_stack();
            while (true) {
                auto state = t->load_state();
                if (state == thd_state::has_pending) [[unlikely]] {
                    execute_pending(t, std::forward<F>(exec));
                    continue;
                }
                assert(state == thd_state::running);
                if (t->try_transition(state, thd_state::dead)) break;
            }
        }

        void prologue(tcb* t) const {
            // There is no way to go back to need_start so no worry about ABA problem, standard says this won't spuriously fail
            t->wait_transition(thd_state::need_start);
        }

        // Called by threads after they complete their pending task, or by the gc thread
        void ack() {
            // TODO: is this the right memory order? seq_cst is def overkill but i wonder if we can drop to acq or release
            _unack.fetch_sub(1, std::memory_order_acq_rel);
            _unack.notify_all();
        }

        void wait_on_all_ack() const {
            while (true) {
                auto val = _unack.load(std::memory_order_acquire);
                if (val == 0) break;
                _unack.wait(val);
            }
        }
    };
    // Will get inlined into llvm code
    inline bool needs_safepoint(tcb* t) { return t->load_state() == thd_state::has_pending; }
}