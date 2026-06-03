#include "gc.h"

using namespace gc;

static detail::tcb& to_impl(tcb_handle* h) { return *reinterpret_cast<detail::tcb*>(h); }


void collector::set_paused(tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    auto& mark_info = tcb.get_mark_info();
    mark_info.capture_ctx();
    mark_info.set_wbbuf(_marker.replace_buf(mark_info.get_wbbuf()));
    _active_thds.fetch_sub(1, std::memory_order_release);
    _active_thds.notify_one();
    tcb.set_state(detail::thd_state::blocked);
}

void collector::set_resumed(tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    while (true) {
        _active_thds.fetch_add(1, std::memory_order_seq_cst);
        if (_gc_flag.load(std::memory_order_acquire)) {
            _active_thds.fetch_sub(1, std::memory_order_release);
            _active_thds.notify_one();
            _gc_flag.wait(static_cast<uint8_t>(detail::gc_state::stw));
            continue;
        }
        break;
    }
    tcb.set_state(detail::thd_state::running);
}
