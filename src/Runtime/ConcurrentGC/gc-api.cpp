#include "gc-api.h"
#include "collector.h"
#include "copier.h"
#include "../../Includes/rpmalloc/rpmalloc.h"

namespace gc {
    namespace {
        static detail::collector* GC = nullptr;
        static detail::tcb* t(tcb_handle* handle) {
            return reinterpret_cast<detail::tcb*>(handle);
        }
    }
    void init_gc(uint8_t& flag) {
        rpmalloc_initialize();
        GC = new detail::collector { flag };
    }
    void end_gc() {
        delete GC;
    }
    [[nodiscard]] tcb_handle* create_tcb(void* start_args, size_t arg_cnt) {
        return GC->create_tcb(static_cast<size_t*>(start_args), arg_cnt);
    }
    void init_thd(tcb_handle* handle, void* stack_start) {
        auto tcb = t(handle);
        tcb->get_mark_info().track_stack((size_t)stack_start);
        GC->thd_prologue(tcb);
    }
    void delete_tcb(tcb_handle* handle) { GC->delete_tcb(t(handle)); }
    void register_root(size_t* root) { GC->register_root(root); }
    void enter_blocked(tcb_handle* handle) { GC->set_paused(t(handle)); }
    void exit_blocked(tcb_handle* handle) { GC->set_resumed(t(handle)); }

    [[nodiscard]] managed* alloc(size_t sz, tcb_handle* handle) {
        return GC->alloc(sz, t(handle));
    }

    void write_barrier(tcb_handle* handle, managed* obj) {
        if (!GC->wb_active()) return;
        auto tcb = t(handle);
        if (tcb->get_mark_info().get_wbbuf()->push(obj)) GC->flush_wbbuf(tcb);
    }
    void poll_safepoint(tcb_handle* handle) {
        auto tcb = t(handle);
        if (tcb->load_state() != detail::thd_state::has_pending) return;
        GC->process_pending(tcb);
    }
    // These 2 functions exists because the common check for them will be inlined into llvm
    // In the interest of having a complete gc the above functions are also provided which do the check + action
    void exec_at_safepoint(tcb_handle* handle) { GC->process_pending(t(handle)); }
    void push_wbbuf(tcb_handle* handle) { GC->flush_wbbuf(t(handle)); }

    managed* to_moved_ptr(managed* obj) { return detail::get_moved(obj); }
}
