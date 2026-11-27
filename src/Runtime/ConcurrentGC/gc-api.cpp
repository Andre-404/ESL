#include "gc-api.h"
#include <algorithm>
#include <cstring>
#include "collector.h"
#include "../../Includes/rpmalloc/rpmalloc.h"

namespace gc {
    namespace {
        static detail::collector* GC = nullptr;
        static detail::tcb* t(tcb_handle* handle) {
            return reinterpret_cast<detail::tcb*>(handle);
        }
    }
    void init_gc(uint8_t& flag, gc_tuning tuning) {
        rpmalloc_initialize(nullptr);
        GC = new detail::collector { flag, tuning };
    }
    // Never freed: threads that haven't finished keep polling and allocating into it until the process exits
    void end_gc() {
        GC->stop();
    }
    tcb_handle* create_tcb(void* start_args, size_t arg_cnt) {
        return GC->create_tcb(static_cast<size_t*>(start_args), arg_cnt);
    }
    void init_thd(tcb_handle* handle, void* stack_start) {
        auto tcb = t(handle);
        tcb->get_mark_info().track_stack((size_t)stack_start);
        GC->thd_prologue(tcb);
    }
    void delete_tcb(tcb_handle* handle) { GC->delete_tcb(t(handle)); }
    void register_root(size_t* root) { GC->register_root(root); }
    void run_blocked(tcb_handle* handle, function_ref<void() const> f) { GC->run_blocked(t(handle), f); }
    void collect(tcb_handle* handle) { GC->collect(t(handle)); }
    gc_stats stats() { return GC->stats(); }
    int64_t add_cleanup(managed* tracked, size_t cleanup, size_t arg, size_t external_bytes) {
        return GC->add_cleanup(tracked, cleanup, arg, external_bytes);
    }
    bool remove_cleanup(int64_t id) { return GC->remove_cleanup(id); }

    managed* detail::alloc(size_t sz, tcb_handle* handle, bool zero) {
        auto res = GC->alloc(sz, t(handle));
        if (!zero) return res;
        constexpr size_t chunk = 256 * 1024;
        for (size_t i = 0; i < sz; i += chunk) {
            if (i) poll_safepoint(handle);
            std::memset((uint8_t*)res + i, 0, std::min(chunk, sz - i));
        }
        return res;
    }

    void write_barrier(tcb_handle* handle, managed* obj) {
        auto tcb = t(handle);
        assert(tcb->is_running() && "a blocked thread can't touch the heap");
        if (!GC->wb_active()) return;
        if (tcb->get_mark_info().get_wbbuf()->push(obj)) GC->flush_wbbuf(tcb);
    }
    void poll_safepoint(tcb_handle* handle) { GC->poll(t(handle)); }
    void exec_at_safepoint(tcb_handle* handle) { GC->process_pending(t(handle)); }
    void push_wbbuf(tcb_handle* handle) { GC->flush_wbbuf(t(handle)); }
}
