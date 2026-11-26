#pragma once
#include <atomic>
#include <cstdint>

#include "TCB.h"
#include "customization.h"
#include "gc-heuristic.h"
#include "gc-metrics.h"
#include "pg-meta.h"
#include "ref-slot.h"

namespace gc {
    void init_gc(uint8_t& flag, gc_tuning tuning = {});
    void end_gc();
    [[nodiscard]] tcb_handle* create_tcb(void* start_args, size_t arg_cnt);
    void init_thd(tcb_handle* handle, void* stack_start);
    void delete_tcb(tcb_handle* handle);
    // Must be done once at the start of the program from a single thread
    void register_root(size_t* root);
    void enter_blocked(tcb_handle* handle);
    void exit_blocked(tcb_handle* handle);
    // Blocks until a cycle completes, the one already running counts
    void collect(tcb_handle* handle);
    gc_stats stats();
    // cleanup and arg are kept alive until tracked dies if they are objects, so they must not point to
    // tracked or it never dies
    void add_cleanup(managed* tracked, size_t cleanup, size_t arg);
    namespace detail {
        [[nodiscard]] managed* alloc(size_t sz, tcb_handle* handle, bool zero);
    }

    // If needs_zeroing is false, the constructor must make sure that the entire object is setup in the constructor
    // (meaning there the tracer cannot possibly reach stale memory)
    // memory fence is required to publish everything the object wrote to its header
    template<typename T, typename... Args>
    [[nodiscard]] T* make_gc(tcb_handle* handle, size_t extra_bytes, Args&&... args) {
        static_assert(std::is_base_of_v<managed, T>);
        auto* obj = new (detail::alloc(sizeof(T) + extra_bytes, handle, T::needs_zeroing))
            T(std::forward<Args>(args)...);
        std::atomic_thread_fence(std::memory_order_release);
        // Always pin when allocating in case there is a collection running right now
        if (obj_is_pinned(obj)) [[unlikely]] detail::pg_meta::head_from_ptr(obj)->pin();
        return obj;
    }

    void write_barrier(tcb_handle* handle, managed* obj);
    void poll_safepoint(tcb_handle* handle);
    // These two functions exist because their check will be inlined into llvm for better perf
    // But the above functions are still needed for some parts of ESL's object handling
    void exec_at_safepoint(tcb_handle* handle);
    void push_wbbuf(tcb_handle* handle);
}
