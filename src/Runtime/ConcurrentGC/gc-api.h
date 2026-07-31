#pragma once
#include <atomic>
#include <cstdint>

#include "TCB.h"

namespace gc {
    void init_gc(uint8_t& flag);
    void end_gc();
    [[nodiscard]] tcb_handle* create_tcb(void* start_args, size_t arg_cnt);
    void init_thd(tcb_handle* handle, void* stack_start);
    void delete_tcb(tcb_handle* handle);
    // Must be done once at the start of the program from a single thread
    void register_root(size_t* root);
    void enter_blocked(tcb_handle* handle);
    void exit_blocked(tcb_handle* handle);
    namespace detail {
        [[nodiscard]] managed* alloc(size_t sz, tcb_handle* handle);
    }

    // TODO: are these fences overkill?
    // Contract: cant gc allocate inside of object constructors nor poll safepoints
    // but can call write barrier, can allocate/poll safepoint inside gc_init
    // object is safe from tracing while inside constructor, after that every field must be in a known state
    template<typename T, typename... Args>
    [[nodiscard]] T* make_gc(tcb_handle* handle, size_t extra_bytes, Args&&... args) {
        static_assert(std::is_base_of_v<managed, T>);
        auto* obj = new (detail::alloc(sizeof(T) + extra_bytes, handle)) T(std::forward<Args>(args)...);
        std::atomic_thread_fence(std::memory_order_release); 
        if constexpr (requires (T* t) { t->gc_init(); }) obj->gc_init();
        std::atomic_thread_fence(std::memory_order_release); 
        return obj;
    }

    void write_barrier(tcb_handle* handle, managed* obj);
    void poll_safepoint(tcb_handle* handle);
    // These two functions exist because their check will be inlined into llvm for better perf
    // But the above functions are still needed for some parts of ESL's object handling
    void exec_at_safepoint(tcb_handle* handle);
    void push_wbbuf(tcb_handle* handle);

    // For object handling
    managed* to_moved_ptr(managed* obj);
}
