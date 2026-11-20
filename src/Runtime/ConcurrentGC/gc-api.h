#pragma once
#include <atomic>
#include <cstdint>

#include "TCB.h"
#include "ref-slot.h"

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
        [[nodiscard]] managed* alloc(size_t sz, bool pinned, tcb_handle* handle);
    }

    // TODO: are these fences overkill?
    // Contract: cant gc allocate inside of object constructors nor poll safepoints
    // but can call write barrier, can allocate/poll safepoint inside gc_init
    // object is safe from tracing while inside constructor, after that every field must be in a known state
    template<typename T, typename... Args>
    [[nodiscard]] T* make_gc(tcb_handle* handle, bool pinned, size_t extra_bytes, Args&&... args) {
        static_assert(std::is_base_of_v<managed, T>);
        auto* obj = new (detail::alloc(sizeof(T) + extra_bytes, pinned, handle)) T(std::forward<Args>(args)...);
        std::atomic_thread_fence(std::memory_order_release); 
        if constexpr (requires (T* t) { t->gc_init(); }) obj->gc_init();
        return obj;
    }

    void write_barrier(tcb_handle* handle, managed* obj);
    void poll_safepoint(tcb_handle* handle);
    // These two functions exist because their check will be inlined into llvm for better perf
    // But the above functions are still needed for some parts of ESL's object handling
    void exec_at_safepoint(tcb_handle* handle);
    void push_wbbuf(tcb_handle* handle);

    // Same thing as gc_read_ptr in jitted code (runtime_bridge::build_ir_helpers). obj_trace loads
    // through it with acquire (see load_raw), which is how a mark heals every slot
    template<managed_ptr T>
    [[gnu::always_inline]] inline T read_ref(T& slot, std::memory_order order = relaxed) {
        auto w = (size_t)load_raw(slot, order);
        auto x = w ^ good_color();
        if (x & config::color_bit) [[unlikely]] return (T)heal_ptr((size_t*)&slot, w);
        return (T)x;
    }

    // Same thing as gc_store_ptr
    // A null gets the color too, it reads back as null either way
    template<managed_ptr T>
    [[gnu::always_inline]] inline void store_ref(T& slot, std::type_identity_t<T> value) {
        store_raw(slot, (T)((size_t)value | good_color()));
    }
}
