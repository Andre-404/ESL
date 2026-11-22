#pragma once
#include "../common.h"
#include "ConcurrentGC/gc-api.h"
#include "ConcurrentGC/platform-specific.h"


namespace gc {
    // A reference Value is 0x7ffe in the top 16 bits, 0x7fff with the color bit (valueHelpers.h).
    // Every non reference is below this and every reference of either color at or above it
    constexpr int64_t ref_floor = 0x7ffe'0000'0000'0000;

    // Either color, for the collector which sees slots raw
    inline bool is_ref_word(Value w) { return (w >> 49) == (ref_floor >> 49); }

    // Same thing as gc_read_val in jitted code (runtime_bridge::build_ir_helpers). Branches
    // rather than selects, so a number doesn't wait on the check
    [[gnu::always_inline, gnu::hot]] inline Value read_ref(Value& slot, std::memory_order order = relaxed) {
        auto w = load_raw(slot, order);
        if ((int64_t)w < ref_floor) return w;
        auto x = w ^ good_color();
        if (x & config::color_bit) [[unlikely]] return heal_word(&slot, w);
        return x;
    }

    // Same thing as gc_store_val
    [[gnu::always_inline, gnu::hot]] inline void store_ref(Value& slot, Value v) {
        if ((int64_t)v >= ref_floor) v |= good_color();
        store_raw(slot, v);
    }

    inline tcb_handle* read_tcb() {
        return reinterpret_cast<tcb_handle*>(gc::platform::read_tcb());
    }

    inline void set_tcb(tcb_handle* handle) { gc::platform::set_tcb(handle); }

    inline void write_b(managed* obj) {
        write_barrier(read_tcb(), obj);
    }

    inline void poll() {
        poll_safepoint(read_tcb());
    }

    template<typename T, typename... Args>
    [[nodiscard]] T* esl_make_gc(size_t extra_bytes, Args&&... args) {
        return make_gc<T>(read_tcb(), extra_bytes, std::forward<Args>(args)...);
    }

    template<typename F>
    inline void run_blocking(F func) {
        gc::enter_blocked(read_tcb());
        func();
        gc::exit_blocked(read_tcb());
    }
}
