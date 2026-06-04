#include "gc.h"

#include <iostream>

#include "../../Includes/fmt/format.h"
#include "../../Includes/rpmalloc/rpmalloc.h"

using namespace gc;

static detail::tcb& to_impl(tcb_handle* h) { return *reinterpret_cast<detail::tcb*>(h); }


void collector::set_state(gc_state new_state) {
    _gc_flag.store((uint8_t)new_state, std::memory_order_seq_cst);
}

void collector::clean_mark_bitmaps() {
    _pg_manager.foreach_active_pg(
        [&](detail::pg_meta* pg) {
            pg->clear_mark_bitmap();
        }
    );
}

void collector::force_collection(size_t sz) {

    std::cerr
        <<"Thread "
        <<std::this_thread::get_id()
        <<fmt::format("failed to allocate {} bytes, heap size is {}, exiting...\n", sz, _alloc_sz);
    std::abort();
}

void collector::_set_paused(tcb_handle *handle, detail::thd_state new_state) {
    auto& tcb = to_impl(handle);
    auto& mark_info = tcb.get_mark_info();
    mark_info.capture_ctx();
    flush_wbbuf(handle);
    thd_deactivate();
    tcb.set_state(new_state);
}

void collector::set_paused(tcb_handle *handle) {
    _set_paused(handle, detail::thd_state::blocked);
}

void collector::set_resumed(tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    while (true) {
        _active_thds.fetch_add(1, std::memory_order_seq_cst);
        auto res = _gc_flag.load(std::memory_order_acquire);
        if (res == (uint8_t)gc_state::marking || res == (uint8_t)gc_state::stw) {
            thd_deactivate();
            _gc_flag.wait(res);
            continue;
        }
        break;
    }
    tcb.set_state(detail::thd_state::running);
}

tcb_handle *collector::create_tcb(size_t *start_args, uint8_t args_cnt) {
    auto tcb = new (rpmalloc(sizeof(detail::tcb))) detail::tcb { start_args, args_cnt };
    _tcb_registry.add(tcb);
    return tcb;
}

void collector::delete_tcb(tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    _tcb_registry.remove(&tcb);
    thd_deactivate();

    _marker.push_buf(tcb.get_mark_info().get_wbbuf());

    auto& arena = tcb.get_arena();
    auto i = 0;
    arena.flush_alloc_caches();
    arena.mutate_caches([&](detail::pg_meta* start) {
        if (start) _pg_manager.release_pgs(i, start);
        i++;
        return nullptr;
    });
    arena.mutate_objs([&](detail::pg_meta* start) {
        if (start) _pg_manager.release_big_objs(start);
        return nullptr;
    });
    rpfree(handle);
}

void collector::flush_wbbuf(tcb_handle *handle) {
    auto& info = to_impl(handle).get_mark_info();
    _marker.push_buf(info.get_wbbuf());
    info.set_wbbuf(_marker.get_buf());
}

void collector::register_root(size_t *root) {
    _marker.register_root(root);
}

managed *collector::alloc(size_t sz, tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    auto& arena = tcb.get_arena();
    if (arena.get_debt() > config::debt_trigger) {
        if (_gc_flag.load(std::memory_order_acquire) == (uint8_t)gc_state::marking)
            arena.remove_debt(_marker.trace_n(config::debt_trigger));

    }
    auto res = arena.alloc(sz, _pg_manager);
    if (!res) [[unlikely]] {
        force_collection(sz);
        return alloc(sz, handle);
    }
    _alloc_sz.fetch_add(sz, std::memory_order_relaxed);
    return res;
}

void collector::phase1(tcb_handle *handle) {
    auto& tcb = to_impl(handle);
    _set_paused(handle, detail::thd_state::at_safepoint);
    _active_thds.wait(0);
}
