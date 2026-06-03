#pragma once

#include "pg-manager.h"
#include "copier.h"
#include "marker.h"
#include "pruner.h"
#include "TCB-registry.h"
#include "sync-point.h"

namespace gc {
    namespace detail {
        enum class gc_state : uint8_t {
            none = 0,
            marking = 1,
            stw = 2
        };
    }
    class collector {
    protected:
        detail::marker _marker;
        detail::copier _copier;
        detail::pruner _pruner;
        detail::tcb_registry _tcb_registry;
        detail::sync_point _gate;
        std::atomic<size_t> _active_thds;
        std::atomic_ref<uint8_t> _gc_flag;
        // Intentionally last because it contains large cache
        detail::pg_manager _pg_manager;

        void set_state(detail::gc_state new_state);
        void concurrent_loop();
        void clean_mark_bitmaps();
        void wait_stw_end();
    public:
        void set_paused(tcb_handle* handle);
        void set_resumed(tcb_handle* handle);
        tcb_handle* create_tcb(size_t* start_args, uint8_t args_cnt);
        void delete_tcb(tcb_handle* handle);

        void flush_wbbuf(tcb_handle* handle);
        void register_root(size_t* root);

        managed* alloc(size_t sz, tcb_handle* handle);

        void phase1(tcb_handle* handle);
        void phase2(tcb_handle* handle);

    };
}