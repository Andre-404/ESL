#pragma once

#include <thread>

#include "pg-manager.h"
#include "copier.h"
#include "marker.h"
#include "pruner.h"
#include "TCB-registry.h"
#include "sync-point.h"
#include "transition-manager.h"
#include "gc-heuristic.h"

namespace gc::detail {
    enum class gc_state : uint8_t {
        none = 0,
        marking = 1,
        stw = 2
    };
    class collector {
        // Roughly grouped by cache lines
        tcb_registry _tcb_registry;

        std::atomic<size_t> _alloc_sz;
        post_manager _synchronizer;
        std::atomic_ref<uint8_t> _gc_flag;
        sync_point _gate;

        marker _marker;
        copier _copier;

        gc_heuristics _heuristic;
        pruner _pruner;
        std::thread _worker;

        // Intentionally last because it contains large cache
        pg_manager _pg_manager;

        void set_state(gc_state new_state);
        [[noreturn]] void concurrent_loop();

        void force_collection(size_t sz);

        void mark_phase();
        void stw_phase();

        // Page mutators, shared by stw_phase and phase2
        auto copy_objs_fn() const {
            return [this](pg_meta* start) { _copier.copy_objects(start); return start; };
        }
        auto update_ptrs_fn() const {
            return [this](pg_meta* start) { _copier.update_ptrs(start); return start; };
        }
        auto prune_pgs_fn() {
            return [this](pg_meta* start) {
                auto [empty, in_use] = _pruner.prune(start);
                _pg_manager.dealloc_pgs(empty);
                return in_use;
            };
        }
        std::vector<tcb*> post_with_state(gc_state s, uint8_t op);

        // While phase1 can be serialized for multiple threads, phase2 has to make parallel progress across every thread
        void phase1(tcb* handle, bool pin);
        void phase2(tcb* handle);

        void handle_pending(tcb* handle);

    public:
        explicit collector(uint8_t& flag) : _gc_flag(flag), _copier(config::copy_evac_threshold), _pruner(0.85) {
            _worker = std::thread(&collector::concurrent_loop, this);
        }
        void set_paused(tcb* handle);
        void set_resumed(tcb* handle);
        tcb* create_tcb(size_t* start_args, uint8_t args_cnt);
        void delete_tcb(tcb* handle);

        void flush_wbbuf(tcb* handle);
        void register_root(size_t* root);

        managed* alloc(size_t sz, tcb* handle);

        void process_pending(tcb* handle);
    };
}