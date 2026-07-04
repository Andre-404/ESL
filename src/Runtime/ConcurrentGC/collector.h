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
    enum class request_type : uint8_t {
        none = 0,
        normal = 1,
        express = 2,
        end = 3
    };
    class collector {
        // Roughly grouped by cache lines
        tcb_registry _tcb_registry;

        std::atomic<size_t> _alloc_sz;
        post_manager _synchronizer;
        std::atomic_ref<uint8_t> _gc_flag;
        std::atomic<request_type> _collection_req;
        sync_point _gate;
        std::vector<size_t*> _roots;

        marker _marker;
        copier _copier;

        gc_heuristics _heuristic;
        pruner _pruner;
        std::thread _worker;

        // Intentionally last because it contains large cache
        pg_manager _pg_manager;

        void set_state(gc_state new_state) {
            _gc_flag.store((uint8_t)new_state, std::memory_order_release);
        }
        bool should_force_stw() const {
            return _collection_req.load(std::memory_order_acquire) == request_type::express;
        }
        // Page mutators, shared by stw_phase and phases
        auto update_live_fn() const {
            return [](pg_meta* start) {
                auto tmp = start;
                while (tmp) {
                    tmp->compute_live();
                    tmp = tmp->next();
                }
                return start;
            };
        }
        auto copy_objs_fn() const {
            return [this](pg_meta* start) { if (start) _copier.copy_objects(start); return start; };
        }
        auto update_ptrs_fn() const {
            return [this](pg_meta* start) { if (start) _copier.update_ptrs(start); return start; };
        }
        auto prune_pgs_fn() {
            return [this](pg_meta* start) {
                auto [empty, in_use] = _pruner.prune(start);
                _pg_manager.dealloc_pgs(empty);
                return in_use;
            };
        }
        std::vector<tcb*> post_with_state(gc_state s, uint8_t op);

        void force_collection(int64_t sz, tcb* handle);

        // While phase1 can be serialized for multiple threads, phase2 has to make parallel progress across every thread
        void phase1(tcb* handle, bool pin);
        void phase2(tcb* handle);
        void mark_phase();
        size_t stw_phase();
        void end_cycle(size_t alloc_snapshot);

        void concurrent_loop();
        void handle_pending(tcb* handle);

        [[gnu::cold]] void alloc_update(tcb* t, size_t debt);

    public:
        explicit collector(uint8_t& flag) : _gc_flag(flag), _collection_req(request_type::none), _copier(config::copy_evac_threshold) {
            _worker = std::thread { &collector::concurrent_loop, this };
        }
        ~collector() {
            _collection_req = request_type::end;
            _collection_req.notify_all();
            _worker.join();
        }
        void thd_prologue(tcb* handle);
        void set_paused(tcb* handle);
        void set_resumed(tcb* handle);
        tcb* create_tcb(size_t* start_args, uint8_t args_cnt);
        void delete_tcb(tcb* handle);

        void flush_wbbuf(tcb* handle);
        void register_root(size_t* root);

        managed* alloc(size_t sz, tcb* handle);

        void process_pending(tcb* handle);

        bool wb_active() const { return _gc_flag.load(std::memory_order_acquire) > 0; }
    };
}