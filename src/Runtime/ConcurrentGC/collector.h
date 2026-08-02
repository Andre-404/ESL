#pragma once

#include <thread>
#include <unordered_map>

#include "pg-manager.h"
#include "copier.h"
#include "marker.h"
#include "pruner.h"
#include "TCB-registry.h"
#include "sync-point.h"
#include "transition-manager.h"
#include "gc-heuristic.h"
#include "gc-metrics.h"
#include "collection-request.h"

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
        post_manager _thd_state_mngr;
        std::atomic_ref<uint8_t> _gc_flag;
        collection_request _collection_req;

        sync_point _gate;
        std::mutex _root_mtx;
        std::vector<size_t*> _roots;
        std::unordered_map<tcb*, std::span<size_t>> _temp_roots;

        marker _marker;
        copier _copier;

        gc_heuristics _heuristic;
        gc_metrics    _metrics;
        cycle_stats   _cycle;

        pruner _pruner;
        
        // Intentionally last because it contains large cache
        pg_manager _pg_manager;
        std::thread _worker;

        // Page mutators, shared by mutator and worker stw functions
        auto copy_objs_fn() const {
            return [this](pg_meta* start) { if (start) _copier.copy_objects(start); return start; };
        }
        auto update_ptrs_fn() const {
            return [this](pg_meta* start) { if (start) _copier.update_ptrs(start); return start; };
        }
        auto prune_pgs_fn() {
            return [this](pg_meta* start) {
                auto b = _pg_manager.start_batch();
                auto fn = [&](pg_meta* pg) { b.add(pg); };
                return _pruner.prune(start, fn);
            };
        }
        auto get_obj_base() {
            return [&](uint8_t* ptr) -> managed* {
                auto pg = _pg_manager.pg_from_ptr((uintptr_t)ptr);
                if (!pg) return nullptr;
                return pg->from_interior((uint8_t*)ptr);
            };
        }

        enum class stw_role : uint8_t { mutator, collector };

        std::vector<tcb*> post_with_state(gc_state s, uint8_t op);

        void force_collection(int64_t sz, tcb* handle);
        void stw_enter(std::span<tcb*> owned);
        size_t stw_mark(std::span<tcb*> owned, stw_role role, bool copying);
        void stw_copy(std::span<tcb*> owned, stw_role role);
        void stw_prune(std::span<tcb*> owned, stw_role role);
        size_t stw(std::span<tcb*> owned, stw_role role);

        void concurrent_mark();
        size_t worker_stw();
        void mutator_stw(tcb* handle);
        void collect_metrics();
        void end_cycle(size_t alloc_snapshot);

        void concurrent_loop();
        void handle_pending(tcb* handle);

        [[gnu::cold]] void alloc_update(tcb* t, size_t debt);

    public:
        explicit collector(uint8_t& flag, gc_tuning tuning = {}) 
            : _gc_flag(flag), _copier(config::copy_evac_threshold), _heuristic(tuning) {
            _worker = std::thread { &collector::concurrent_loop, this };
        }
        ~collector() {
            _collection_req.shutdown();
            _worker.join();
            printf("%s\n", _metrics.report().c_str());
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

        const gc_metrics& metrics() const { return _metrics; }
    };
}