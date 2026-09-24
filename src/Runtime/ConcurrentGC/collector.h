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
        // Whether the cycle in flight nominates source pages, published before the marking
        // handshake so every mutator scores its own pages under the same answer.
        // TODO(step 6): becomes a gc_flag bit and drives stw's copy decision as well
        std::atomic<bool> _nominating;
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

        enum class stw_role : uint8_t { mutator, collector };

        // Page mutators, shared by mutator and worker stw functions. All three tolerate a null
        // list, which is what an arena with no pages of a given size class hands them.
        auto copy_objs_fn() const {
            return [this](pg_meta* start) { _copier.copy_objects(start); return start; };
        }
        auto update_ptrs_fn() const {
            return [this](pg_meta* start) { _copier.update_ptrs(start); return start; };
        }
        auto prune_pgs_fn() {
            return [this](pg_meta* start) {
                auto b = _pg_manager.start_batch();
                return _pruner.prune(start, _pg_manager.bits(), [&](pg_meta* pg) { b.add(pg); });
            };
        }
        // Don't nominate pages if we're not in a copying cycle
        auto observe_pgs_fn() const {
            auto nominate = _nominating.load(std::memory_order_acquire);
            auto should_nominate = [this, nominate](pg_meta* pg, uint16_t occ, uint8_t age) {
                // Large objects are never evacuated, and occ == 0 is either an empty page the sweep
                // will retire or one the allocator is filling right now - a target either way
                if (pg->szclass() == config::large_class || occ == 0) return false;
                auto pred_survivors = occ * _pruner.survival().survival(age);
                return nominate && pred_survivors < config::nominate_threshold * pg->block_cnt();
            };
            
            return [this, predicate = std::move(should_nominate)](pg_meta* start) {
                for (auto pg = start; pg; pg = pg->next()) {
                    if (!pg->is_active()) continue;
                    auto occ = uint16_t(pg->compute_alloc());
                    auto age = pg->history().age();
                    pg->set_history({ occ, age });
                    if (predicate(pg, occ, age)) pg->nominate();
                }
                return start;
            };
        }
        // Every stw phase runs its mutator over the pages this thread owns, and the collector
        // additionally over the ones handed back to the manager by dead threads
        template<typename F>
        void mutate_all(std::span<tcb*> owned, stw_role role, F fn) {
            for (auto t : owned) t->get_arena().mutate_owned(fn);
            if (role == stw_role::collector) _pg_manager.mutate_owned(fn);
        }
        void observe_thread_pages(tcb* t) { t->get_arena().observe_owned(observe_pgs_fn()); }

        auto get_obj_base() {
            return [&](uint8_t* ptr) {
                if (auto pg = _pg_manager.pg_from_ptr(ptr))
                    return pg->from_interior(ptr);
                return (managed*)nullptr;
            };
        }

        std::vector<tcb*> post_with_state(gc_state s, uint8_t op);
        void stw_enter(std::span<tcb*> owned, stw_role role);
        size_t stw_mark(std::span<tcb*> owned, stw_role role, bool copying);
        void stw_copy(std::span<tcb*> owned, stw_role role);
        void stw_prune(std::span<tcb*> owned, stw_role role);
        size_t stw(std::span<tcb*> owned, stw_role role);

        void concurrent_mark();
        size_t worker_stw();
        void mutator_stw(tcb* handle);
        void collect_metrics();
        uint64_t* end_cycle(size_t alloc_snapshot);

        void concurrent_loop();
        void handle_pending(tcb* handle);

        [[gnu::cold, clang::noinline]] void alloc_update(tcb* t, size_t debt);
        [[gnu::cold, clang::noinline]] void force_collection(int64_t sz, tcb* handle);

    public:
        explicit collector(uint8_t& flag, gc_tuning tuning = {}) 
            : _gc_flag(flag), _nominating(false),
              _copier(config::copy_evac_threshold), _heuristic(tuning) {
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

        managed* alloc(size_t sz, bool pinned, tcb* handle);

        void process_pending(tcb* handle);

        bool wb_active() const { return _gc_flag.load(std::memory_order_acquire) > 0; }

        const gc_metrics& metrics() const { return _metrics; }

        const survival_model& survival() const { return _pruner.survival(); }
    };
}