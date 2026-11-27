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
#include "cleanups.h"

namespace gc::detail {
    enum class gc_state : uint8_t {
        none = 0,
        marking = 1,
        stw = 2
    };
    constexpr uint8_t gc_state_mask = 0b011;
    class collector {
        // Roughly grouped by cache lines
        tcb_registry _tcb_registry;

        std::atomic<size_t> _alloc_sz;
        size_t _mark_alloc_start;
        post_manager _thd_state_mngr;
        std::atomic_ref<uint8_t> _gc_flag;
        // Master copy of the good pointer color, every thread caches it in r14
        std::atomic<uint64_t> _good_color { 0 };
        collection_request _collection_req;
        bool _express = false;

        sync_point _gate;
        std::mutex _root_mtx;
        std::vector<size_t*> _roots;
        std::unordered_map<tcb*, std::span<size_t>> _temp_roots;

        marker _marker;
        copier _copier;
        cleanups _cleanups;

        gc_heuristics _heuristic;
        gc_metrics    _metrics;
        cycle_stats   _cycle;

        pruner _pruner;
        
        // Intentionally last because it contains large cache
        pg_manager _pg_manager;
        std::thread _worker;

        enum class stw_role : uint8_t { mutator, collector };

        bool state_is(gc_state s) const {
            return (_gc_flag.load(std::memory_order_acquire) & gc_state_mask) == (uint8_t)s;
        }
        void reload_color() const {
            platform::write_color(_good_color.load(std::memory_order_relaxed));
        }

        // Page mutators, shared by mutator and worker stw functions. Both tolerate a null
        // list, which is what an arena with no pages of a given size class hands them.
        auto gather_fn() {
            return [this](pg_meta* start) { _copier.gather(start, !_express); return start; };
        }
        auto prune_pgs_fn() {
            return [this](pg_meta* start) {
                auto b = _pg_manager.start_batch();
                return _pruner.prune(start, _pg_manager.bits(), [&](pg_meta* pg) { b.add(pg); });
            };
        }
        // Every stw phase runs its mutator over the pages this thread owns, and the collector
        // additionally over the ones handed back to the manager by dead threads
        template<typename F>
        void mutate_all(std::span<tcb*> owned, stw_role role, F fn) {
            for (auto t : owned) t->get_arena().mutate_owned(fn);
            if (role == stw_role::collector) _pg_manager.mutate_owned(fn);
        }
        auto get_obj_base() {
            return [&](uint8_t* ptr) {
                if (auto pg = _pg_manager.pg_from_ptr(ptr))
                    return pg->from_interior(ptr);
                return (managed*)nullptr;
            };
        }

        std::vector<tcb*> post_with_state(uint8_t flags, uint8_t op);
        void stw_enter(std::span<tcb*> owned, stw_role role);
        size_t stw_mark(std::span<tcb*> owned, stw_role role);
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
        [[gnu::cold, clang::noinline]] managed* force_collection(size_t sz, tcb* handle);

    public:
        explicit collector(uint8_t& flag, gc_tuning tuning = {}) 
            : _gc_flag(flag), _mark_alloc_start(0),_copier(config::copy_evac_threshold),
            _heuristic(tuning), _pg_manager(tuning.scavenge)
        {
            _worker = std::thread { &collector::concurrent_loop, this };
            _cleanups.run(*this);
        }
        ~collector() {
            stop();
        }
        // Waits for the cleanups already queued, then ends collection. Mutators may keep running against
        // the collector after it, the runner too
        void stop() {
            if (!_worker.joinable()) return;
            _cleanups.drain();
            _collection_req.shutdown();
            _worker.join();
        }
        void thd_prologue(tcb* handle);
        void set_paused(tcb* handle);
        void set_resumed(tcb* handle);
        tcb* create_tcb(size_t* start_args, uint8_t args_cnt);
        void delete_tcb(tcb* handle);

        void flush_wbbuf(tcb* handle);
        void register_root(size_t* root);
        // Blocks until a cycle that starts after the call completes
        void collect(tcb* handle);
        void add_cleanup(managed* tracked, size_t cleanup, size_t arg) { _cleanups.add(tracked, cleanup, arg); }

        managed* alloc(size_t sz, tcb* handle);

        void process_pending(tcb* handle);
        void poll(tcb* handle) {
            if (handle->load_state() == thd_state::has_pending) process_pending(handle);
        }

        bool wb_active() const { return _gc_flag.load(std::memory_order_acquire) > 0; }

        gc_stats stats();
    };
}