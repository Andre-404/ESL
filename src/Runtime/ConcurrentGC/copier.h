#pragma once

#include <atomic>
#include <mutex>
#include <span>
#include <vector>
#include <optional>

#include "pg-meta.h"
#include "work-queue.h"
#include "allocator/reloc-hdrs.h"

namespace gc::detail {
    struct copy_plan {
        std::vector<pg_meta*> runs;
        size_t bytes;   // moved
        size_t freed;   // handed back
    };

    // Compacts sparse runs of a size class into the front of the same runs: survivors keep their
    // order, the emptied tail is retired by the pruner, and reloc hdrs + units map every old
    // address to the new one
    class copier {
        struct candidate { pg_meta* pg; uint32_t live; uint32_t holes; };
        // A list's candidates, sparsest first
        using list_cands = std::vector<candidate>;

        double _evac_threshold;
        std::mutex _mtx;
        std::vector<list_cands> _clists;
        work_queue<copy_plan> _plans;
        std::atomic<size_t> _moved;

        // The first `runs` candidates of a list picked so far, the blocks they hold and how many runs
        // packing them empties
        struct selection { size_t runs, live_blocks, runs_freed; };
        // How far a list's selection has to grow to empty one more run, and the bytes that moves
        struct extension { size_t byte_cost, list, runs_added, live_blocks; };

        std::optional<extension> try_extend(size_t list, const selection& sel) const;
        // Greedly grows the selections
        std::vector<selection> select_within_budget(size_t budget) const;
        // Up to threads plans for every selection that empties at least one run, each a strip of its own
        std::vector<copy_plan> plans_from(const std::vector<selection>& sels, size_t threads) const;
        static std::optional<copy_plan> plan_from(std::span<const candidate> runs, size_t offset, size_t stride);

    public:
        explicit copier(double evac_threshold) : _evac_threshold(evac_threshold), _clists(), _plans(), _moved(0) {}

        void gather(pg_meta* list, bool stranded_only = false);
        size_t select(size_t budget, size_t min_gain = 1, size_t threads = 1);
        // Taking plans doesn't change the answer because the vector is never emptied concurrently
        bool has_plans() const { return !_plans.empty(); }
        copy_plan* take_plan() { return _plans.take(); }
        void compact(const copy_plan& plan, reloc_hdrs& relocs);
        // Returns the bytes compact moved
        size_t end_copy();
    };
}
