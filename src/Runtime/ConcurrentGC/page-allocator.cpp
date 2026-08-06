#include "gc-config.h"
#include "radix-tree.h"
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <new>
#include <thread>

#include <algorithm>

#include "os-memory.h"
#include "page-allocator.h"

using namespace gc;
using namespace gc::detail;

namespace {
    constexpr std::size_t hdr_commit_sz = 64ull << 10;
    constexpr std::size_t hdrs_per_chunk = hdr_commit_sz / sizeof(pg_meta);
    constexpr std::size_t hdr_chunks = (heap_geometry::total_pages + hdrs_per_chunk - 1) / hdrs_per_chunk;
    constexpr std::size_t reserve_bytes = config::heap_max_sz + (hdr_chunks * hdr_commit_sz);

    constexpr std::size_t alloc_words = heap_geometry::words;
    constexpr std::size_t committed_words = heap_geometry::total_pages / config::commit_granule / 64;
    constexpr std::size_t meta_bytes = (alloc_words + committed_words) * sizeof(uint64_t)
                                     + heap_geometry::total_entries * sizeof(summary);

    constexpr auto G = config::commit_granule;
    constexpr auto granule_bytes = G * config::page_sz;
    std::size_t granule_of(std::size_t page) { return page / G; }
    bool granule_all_free(std::size_t g, const radix_tree& tree) {
        return !tree.bits().any_set(g * G, G);
    }
    std::size_t granules_unalloced(std::size_t start, std::size_t n, const radix_tree& tree) {
        auto end = start + n;
        std::size_t d = 0;
        for (auto g = granule_of(start); g <= granule_of(end - 1); ++g) {
            auto lo = std::max(start, g * G);
            auto hi = std::min(end, (g + 1) * G);
            if (hi - lo == G || granule_all_free(g, tree)) ++d;
        }
        return d;
    }

    // Paces the background scavenger: on each wake it decides how many commit granules to
    // hand back to the OS, and how long to sleep afterwards. Pure policy - it issues no
    // syscalls and touches no allocator state, so it can be unit tested by feeding it
    // synthetic counters and a synthetic clock. Everything is counted in commit granules

    class heuristic {
        using clock = std::chrono::steady_clock;

        scavenge_policy _cfg;
        double _peak = 0.0;
        double _goal = 0.0;
        double _f    = 0.0;
        double _ns_per_granule;
        clock::time_point _last{};
        bool _started = false;
    public:

        struct observation {
            std::size_t in_use   = 0;   // granules currently allocated
            std::size_t resident = 0;   // granules physically backed (RSS proxy, not _committed)
            double      thrash   = 0.0; // recent decommit->refault rate in [0,1]; widens the band
        };

        explicit heuristic(scavenge_policy cfg)
            : _cfg(cfg), _ns_per_granule(cfg.init_ns_per_granule) {}

        // Top of each wake. Advances the decaying demand peak using `now`, then returns how
        // many high-address free granules to decommit this quantum. 0 means resident memory
        // is at or below the goal - nothing to do.
        std::size_t plan(clock::time_point now, observation obs) {
            double dt = _started ? std::chrono::duration<double>(now - _last).count() : 0.0;
            _last = now;
            // A fresh peak seeds to current demand; after that it can only rise to meet demand
            // or decay slowly toward it, so a transient free trough does not trigger a scavenge.
            if (!_started) { _peak = double(obs.in_use); _started = true; }
            else _peak = std::max(double(obs.in_use), _peak * std::exp(-_cfg.decay_per_sec * dt));

            _goal = std::max(double(_cfg.min_retain),
                             _peak * (1.0 + _cfg.headroom + _cfg.thrash_gain * obs.thrash));

            double excess = double(obs.resident) - _goal;
            if (excess <= 0.0) { _f = _cfg.base_cpu; return 0; }

            // Work harder the further resident memory has drifted above the goal.
            _f = std::clamp(_cfg.base_cpu + _cfg.cpu_gain * (excess / _goal),
                            _cfg.base_cpu, _cfg.max_cpu);

            double quantum = _cfg.quantum_ns / std::max(1.0, _ns_per_granule);
            auto want = std::size_t(std::min(excess, quantum));
            return std::max<std::size_t>(want, 1); // make progress even on a sub-granule excess
        }

        // After the decommit. Feeds back the measured cost, refines the per-granule estimate,
        // and returns how long to sleep to hold the CPU fraction plan() just chose. A wake
        // that scavenged nothing sleeps the idle interval so the decaying goal is re-checked.
        std::chrono::nanoseconds pace(std::size_t granules_done, std::chrono::nanoseconds work) {
            if (granules_done == 0 || work <= std::chrono::nanoseconds::zero())
                return _cfg.idle_sleep;

            double per = double(work.count()) / double(granules_done);
            _ns_per_granule = (1.0 - _cfg.ewma_alpha) * _ns_per_granule + _cfg.ewma_alpha * per;

            // sleep / (sleep + work) == 1 - f  ->  sleep = work * (1 - f) / f
            double sleep = double(work.count()) * (1.0 - _f) / _f;
            sleep = std::min(sleep, double(_cfg.max_sleep.count()));
            return std::chrono::nanoseconds(std::int64_t(sleep));
        }
    };
}

page_allocator::page_allocator(scavenge_policy policy) :
    _base(nullptr), _meta(nullptr), _pg_headers(nullptr), _hdrs_committed(0),
    _scavenge_idx(0), _in_use(0), _resident(0), _policy(policy) {
    // Heap and headers are one reservation, aligned so the low heap_bits of any heap address
    // are its offset within it - the masking pg_meta does to get from a pointer to a header
    _base = (uint8_t*)os::reserve(reserve_bytes, config::heap_max_sz);
    _pg_headers = (pg_meta*)(_base + config::heap_max_sz);
    _meta = os::map_rw(meta_bytes);

    if (!_base || !_meta) {
        os::release(_base, reserve_bytes);
        os::release(_meta, meta_bytes);
        throw std::bad_alloc{};
    }

    auto cursor = (uint8_t*)_meta;

    auto words = os::carve<uint64_t>(cursor, alloc_words);
    auto sums = os::carve<summary>(cursor, heap_geometry::total_entries);
    _tree = radix_tree { sums, words };

    _committed = bitmap { os::carve<uint64_t>(cursor, committed_words) };

    assert(cursor == (uint8_t*)_meta + meta_bytes && "meta_bytes disagrees with the layout");
    _scavenger = std::jthread { [&](auto st) { scavenge_loop(st); } };
}

page_allocator::~page_allocator() {
    _scavenger.request_stop();
    _scavenger.join();
    os::release(_base, reserve_bytes);
    os::release(_meta, meta_bytes);
}

bool page_allocator::commit_headers(std::size_t start, std::size_t n) {
    auto need = std::min(start + n + 1, config::total_pages);
    auto from = _hdrs_committed.load(std::memory_order_acquire);
    if (need <= from) return true;

    // Out to the chunk boundary, which is inside the reservation because it was rounded out
    // to whole chunks
    auto to = (need + hdrs_per_chunk - 1) / hdrs_per_chunk * hdrs_per_chunk;
    if (!os::commit(_pg_headers + from, (to - from) * sizeof(pg_meta))) return false;

    _hdrs_committed.store(to, std::memory_order_release);
    return true;
}

bool page_allocator::ensure_committed(std::size_t start, std::size_t n) {
    if (!commit_headers(start, n)) return false;

    auto lo = granule_of(start);
    auto end = granule_of(start + n - 1);

    while (lo <= end) {
        if (_committed.test(lo)) { ++lo; continue; }
        // Coalesce the uncommitted granules so a large run costs one syscall, not one each
        auto hi = lo;
        while (hi <= end && !_committed.test(hi)) ++hi;

        if (!os::commit(_base + lo * granule_bytes, (hi - lo) * granule_bytes))
            return false;

        _committed.set_range(lo, hi-lo);
        _resident.fetch_add(hi - lo, std::memory_order_relaxed);

        lo = hi;
    }
    return true;
}

void* page_allocator::claim_span(std::size_t start, std::size_t npages) {
    auto need_alloc = granules_unalloced(start, npages, _tree);
    _tree.mark_alloc(start, npages);

    if (!ensure_committed(start, npages)) [[unlikely]] {
        _tree.mark_free(start, npages);
        _tree.flush_free();
        return nullptr;
    }

    _in_use.fetch_add(need_alloc, std::memory_order_relaxed);
    return _base + start * config::page_sz;
}

// TODO: right now scavenge locks the mutex even though it doesn't do any operations on the tree
// (it just reads the bits) try making it lock free(?) or defer tree freeing to scavenge as well
std::size_t page_allocator::scavenge(std::size_t max_granules) {
    auto lk = std::lock_guard { _mtx };
    
    auto can_free = [&](std::size_t g) {
        return _committed.test(g) && granule_all_free(g, _tree);
    };

    std::size_t released = 0;
    auto hi = _scavenge_idx + 1;
    while (hi > 0 && released < max_granules) {
        if (!can_free(hi - 1)) { 
            --hi;
            continue;
        }
        // lo points to first nonfreeable granule
        auto lo = hi;
        while (lo > 0 && can_free(lo - 1) && (hi - lo) < (max_granules - released)) --lo;

        auto n = hi - lo;
        if (os::decommit(_base + lo * granule_bytes, n * granule_bytes)) {
            released += n;
            _resident.fetch_sub(n, std::memory_order_relaxed);
            _committed.clear_range(lo, n);
        }
        hi = lo;
    }

    _scavenge_idx = std::min(hi, _scavenge_idx);
    return released;
}

void page_allocator::scavenge_loop(std::stop_token st) {
    auto h = heuristic { _policy };
    auto mtx = std::mutex {};
    // Held across the loop; wait_for is what drops and retakes it
    auto lk = std::unique_lock { mtx };
    auto cv = std::condition_variable_any {};

    using clock = std::chrono::steady_clock;

    while (!st.stop_requested()) {
        auto now = clock::now();

        auto granules = h.plan(now, { _in_use, _resident });
        auto released = scavenge(granules);

        auto work_time = clock::now() - now;
        auto sleep = h.pace(released, work_time);

        cv.wait_for(lk, st, sleep, [&]{ return st.stop_requested(); });
    }
}

void* page_allocator::alloc(std::size_t npages) {
    if (npages == 0) return nullptr;
    auto lk = std::lock_guard{ _mtx };
    auto s = _tree.find(npages);
    return s ? claim_span(*s, npages) : nullptr;
}

void* page_allocator::alloc_aligned(std::size_t npages, std::size_t align) {
    if (npages == 0 || npages > align || align > 64 || (align & (align - 1))) return nullptr;
    auto lk = std::lock_guard{ _mtx };
    auto s = _tree.find_aligned(npages, align);
    return s ? claim_span(*s, npages) : nullptr;
}

void page_allocator::free(void* addr, std::size_t npages) {
    auto idx = page_idx(addr);
    assert(_tree.bits().all_set(idx, npages) && "freeing pages that are not allocated");

    _tree.mark_free(idx, npages);
    _in_use.fetch_sub(granules_unalloced(idx, npages, _tree), std::memory_order_relaxed);
}

pg_meta* pg_allocator::alloc_pg(size_t block_sz, size_t num_pgs) {
    auto* page = _pages.alloc(num_pgs);
    if (!page) [[unlikely]] return nullptr;

    // When allocating new pages both halves come back zeroed
    auto blocks = config::blocks_in_pg(config::sz_to_class(block_sz));
    auto alloc = _bits.alloc_bits(blocks);
    auto mark = _bits.mark_bits(blocks);
    if (alloc.empty() || mark.empty()) [[unlikely]] {
        _pages.begin_free().add(page, num_pgs);
        return nullptr;
    }

    auto pg_off = ((uint8_t*)page - heap_base()) / config::page_sz;
    auto head = new(meta_base() + pg_off) pg_meta(block_sz, alloc.data(), mark.data());
    for (std::size_t i = 1; i < num_pgs; ++i)
        pg_meta::emplace_continuation(head + i, int32_t(i), num_pgs);

    return head;
}

void pg_allocator::free_pgs(pg_meta* start) {
    auto b = _pages.begin_free();
    while (start) {
        auto* head = start;
        start = start->next();
        b.add(head->get_data(), head->num_pages());
    }
}
