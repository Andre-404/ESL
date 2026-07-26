#pragma once
#include <cstdint>
#include <cassert>
#include <span>
#include <algorithm>
#include <optional>

#include "bitmap.h"
#include "gc-config.h"

namespace gc::detail {
    // A packed summary of a contiguous region of pages:
    //   start - length of the free run at the head of the region
    //   max   - length of the longest free run anywhere in the region
    //   end   - length of the free run at the tail of the region
    //
    // They are 21 bits each, so a field can hold 0..max_packed-1. On the full heap a top
    // level entry spans exactly max_packed pages, one more than a field can count, and
    // full_bit covers that single missing value: it is set exactly when such an entry is
    // wholly free, the only time any field reaches max_packed. Every level below spans
    // fewer pages, so their fields always fit. tree_geometry asserts no level exceeds
    // max_packed.
    class summary {
    public:
        static constexpr uint32_t fanout     = 8;
        static constexpr uint32_t field_bits = 21;
        static constexpr uint32_t max_packed = 1u << field_bits;
        static constexpr uint32_t field_mask = max_packed - 1;
        static constexpr uint64_t full_bit   = 1ull << 63;

    private:
        uint64_t _data;

        static constexpr uint64_t pack(uint32_t start, uint32_t mx, uint32_t end) {
            assert(start <= max_packed && mx <= max_packed && end <= max_packed);
            // start/end reach max_packed only when the whole region is free, in
            // which case mx == max_packed too, so testing mx alone is enough
            if (mx == max_packed) return full_bit;

            return  (uint64_t(start) & field_mask)
                 | ((uint64_t(mx)    & field_mask) << field_bits)
                 | ((uint64_t(end)   & field_mask) << (2 * field_bits));
        }

        constexpr uint32_t field(uint32_t shift) const {
            return is_full() ? max_packed : uint32_t((_data >> shift) & field_mask);
        }

        static constexpr summary of_word(uint64_t x) {
            if (x == 0) return { word_bits, word_bits, word_bits };

            uint32_t start = 0, mx = 0, end = 0;
            each_free_run(x, [&](uint32_t off, uint32_t len) {
                if (off == 0)               start = len;   // run touches bit 0
                if (off + len == word_bits) end   = len;   // run touches bit 63
                mx = std::max(mx, len);
            });
            return { start, mx, end };
        }

        // Folds adjacent, equal sized child regions left to right into their parent.
        // merge() feeds a node's fanout children; summarize() feeds the 64 page words of a chunk.
        template<class R, class Get>
        static constexpr summary fold(R&& children, uint32_t cover, Get as_summary) {
            uint32_t start = 0, mx = 0, end = 0, covered = 0;
            for (auto&& c : children) {
                auto s = as_summary(c);
                if (start == covered) start += s.start();          // head grows while all prior are free
                mx  = std::max({ mx, end + s.start(), s.max() });  // carried across, or inside s
                end = (s.end() == cover) ? end + s.end() : s.end();
                covered += cover;
            }
            return { start, mx, end };
        }

    public:
        constexpr summary() : _data(0) {}
        constexpr explicit summary(uint64_t data) : _data(data) {}
        constexpr summary(uint32_t start, uint32_t mx, uint32_t end) : _data(pack(start, mx, end)) {}

        constexpr bool     is_full() const { return _data & full_bit; }
        constexpr uint32_t start()   const { return field(0); }
        constexpr uint32_t max()     const { return field(field_bits); }
        constexpr uint32_t end()     const { return field(2 * field_bits); }
        constexpr uint64_t raw()     const { return _data; }

        constexpr bool operator==(const summary&) const = default;

        static constexpr summary summarize(std::span<const uint64_t, config::chunk_words> chunk) {
            return fold(chunk, word_bits, of_word);
        }

        // Merge one parent's children. child_cover is the pages each child
        // covers; the parent covers fanout * child_cover
        static constexpr summary merge(std::span<const summary, fanout> children, uint32_t child_cover) {
            return fold(children, child_cover, [](summary s) { return s; });
        }
    };

    // The bottom level holds one summary per chunk of 512 pages
    // Each level above fans in by summary::fanout
    // Levels stop growing once an entry would span more than summary::max_packed pages,
    // because past that the packed fields can no longer represent a wholly free entry exactly
    template<std::size_t TotalPages>
    struct tree_geometry {
        static constexpr std::size_t fanout = summary::fanout;
        static constexpr std::size_t total_pages = TotalPages;
        static constexpr std::size_t chunks = TotalPages / config::chunk_pages;
        static constexpr std::size_t words = TotalPages / 64;

        static constexpr std::size_t calc_levels() {
            std::size_t n = 1, span = config::chunk_pages;
            while (span * fanout <= summary::max_packed && span * fanout <= TotalPages) {
                span *= fanout;
                n++;
            }
            return n;
        }
        static constexpr std::size_t levels = calc_levels();

        static constexpr std::size_t pages_at(std::size_t l) {
            std::size_t span = config::chunk_pages;
            for (std::size_t i = levels - 1; i > l; --i) span *= fanout;
            return span;
        }
        static constexpr auto level_pages = []{
            std::array<std::size_t, levels> a{};
            for (std::size_t i = 0; i < levels; ++i) a[i] = pages_at(i);
            return a;
        }();
        static constexpr auto level_entries = []{
            std::array<std::size_t, levels> a{};
            for (std::size_t i = 0; i < levels; ++i) a[i] = TotalPages / pages_at(i);
            return a;
        }();
        // Every level lives in one flat array; this is where each one starts.
        static constexpr auto level_offset = []{
            std::array<std::size_t, levels> a{};
            std::size_t acc = 0;
            for (std::size_t i = 0; i < levels; ++i) { a[i] = acc; acc += level_entries[i]; }
            return a;
        }();
        static constexpr std::size_t total_entries = level_offset[levels - 1] + level_entries[levels - 1];

        static_assert(TotalPages % config::chunk_pages == 0);
        // A top level entry may not span more than a summary field can count - see the
        // comment on summary. A heap small enough to stop growing levels early spans less,
        // which is fine: full_bit is only needed at exactly max_packed.
        static_assert(level_pages[0] <= summary::max_packed);
        // The bottom level is one summary per chunk, which is what resummarize and
        // find_in_chunk both index by.
        static_assert(level_entries[levels - 1] == chunks);
    };

    using heap_geometry = tree_geometry<config::total_pages>;

    // Go's mpagealloc radix tree: a bit per page, summarised bottom-up so a run of N free
    // pages can be located without scanning the bitmap. Storage is owned by the caller
    // Not thread safe
    template<typename Geom>
    class radix_tree_t {
        summary* _sums;
        bitmap _alloc;
        std::size_t _dirty_lo;
        std::size_t _dirty_hi;

        // Lets tests reach the private *_in_chunk helpers without widening the public API.
        template<typename> friend struct radix_tree_test_peer;

        summary* level(std::size_t l) const { return _sums + Geom::level_offset[l]; }

        struct scan_result {
            enum kind {
                found,
                descend,
                exhausted
            };
            kind what;
            std::size_t value;
        };

        // Walks `window` entries of level l from blk, carrying a free run across them.
        scan_result scan_level(std::size_t l, std::size_t blk, std::size_t window, std::size_t npages) const {
            const auto lv = level(l);
            const auto span = Geom::level_pages[l];
            std::size_t run = 0, run_start = 0;

            for (std::size_t j = 0; j < window; ++j) {
                auto s = lv[blk + j];
                auto entry_base = (blk + j) * span;
                // Either a run carried in from the left, or a fresh one starting here.
                auto head = run ? run_start : entry_base;

                if (run + s.start() >= npages) return { scan_result::found, head };
                // max() means the run lies wholly inside this entry, so whatever was
                // carried in is irrelevant below and the descent may start fresh
                if (s.max() >= npages) return { scan_result::descend, blk + j };

                if (s.end() == span) { run_start = head; run += span; }   // wholly free
                else { run = s.end(); run_start = entry_base + span - s.end(); }
            }
            return { scan_result::exhausted, 0 };
        }

        // Only checks max because an aligned run can't span chunk boundaries
        // However a run of n pages might not be aligned so dont early exit
        std::optional<std::size_t> descend_aligned(size_t l, size_t idx, size_t n, size_t align) const {
            if (level(l)[idx].max() < n) return std::nullopt;

            if (l + 1 == Geom::levels) return find_aligned_in_chunk(idx, n, align);

            for (std::size_t j = 0; j < Geom::fanout; ++j) {
                if (auto r = descend_aligned(l + 1, idx * Geom::fanout + j, n, align))
                    return r;
            }
            
            return std::nullopt;
        }

        // Runs that cross a chunk boundary are caught a level up, where the
        // start/end summaries make them visible
        std::optional<std::size_t> find_in_chunk(std::size_t chunk_idx, std::size_t n) const {
            auto base = chunk_idx * config::chunk_pages;
            std::size_t run = 0, run_start = 0, prev_end = base;
            std::optional<std::size_t> hit;

            for (auto w : _alloc.chunk(chunk_idx)) {
                each_free_run(w, [&](uint32_t off, uint32_t len) {
                    auto pg = base + off;
                    if (pg != prev_end) run = 0;
                    if (run == 0) run_start = pg;

                    run += len;
                    prev_end = pg + len;

                    if (run >= n && !hit.has_value()) hit = run_start;
                });
                if (hit) return hit;
                base += word_bits;
            }
            return std::nullopt;
        }

        // Precondition(guaranteed by find_aligned): n <= align, align a power of two <= 64
        std::optional<std::size_t> find_aligned_in_chunk(std::size_t chunk_idx, std::size_t n, std::size_t align) const {
            const auto base = chunk_idx * config::chunk_pages;
            const auto w = _alloc.chunk(chunk_idx);
            // A group is free when its low n bits are clear.
            const uint64_t need = (n == word_bits) ? ~uint64_t(0) : ((uint64_t(1) << n) - 1);

            for (std::size_t i = 0; i < config::chunk_words; ++i) {
                auto x = w[i];
                if (x == ~uint64_t(0)) continue;
                for (std::size_t bit = 0; bit < word_bits; bit += align)
                    if ((x >> bit & need) == 0) return base + i * word_bits + bit;
            }
            return std::nullopt;
        }

        void init_all_free() {
            for (std::size_t l = 0; l < Geom::levels; ++l) {
                auto span = uint32_t(Geom::level_pages[l]);
                for (std::size_t i = 0; i < Geom::level_entries[l]; ++i)
                    level(l)[i] = summary { span, span, span };
            }
        }

        void resummarize(std::size_t lo_p, std::size_t hi_p) {
            auto lo = lo_p / config::chunk_pages;
            auto hi = (hi_p - 1) / config::chunk_pages;

            auto* leaf = level(Geom::levels - 1);
            for (auto c = lo; c <= hi; ++c)
                leaf[c] = summary::summarize(_alloc.chunk(c));

            for (std::size_t l = Geom::levels - 1; l-- > 0;) {
                lo /= Geom::fanout;
                hi /= Geom::fanout;

                auto* parent = level(l);
                const auto* child = level(l + 1);
                // At every level update every affected node
                // Large memory requests can span multiple nodes at each level
                for (auto p = lo; p <= hi; ++p) {
                    parent[p] = summary::merge(
                        std::span<const summary, summary::fanout> { child + p * Geom::fanout, summary::fanout },
                        Geom::level_pages[l + 1]
                    );
                }
            }
        }

    public:
        using geometry = Geom;

        radix_tree_t() : _sums(nullptr), _alloc(), _dirty_lo(~std::size_t(0)), _dirty_hi(0) {}
        radix_tree_t(summary* sums, uint64_t* words) : _sums(sums), _alloc(words), _dirty_lo(~std::size_t(0)), _dirty_hi(0) {
            init_all_free();
        }

        const bitmap& bits() const { return _alloc; }

        // First fit
        std::optional<std::size_t> find(std::size_t npages) const {
            assert(_dirty_hi == 0 && "unflushed frees");
            if (npages == 0 || npages > Geom::total_pages) return std::nullopt;

            // Level 0 has many roots, so it is scanned as one wide window and is the only level
            // that can conclude "nothing fits" - below it the parent's max() promised a run
            auto r = scan_level(0, 0, Geom::level_entries[0], npages);
            for (std::size_t l = 0; r.what == scan_result::descend; ++l) {
                if (l + 1 == Geom::levels) return find_in_chunk(r.value, npages);
                r = scan_level(l + 1, r.value * Geom::fanout, Geom::fanout, npages);
            }
            return r.what == scan_result::found ? std::optional { r.value } : std::nullopt;
        }

        // An aligned run always fits inside one bitmap word - it can never straddle a word
        // (nor, since 64 | chunk_pages, a chunk) boundary
        // max is a loose filter so a subtree passing it may be empty and the walk continues
        std::optional<std::size_t> find_aligned(std::size_t n, std::size_t align) const {
            assert(_dirty_hi == 0 && "unflushed frees");
            if(n == 0 || n > align || align > 64 || (align & (align - 1))) return std::nullopt;
            for (std::size_t i = 0; i < Geom::level_entries[0]; ++i)
                if (auto r = descend_aligned(0, i, n, align)) return r;
            return std::nullopt;
        }

        void mark_alloc(std::size_t start, std::size_t n) {
            _alloc.set_range(start, n);
            resummarize(start, start + n);
        }

        void mark_free(std::size_t start, std::size_t n) {
            _alloc.clear_range(start, n);
            _dirty_lo = std::min(_dirty_lo, start);
            _dirty_hi = std::max(_dirty_hi, start + n);
        }

        void flush_free() {
            if (_dirty_hi > _dirty_lo) resummarize(_dirty_lo, _dirty_hi);
            _dirty_lo = ~std::size_t(0);
            _dirty_hi = 0;
        }
        
    };

    using radix_tree = radix_tree_t<heap_geometry>;
}