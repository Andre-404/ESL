#pragma once
#include "gc-config.h"
#include "managed.h"
#include <algorithm>
#include <atomic>
#include <cassert>
#include <limits>
#include <span>
#include <utility>

namespace gc::detail {
    class dual_bitmap {
        uint32_t _alloc;
        uint32_t _mark;

        uint64_t* base() const {
            auto meta_base = (size_t)this & ~((1ull << config::heap_bits) - 1);
            return (uint64_t*)(meta_base + config::hdr_region_sz);
        }
        uint32_t compute_offset(uint64_t* bitmap) const { return bitmap - base(); }
        uint64_t* compute_ptr(uint32_t offset) const { return base() + offset; }
    public:
        dual_bitmap(uint64_t* alloc, uint64_t* mark)
            : _alloc(compute_offset(alloc)), _mark(compute_offset(mark)) {}

        void flip(uint64_t* new_mark) {
            _alloc = _mark;
            _mark = compute_offset(new_mark);
        }
        // Bitmap aligned to 8 bytes to make atomic ops and popcnt easier
        std::span<size_t> mark_bits(size_t block_cnt) const {
            return { compute_ptr(_mark), (block_cnt + 63) / 64 };
        }

        // Alloc bits are written by the owning thread (obj_allocator's cache flush) and read
        // concurrently by the collector's conservative stack scan, so they are only reachable
        // through these two
        size_t load_alloc(size_t bit) const {
            return std::atomic_ref { compute_ptr(_alloc)[bit / 64] }.load(std::memory_order_acquire);
        }
        void store_alloc(size_t bit, size_t val) const {
            std::atomic_ref { compute_ptr(_alloc)[bit / 64] }.store(val, std::memory_order_release);
        }
    };

    class pg_history {
        uint16_t _word;
    public:
        static constexpr uint16_t live_max = (1u << config::pg_live_bits) - 1;
        static constexpr uint8_t  age_max  = uint8_t(config::pg_age_cnt - 1);

        pg_history() : _word(0) {}
        pg_history(uint16_t occ, uint8_t age)
            : _word(uint16_t(std::min(occ, live_max) | (std::min(age, age_max) << config::pg_live_bits)))
            {}

        uint16_t occ() const { return _word & live_max; }
        uint8_t age() const { return uint8_t(_word >> config::pg_live_bits); }
        // Zero count can only mean no cycle has recorded this page yet
        bool known() const { return occ() != 0; }
    };
    static_assert(sizeof(pg_history) == config::history_entry_sz);

    class pg_meta {
    public:
        enum pg_flag : uint8_t {
            // Shape of the page, written once by the constructor and never again
            f_first_in_run = 1,
            f_has_cont     = 1 << 1,
            // Role and state for the cycle in flight, reset by next_cycle
            f_pinned       = 1 << 2, // holds an object that must not move
            f_source       = 1 << 3, // nominated for evacuation; clear means the page is a target
            f_any_dirty    = 1 << 4, // at least one card bit is set, so the update pass must look

            cycle_flags = f_pinned | f_source | f_any_dirty,
        };

    private:
        static constexpr size_t region_mask = ~((1ull << config::heap_bits) - 1);
        static constexpr int32_t sll_null = std::numeric_limits<int32_t>::max();

        std::atomic<int32_t> _next;
        const uint8_t _szclass;
        std::atomic<uint8_t> _flags;
        std::atomic<bool> _active;
        // Dirty, but allows us to save on size since continuations don't use the bitmap
        union {
            dual_bitmap _bits;
            size_t _run_pages;
        };

        static constexpr uint8_t hdr_flags(size_t block_sz) {
            return f_first_in_run | (block_sz > config::page_sz ? f_has_cont : 0);
        }

        bool test_flag(pg_flag f) const { return (_flags.load(std::memory_order_relaxed) & f) != 0; }
        void set_flag(pg_flag f) { _flags.fetch_or(f, std::memory_order_relaxed); }
        void clear_flag(pg_flag f) { _flags.fetch_and(uint8_t(~f), std::memory_order_relaxed); }
        void pin() { _flags.fetch_or(f_pinned, std::memory_order_relaxed); }

        std::pair<std::atomic_ref<size_t>, uint64_t> mark_at(size_t i) const {
            return {
                std::atomic_ref { _bits.mark_bits(block_cnt())[i / 64] },
                1ull << (i % 64)
            };
        }

        // Headers sit above the heap's 2^heap_bits boundary
        pg_meta* hdr_base() const { return (pg_meta*)((size_t)this & region_mask); }

        pg_history* history_slot() const {
            auto base = (size_t)hdr_base() + config::hdr_region_sz
                      + config::bits_region_sz + config::card_region_sz;
            return (pg_history*)base + (this - hdr_base());
        }

        // Not correct for large objects but it doesn't matter
        // for the purposes of from_interior, record_mark and pg_slots_iter::is_marked
        size_t block_sz_fast() const { return config::sz_classes[_szclass]; }

        // For pages following the header
        explicit pg_meta(int32_t offset, size_t num_pages)  : _next(-offset), _szclass(0),
            _flags(0), _active(true), _run_pages(num_pages) {}

    public:
        explicit pg_meta(size_t block_sz, uint64_t* alloc, uint64_t* mark) : _next(sll_null),
            _szclass(config::sz_to_class(block_sz)), _flags(hdr_flags(block_sz)),
            _active(false), _bits(alloc, mark)
        {
            *history_slot() = pg_history {};
            // Init bits before publishing this page as active
            _active.store(true, std::memory_order_release);
            // Large obj pages are handed out with their alloc bit clear on purpose, the object
            // isn't constructed yet. arena publishes it once it is (see arena::publish_big)
        }

        static void emplace_continuation(pg_meta* at, int32_t off, size_t num_pages) {
            new(at) pg_meta(off, num_pages);
        }

        static pg_meta* pg_from_ptr(void* ptr) {
            auto meta_start = (pg_meta*)(((size_t)ptr & region_mask) + config::heap_max_sz);
            return meta_start + (((size_t)ptr & ~region_mask) / config::page_sz);
        }
        static pg_meta* head_from_ptr(void* ptr) {
            auto pg = pg_from_ptr(ptr);
            return pg->test_flag(f_first_in_run) ? pg : pg->next();
        }

        class pg_slots_iter {
            const pg_meta* _pg;
            size_t _block_cnt;
            size_t _block_sz;
            size_t _cache;
            uint16_t _i;
        public:
            pg_slots_iter(pg_meta* pg, uint16_t i)
                : _pg(pg), _block_cnt(_pg->block_cnt()), _block_sz(_pg->block_sz_fast()),
                  _cache(_pg->_bits.mark_bits(_block_cnt)[i / 64]), _i(i) {}

            void next() {
                if (++_i % 64 == 0)
                    _cache = _pg->_bits.mark_bits(_block_cnt)[_i / 64];
            }
            bool at_end() const { return _i == _block_cnt; }

            managed* get() const {
                return at_end() ? nullptr : (managed*)(_pg->get_data() + _i * _block_sz);
            }

            bool is_marked() const { return _cache & (1ull << (_i % 64)); }
            void set_marked() { 
                _cache |= (1ull << (_i % 64));
                _pg->_bits.mark_bits(_block_cnt)[_i / 64] = _cache;
            }
        };

        void mark_inactive() {
            for (size_t i = 0; i < num_pages(); i++)
                (this + i)->_active.store(false, std::memory_order_release);
        }
        bool is_active() const { return _active.load(std::memory_order_acquire); }

        // Getters
        uint16_t block_cnt() const { return config::blocks_in_pg(_szclass); }
        uint64_t block_sz() const {
            return _szclass == config::large_class ? num_pages() * config::page_sz : config::sz_classes[_szclass];
        }
        uint8_t szclass() const { return _szclass; }
        size_t num_pages() const {
            if (!test_flag(f_has_cont)) return 1;
            // f_has_cont is set only for runs of > 1 page, so this deref is safe
            return (this + 1)->_run_pages;
        }
        uint8_t* get_data() const {
            return (uint8_t*)hdr_base() - config::heap_max_sz + (this - hdr_base()) * config::page_sz;
        }
        bool has_pinned() const { return test_flag(f_pinned); }
        // Instead of doing the "pin clears source" dance which slows down the hot path
        // we say that pinned dominates source
        bool is_source() const {
            auto f = _flags.load(std::memory_order_relaxed);
            return (f & f_source) && ((f & f_pinned) == 0);
        }
        bool any_dirty() const { return test_flag(f_any_dirty); }

        bool nominate() {
            auto prev = _flags.fetch_or(f_source, std::memory_order_relaxed);
            return (prev & f_source) == 0 && (prev & f_pinned) == 0;
        }
        void demote() { clear_flag(f_source); }
        void set_dirty() { set_flag(f_any_dirty); }

        void next_cycle(uint64_t* new_mark) {
            clear_flag(cycle_flags);
            _bits.flip(new_mark);
        }

        // Marking stuff
        [[gnu::hot]] bool record_mark(managed* ptr, bool is_pinned) {
            // Pin regardless of the fact that the mark bit is set or not
            if (is_pinned) pin();

            auto [word, in_word] = mark_at((size_t)((uint8_t*)ptr - get_data()) / block_sz_fast());
            return (word.fetch_or(in_word, std::memory_order_relaxed) & in_word) == 0;
        }
        // Precondition: interior is in the page
        managed* from_interior(uint8_t* interior) const {
            auto data = get_data();
            // large class branch needed here because block_sz_fast returns 1 for large objects
            auto idx = _szclass == config::large_class ? 0 : (size_t)(interior - data) / block_sz_fast();
            if (idx >= block_cnt() || !(_bits.load_alloc(idx) & (1ull << idx % 64))) return nullptr;
            return (managed*)(data + idx * block_sz());
        }
        size_t compute_live() const {
            auto n = 0ull;
            for (auto w : _bits.mark_bits(block_cnt())) n += std::popcount(w);
            return n;
        }
        size_t compute_alloc() const {
            auto n = 0ull;
            for (size_t bit = 0; bit < block_cnt(); bit += 64) n += std::popcount(_bits.load_alloc(bit));
            return n;
        }

        pg_history history() const { return *history_slot(); }
        void set_history(pg_history h) { *history_slot() = h; }
        uint64_t load_alloc_word(uint16_t bit) const  { return _bits.load_alloc(bit); }
        void     store_alloc_word(uint16_t bit, uint64_t w) const { _bits.store_alloc(bit, w); }

        // Instrusive SLL
        pg_meta* next() {
            auto off = _next.load(std::memory_order_acquire);
            return off == sll_null ? nullptr : this + off;
        }
        void link(pg_meta* next) {
            _next.store(next ? int32_t(next - this) : sll_null, std::memory_order_release);
        }
        void unlink() { link(nullptr); }
    };
    static_assert(sizeof(pg_meta) == config::hdr_entry_sz);
}
