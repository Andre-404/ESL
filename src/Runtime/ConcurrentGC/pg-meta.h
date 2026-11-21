#pragma once
#include "gc-config.h"
#include "managed.h"
#include "allocator/heap-layout.h"
#include <algorithm>
#include <atomic>
#include <bit>
#include <cassert>
#include <limits>
#include <span>
#include <utility>

namespace gc::detail {
    class dual_bitmap {
        uint32_t _alloc;
        uint32_t _mark;

        uint64_t* base() const { return (uint64_t*)heap_layout::bits(heap_layout::heap_from_hdr(this)); }
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

    class pg_meta {
    public:
        enum pg_flag : uint8_t {
            // Shape of the page, written once by the constructor and never again
            f_first_in_run = 1,
            f_has_cont     = 1 << 1,
            // Role and state for the cycle in flight, reset by next_cycle
            f_pinned       = 1 << 2, // holds an object that must not move
            f_active       = 1 << 3, // This page is active
        };

    private:
        static constexpr int32_t sll_null = std::numeric_limits<int32_t>::max();

        std::atomic<int32_t> _next;
        const uint8_t _szclass;
        std::atomic<uint8_t> _flags;
        // Dirty, but allows us to save on size since continuations don't use the bitmap
        union {
            dual_bitmap _bits;
            size_t _run_pages;
        };

        static constexpr uint8_t hdr_flags(size_t block_sz) {
            return f_first_in_run | (block_sz > config::page_sz ? f_has_cont : 0);
        }

        bool test_flag(pg_flag f, std::memory_order mo = std::memory_order_relaxed) const { return (_flags.load(mo) & f) != 0; }
        void set_flag(pg_flag f, std::memory_order mo = std::memory_order_relaxed) { _flags.fetch_or(f, mo); }
        void clear_flag(pg_flag f, std::memory_order mo = std::memory_order_relaxed) { _flags.fetch_and(uint8_t(~f), mo); }

        std::pair<std::atomic_ref<size_t>, uint64_t> mark_at(size_t i) const {
            return {
                std::atomic_ref { _bits.mark_bits(block_cnt())[i / 64] },
                1ull << (i % 64)
            };
        }
        // Only works if ptr isn't an interior pointer
        size_t block_idx(managed* ptr) const {
            assert(_szclass < config::large_class || (size_t)((uint8_t*)ptr - get_data()) == 0);
            return (size_t)((uint8_t*)ptr - get_data()) / block_sz_fast();
        }

        // Not correct for large objects but it doesn't matter
        // for the purposes of from_interior and record_mark
        size_t block_sz_fast() const { return config::sz_classes[_szclass]; }

        // For pages following the header
        explicit pg_meta(int32_t offset, size_t num_pages)  : _next(-offset), _szclass(0),
            _flags(0), _run_pages(num_pages) {}

    public:
        explicit pg_meta(size_t block_sz, uint64_t* alloc, uint64_t* mark) : _next(sll_null),
            _szclass(config::sz_to_class(block_sz)), _flags(hdr_flags(block_sz)), _bits(alloc, mark)
        {
            // Large obj pages are handed out with their alloc bit clear on purpose, the object
            // isn't constructed yet. arena publishes it once it is (see arena::publish_big)
        }

        static void emplace_continuation(pg_meta* at, int32_t off, size_t num_pages) {
            new(at) pg_meta(off, num_pages);
        }

        static pg_meta* pg_from_ptr(void* ptr) { return heap_layout::hdrs(ptr) + heap_layout::page_idx(ptr); }
        static pg_meta* head_from_ptr(void* ptr) {
            auto pg = pg_from_ptr(ptr);
            return pg->test_flag(f_first_in_run) ? pg : pg->next();
        }

        void set_active(bool active) {
            for (size_t i = 0; i < num_pages(); i++)
                active ? (this + i)->set_flag(f_active, std::memory_order_release)
                       : (this + i)->clear_flag(f_active, std::memory_order_release);
        }
        bool is_active() const { return test_flag(f_active, std::memory_order_acquire); }

        // Getters
        uint16_t block_cnt() const { return config::blocks_in_pg(_szclass); }
        uint64_t block_sz() const {
            return _szclass == config::large_class ? num_pages() * config::page_sz : config::sz_classes[_szclass];
        }
        uint8_t szclass() const { return _szclass; }
        size_t num_pages() const {
            if (!test_flag(f_has_cont)) return _szclass == config::large_class ? 1 : config::small_obj_pgs;
            // f_has_cont is set only for runs of > 1 page, so this deref is safe
            return (this + 1)->_run_pages;
        }
        uint8_t* get_data() const {
            auto heap = heap_layout::heap_from_hdr(this);
            return heap + (this - heap_layout::hdrs(heap)) * config::page_sz;
        }
        bool has_pinned() const { return test_flag(f_pinned); }

        void next_cycle(uint64_t* new_mark) {
            clear_flag(f_pinned);
            _bits.flip(new_mark);
        }

        // Marking stuff
        [[gnu::hot]] bool record_mark(managed* ptr, bool is_pinned) {
            // Pin regardless of the fact that the mark bit is set or not
            if (is_pinned) set_flag(f_pinned);

            auto [word, in_word] = mark_at(block_idx(ptr));
            if (word.load(std::memory_order_relaxed) & in_word) return false;
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
        // Calls fn with the index of every marked block, in ascending order, a word at a time
        template<typename F>
        void for_each_marked(F&& fn) const {
            auto words = _bits.mark_bits(block_cnt());
            for (size_t w = 0; w < words.size(); ++w)
                for (auto m = words[w]; m; m &= m - 1) fn(uint16_t(w * 64 + std::countr_zero(m)));
        }
        std::span<const size_t> mark_words() const { return _bits.mark_bits(block_cnt()); }
        // Leaves exactly the first n blocks marked
        void mark_prefix(size_t n) {
            for (auto& w : _bits.mark_bits(block_cnt())) {
                w = n >= 64 ? ~0ull : (1ull << n) - 1;
                n -= std::min<size_t>(n, 64);
            }
        }
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
