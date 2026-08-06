#pragma once
#include "gc-config.h"
#include "managed.h"
#include <atomic>
#include <cassert>
#include <limits>
#include <span>
#include <utility>

namespace gc::detail {
    class dual_bitmap {
        uint64_t* _alloc;
        uint64_t* _mark;
    public:
        dual_bitmap(uint64_t* alloc, uint64_t* mark) : _alloc(alloc), _mark(mark) {}

        void flip(uint64_t* new_mark) {
            _alloc = _mark;
            _mark = new_mark;
        }
        // Bitmap aligned to 8 bytes to make atomic ops and popcnt easier
        std::span<size_t> mark_bits(size_t block_cnt) const {
            return { _mark, (block_cnt + 63) / 64 };
        }

        // Alloc bits are written by the owning thread (obj_allocator's cache flush) and read
        // concurrently by the collector's conservative stack scan, so they are only reachable
        // through these two
        size_t load_alloc(size_t bit) const {
            return std::atomic_ref { _alloc[bit / 64] }.load(std::memory_order_acquire);
        }
        void store_alloc(size_t bit, size_t val) const {
            std::atomic_ref { _alloc[bit / 64] }.store(val, std::memory_order_release);
        }
    };

    class pg_meta {
        static constexpr size_t region_mask = ~((1ull << config::heap_bits) - 1);
        static constexpr int32_t sll_null = std::numeric_limits<int32_t>::max();
        enum flags : uint8_t {
            first_in_run = 1,
            has_cont = 1 << 1,
        };

        std::atomic<int32_t> _next;
        const uint8_t _szclass;
        const uint8_t _flags;
        std::atomic<bool> _has_pinned;
        std::atomic<bool> _active;
        // Dirty, but allows us to save on size since continuations don't use the bitmap
        union {
            dual_bitmap _bits;
            size_t _run_pages;
        };

        bool has_flag(flags flag) const { return (_flags & flag) != 0; }
        static constexpr uint8_t hdr_flags(size_t block_sz) {
            return flags::first_in_run | (block_sz > config::page_sz ? flags::has_cont : 0);
        }

        std::pair<std::atomic_ref<size_t>, uint64_t> mark_at(size_t i) const {
            return {
                std::atomic_ref { _bits.mark_bits(block_cnt())[i / 64] },
                1ull << (i % 64)
            };
        }

        // Headers sit above the heap's 2^heap_bits boundary
        pg_meta* hdr_base() const { return (pg_meta*)((size_t)this & region_mask); }

        // For pages following the header
        explicit pg_meta(int32_t offset, size_t num_pages)  : _next(-offset), _szclass(0),
            _flags(0), _has_pinned(false), _active(true), _run_pages(num_pages) {}

    public:
        explicit pg_meta(size_t block_sz, uint64_t* alloc, uint64_t* mark) : _next(sll_null),
            _szclass(config::sz_to_class(block_sz)), _flags(hdr_flags(block_sz)),
            _has_pinned(false), _active(true), _bits(alloc, mark)
        {
            // Pages for large objects are created when an object of that size is needed,
            // thus creating a large obj page == allocating large obj
            if (_szclass == config::large_class) _bits.store_alloc(0, 1);
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
            return pg->has_flag(flags::first_in_run) ? pg : pg->next();
        }

        class pg_slots_iter {
            const pg_meta* _pg;
            uint16_t _i;
        public:
            pg_slots_iter(pg_meta* pg, uint16_t i) : _pg(pg), _i(i) {}

            void next() { _i++; }
            bool at_end() const { return _i == _pg->block_cnt(); }

            managed* get() const {
                return at_end() ? nullptr : (managed*)(_pg->get_data() + _i * _pg->block_sz());
            }

            bool is_marked() const {
                auto [word, in_word] = _pg->mark_at(_i);
                return word.load(std::memory_order_acquire) & in_word;
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
            if (!has_flag(flags::has_cont)) return 1;
            // has_cont is set only for runs of > 1 page, so this deref is safe
            return (this + 1)->_run_pages;
        }
        uint8_t* get_data() const {
            return (uint8_t*)hdr_base() - config::heap_max_sz + (this - hdr_base()) * config::page_sz;
        }
        bool has_pinned() const { return _has_pinned.load(std::memory_order_relaxed); }

        void next_cycle(uint64_t* new_mark) {
            _has_pinned.store(false, std::memory_order_relaxed);
            _bits.flip(new_mark);
        }

        // Marking stuff
        [[gnu::hot]] bool record_mark(managed* ptr, bool is_pinned) {
            // Pin regardless of the fact that the mark bit is set or not
            if (is_pinned) _has_pinned.store(true, std::memory_order_relaxed);

            auto [word, in_word] = mark_at((size_t)((uint8_t*)ptr - get_data()) / block_sz());
            return (word.fetch_or(in_word, std::memory_order_relaxed) & in_word) == 0;
        }
        // Precondition: interior is in the page
        managed* from_interior(uint8_t* interior) const {
            auto data = get_data();
            auto idx = _szclass == config::large_class ? 0 : (size_t)(interior - data) / block_sz();
            if (idx >= block_cnt() || !(_bits.load_alloc(idx) & (1ull << idx % 64))) return nullptr;
            return (managed*)(data + idx * block_sz());
        }
        size_t compute_live() const {
            auto n = 0ull;
            for (auto w : _bits.mark_bits(block_cnt())) n += std::popcount(w);
            return n;
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
}
