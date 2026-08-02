#pragma once
#include "gc-config.h"
#include "managed.h"
#include "tstack.h"
#include <atomic>
#include <memory>
#include <cstring>
#include <algorithm>
#include <cassert>
#include <span>

namespace gc::detail {
    class dual_bitmap {
        // Padded to 8bytes
        const uint16_t _bitmap_size;
        uint16_t _isflipped;

        uint64_t* alloc_bits() const {
            uint8_t* ptr = nullptr;
            if (_isflipped) ptr = (uint8_t*)this + sizeof(dual_bitmap) + _bitmap_size;
            else ptr = (uint8_t*)this + sizeof(dual_bitmap);
            return (uint64_t*)ptr;
        }
    public:
        dual_bitmap() : _bitmap_size(0), _isflipped(false) {}
        dual_bitmap(uint16_t _bitmap_sz) : _bitmap_size(_bitmap_sz), _isflipped(false) {}

        std::span<size_t> mark_bits() const {
            uint8_t* ptr = nullptr;
            if (_isflipped) ptr = (uint8_t*)this + sizeof(dual_bitmap);
            else ptr = (uint8_t*)this + sizeof(dual_bitmap) + _bitmap_size;
            return { (size_t*)ptr, (size_t)(_bitmap_size / 8) };
        }
        void flip() { _isflipped = !_isflipped; }
        void clear_mark() const {
            auto ptr = std::assume_aligned<8>(mark_bits().data());
            memset(ptr, 0, _bitmap_size);
        }
        void clear_both() {
            auto ptr = std::assume_aligned<8>((uint8_t*)this + sizeof(dual_bitmap));
            memset(ptr, 0, _bitmap_size*2);
        }

        // Alloc bits are written by the owning thread (obj_allocator's cache flush) and read
        // concurrently by the collector's conservative stack scan, so they are only reachable
        // through these two. Relaxed is enough: a reader that misses a not-yet-flushed bit
        // concludes "slot not allocated", which is exactly what the pre-flush window means
        // everywhere else in the allocator.
        size_t load_alloc(size_t bit) const {
            return std::atomic_ref { alloc_bits()[bit / 64] }.load(std::memory_order_acquire);
        }
        void store_alloc(size_t bit, size_t val) const {
            std::atomic_ref { alloc_bits()[bit / 64] }.store(val, std::memory_order_release);
        }
    };
    
    inline size_t large_pg_num(size_t obj_sz) {
        return (obj_sz + config::page_sz - 1) / config::page_sz;
    }

    class pg_meta : public tnode<pg_meta> {
        // block_sz must be able to hold large objects so its size_t
        const size_t _block_sz;
        const uint16_t _block_cnt;
        const uint16_t _slot_start;
        uint16_t _num_live;
        std::atomic<uint8_t> _has_pinned;
        // Padding is here because dual_bitmap assumes its bitmap comes after the class itself
        char _[5];
        dual_bitmap _bitmap;

        static constexpr auto class_to_slot_n = []() constexpr {
            auto arr = std::array<uint16_t, config::szclass_cnt> {};
            for (size_t i = 0; i < config::szclass_cnt; ++i) {
                auto max_n = 0;
                auto b = 1;
                while (std::min(64ul*b, (config::page_sz - 32 - 16 * b) / config::sz_classes[i]) > max_n) {
                    max_n = std::min(64ul*b, (config::page_sz - 32 - 16 * b) / config::sz_classes[i]);
                    b++;
                }
                arr[i] = max_n;
            }
            return arr;
        }();

        static constexpr auto block_cnt(size_t sz) {
            return config::sz_to_class(sz) == config::large_class ? (uint16_t)1 : class_to_slot_n[config::sz_to_class(sz)];
        }
        static constexpr uint16_t bitmap_sz(size_t cnt) { return (uint16_t)((cnt + 63) / 64 * 8); }

        // Shrinking page_sz is what can push a size class off the end of a page, and nothing
        // else catches it: block_cnt/slot_start are constexpr, so a bad geometry only shows up
        // as slots running past the page at runtime.
        static_assert([]() constexpr {
            for (std::size_t i = 0; i < config::szclass_cnt; ++i) {
                auto sz  = config::sz_classes[i];
                auto cnt = class_to_slot_n[i];
                if (cnt == 0) return false;
                auto start = 32 + 2 * ((std::size_t(cnt) + 63) / 64 * 8);
                if (start + std::size_t(cnt) * sz > config::page_sz) return false;
            }
            return true;
        }(), "a size class does not fit in a page: check config::page_sz vs sz_classes");

        uint8_t* get_data() const { return (uint8_t*)(this) + _slot_start; }
        void set_pinned() { _has_pinned.store(true, std::memory_order_relaxed); }
    public:
        explicit pg_meta(size_t block_sz) : _block_sz(block_sz), _block_cnt(block_cnt(block_sz)), _slot_start(32 + 2 * bitmap_sz(_block_cnt)),
                                            _num_live(0), _has_pinned(false), _{},
                                            _bitmap(bitmap_sz(_block_cnt)) {
            // Pages for large objects are created when an object of that size is needed, thus creating a large obj page == allocating large obj
            if (config::sz_to_class(_block_sz) == config::large_class) _bitmap.store_alloc(0, 1);
        }

        class pg_slots_iter {
            const pg_meta* _pg;
            uint16_t _i;
        public:
            pg_slots_iter(pg_meta* pg, uint16_t i) : _pg(pg), _i(i) {}

            void next() {
                _i++;
            }

            bool at_end() const {
                return _i == _pg->block_cnt();
            }

            managed* get() const {
                return at_end() ? nullptr : (managed*)(_pg->get_data() + _i * _pg->_block_sz);
            }

            bool is_marked() const {
                auto [word, in_word] = std::pair { _i / 64, 1ull << (_i % 64) };
                auto ref = std::atomic_ref { _pg->_bitmap.mark_bits()[word] };
                return ref.load(std::memory_order_acquire) & in_word;
            }
        };

        managed* from_interior(uint8_t* interior) const {
            auto diff = interior - (uint8_t*)this;
            if (diff < _slot_start || diff >= (_slot_start + block_cnt() * _block_sz)) return nullptr;
            diff -= _slot_start;
            auto ptr = (managed*)(interior - diff % _block_sz);

            auto idx = (size_t)((uint8_t*)ptr - get_data()) / _block_sz;
            if (_bitmap.load_alloc(idx) & (1ull << idx % 64)) return ptr;
            return nullptr;
        }

        static constexpr std::size_t header_bytes(size_t block_sz) {
            return 32 + 2 * bitmap_sz(block_cnt(block_sz));
        }
        size_t block_sz() const { return _block_sz; }
        uint16_t block_cnt() const { return _block_cnt; }
        // Anything past the largest size class gets a page of its own, sized to the object.
        bool is_large() const { return config::sz_to_class(_block_sz) == config::large_class; }
        // How many pages back this pg_meta. The single source of truth for what the page
        // allocator handed out, and so for what it must take back.
        // TODO: need to change this if we're gonna have a multipage buffer for small objects
        size_t num_pages() const { return is_large() ? large_pg_num(_block_sz) : 1; }
        uint16_t start_off() const { return _slot_start; }
        uint16_t live_count() const { return _num_live; }
        bool has_pinned() const { return _has_pinned.load(std::memory_order_relaxed); }
        void reset_trackers() {
            _num_live = 0;
            _has_pinned = false;
            _bitmap.flip();
        }
        void recycle() {
            _num_live = 0;
            _has_pinned = false;
            // Only mark needs clearing but reset_trackers flips the bitmaps so we clear everything
            _bitmap.clear_both();
        }

        [[gnu::hot]] bool record_mark(managed* ptr, bool is_pinned) {
            if (is_pinned) set_pinned(); // Pin regardless of the fact that the mark bit is set or not

            auto pos = (size_t)((uint8_t*)ptr - get_data()) / _block_sz;
            auto [word, in_word] = std::pair { pos / 64, 1ull << (pos % 64) };

            auto ref = std::atomic_ref { _bitmap.mark_bits()[word] };
            auto res = ref.fetch_or(in_word, std::memory_order_relaxed);
            return (res & in_word) == 0;
        }
        void compute_live() {
            auto p = _bitmap.mark_bits();
            _num_live = 0;
            for (auto w : p) _num_live += std::popcount(w);
        }
        void clear_mark_bitmap() const { _bitmap.clear_mark(); }

        size_t load_alloc_word(uint16_t bit) const  { return _bitmap.load_alloc(bit); }
        void   store_alloc_word(uint16_t bit, size_t w) const { _bitmap.store_alloc(bit, w); }
    };

    inline pg_meta* pg_from_obj(managed* obj) {
        return (pg_meta*)((size_t)obj & ~(config::page_sz - 1));
    }
}
