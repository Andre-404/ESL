#pragma once
#include "gc-config.h"
#include "managed.h"
#include "tstack.h"
#include <memory>

namespace gc::detail {
    class dual_bitmap {
        // Padded to 8bytes
        uint16_t _bitmap_size;
        uint16_t _isflipped;
    public:
        dual_bitmap() : _bitmap_size(0), _isflipped(false) {}
        dual_bitmap(uint16_t _bitmap_sz) : _bitmap_size(_bitmap_sz), _isflipped(false) {}

        uint8_t* alloc_bits() const {
            if (_isflipped) return (uint8_t*)this + sizeof(dual_bitmap) + _bitmap_size;
            return (uint8_t*)this + sizeof(dual_bitmap);
        }
        uint8_t* mark_bits() const {
            if (!_isflipped) return (uint8_t*)this + sizeof(dual_bitmap) + _bitmap_size;
            return (uint8_t*)this + sizeof(dual_bitmap);
        }
        void flip() { _isflipped = !_isflipped; }
        void clear_mark() {
            auto ptr = std::assume_aligned<8>(mark_bits());
            memset(ptr, 0, _bitmap_size);
        }
        void clear_both() {
            auto ptr = std::assume_aligned<8>((uint8_t*)this + sizeof(dual_bitmap));
            memset(ptr, 0, _bitmap_size * 2);
        }
    };
    class pg_meta : public tnode<pg_meta> {
        // Number of objects live in the last gc cycle
        std::atomic<uint16_t> _num_live;
        const uint16_t _block_sz;
        const uint16_t _block_cnt;
        const uint16_t _slot_start;
        uint8_t _is_pod;
        std::atomic<uint8_t> _has_pinned;
        dual_bitmap _bitmap;

        static constexpr size_t slots_per_page(size_t X, size_t Y) {
            const size_t m = (X - 32 + 63 * Y) / (64 * Y + 16);
            const size_t cap   = 64 * m;
            const size_t space = (X - 16 * (m + 2)) / Y;
            return std::min(cap, space);
        }

        static constexpr size_t bitmap_sz(size_t block_cnt) {
            return (block_cnt + 63) / 64 * 8;
        }
        uint8_t* get_data() const {
            return (uint8_t*)(this) + _slot_start;
        }
        void add_live() { _num_live.fetch_add(1, std::memory_order_relaxed); }
        void set_pinned() { _has_pinned.store(true, std::memory_order_relaxed); }
    public:
        explicit pg_meta(uint16_t block_sz) : _num_live(0), _block_sz(block_sz), _block_cnt(slots_per_page(config::page_sz, _block_sz)),
            _slot_start(32 * 2 * bitmap_sz(_block_cnt)), _is_pod(true), _has_pinned(false), _bitmap(bitmap_sz(_block_cnt)) {}

        // For large objects which are their own pages
        explicit pg_meta(uint16_t block_sz, size_t page_sz) : _num_live(0), _block_sz(block_sz), _block_cnt(slots_per_page(page_sz, _block_sz)),
            _slot_start(32 * 2 * bitmap_sz(_block_cnt)), _is_pod(true), _has_pinned(false), _bitmap(bitmap_sz(_block_cnt)) {}

        class pg_slots_iter {
            const pg_meta* _pg;
            uint16_t _i;
        public:
            pg_slots_iter(pg_meta* pg, uint16_t i) : _pg(pg), _i(i) {}
            pg_slots_iter(pg_meta* pg, managed* ptr) : _pg(pg) {
                _i = (size_t)(ptr - pg->_slot_start) / pg->block_cnt();
            }

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
                auto [byte, in_byte] = std::pair { _i / 8, _i % 8};
                auto ref = std::atomic_ref { _pg->_bitmap.mark_bits()[byte] };
                return ref.load(std::memory_order_acquire) & (1 << in_byte);
            }
        };

        managed* slot_containing(uint8_t* ptr) const {
            auto diff = (ptr - (uint8_t*)this);
            if (diff < _slot_start) return nullptr;
            diff -= _slot_start;
            return (managed*)(ptr - diff % _block_sz);
        }

        uint16_t block_sz() const {
            return _block_sz;
        }
        uint16_t block_cnt() const {
            return _block_cnt;
        }
        void reset_trackers() {
            _num_live = 0;
            _has_pinned = false;
            _bitmap.flip();
        }

        void recycle() {
            _num_live = 0;
            _has_pinned = false;
            _bitmap.clear_both();
        }

        bool record_mark(managed* ptr, bool is_pinned) {
            auto pos = ((size_t)ptr - _slot_start) / _block_sz;
            auto [byte, in_byte] = std::pair { pos / 8, pos % 8};

            auto ref = std::atomic_ref { _bitmap.mark_bits()[byte] };
            auto res = ref.fetch_or(1 << in_byte, std::memory_order_acq_rel);
            if (res & (1 << in_byte)) return false;
            add_live();
            if (is_pinned) set_pinned();
            return true;
        }
        void clear_mark_bitmap() { _bitmap.clear_mark(); }
        size_t& alloc_word(uint16_t pos) const {
            size_t* ptr =(size_t*)_bitmap.alloc_bits();
            return ptr[pos / 8];
        }

        uint16_t live_count() const { return _num_live.load(std::memory_order_relaxed); }
        bool has_pinned() const { return _has_pinned.load(std::memory_order_relaxed); }

        void set_nonpod() { _is_pod = false; }
        bool is_pod() const { return _is_pod; }
    };

    inline pg_meta* pg_from_obj(managed* obj) {
        return (pg_meta*)((size_t)obj & ~(config::page_sz - 1));
    }

    // sizeof(header) + 2 * sizeof(bitmap size) + object sz
    inline size_t large_pg_sz(size_t obj_sz) {
        return sizeof(pg_meta) + 2 + obj_sz;
    }
}