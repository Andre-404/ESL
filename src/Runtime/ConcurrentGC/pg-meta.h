#pragma once
#include "gc-config.h"
#include "managed.h"
#include "tstack.h"
#include <memory>
#include <cstring>
#include <algorithm>
#include <cassert>

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
            if (_isflipped) return (uint8_t*)this + sizeof(dual_bitmap);
            return (uint8_t*)this + sizeof(dual_bitmap) + _bitmap_size;
        }
        void flip() { _isflipped = !_isflipped; }
        void clear_mark() const {
            auto ptr = std::assume_aligned<8>(mark_bits());
            memset(ptr, 0, _bitmap_size);
        }
    };
    class pg_meta : public tnode<pg_meta> {
        // Number of objects live in the last gc cycle
        const size_t _block_sz;
        const uint16_t _block_cnt;
        const uint16_t _slot_start;
        std::atomic<uint16_t> _num_live;
        std::atomic<uint8_t> _has_pinned;
        // Padding is here because dual_bitmap assumes its bitmap comes after the class itself
        char _[4];
        dual_bitmap _bitmap;

        static constexpr auto mapping = []() constexpr {
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
            return config::sz_to_class(sz) == -1 ? (uint16_t)1 : mapping[config::sz_to_class(sz)];
        }
        static constexpr auto bitmap_sz(size_t cnt) { return (uint16_t)((cnt + 63) / 64 * 8); }

        uint8_t* get_data() const { return (uint8_t*)(this) + _slot_start; }
        void add_live() { _num_live.fetch_add(1, std::memory_order_relaxed); }
        void set_pinned() { _has_pinned.store(true, std::memory_order_relaxed); }
    public:
        explicit pg_meta(size_t block_sz) : _block_sz(block_sz), _num_live(0), _block_cnt(block_cnt(block_sz)),
            _slot_start(32 + 2 * bitmap_sz(_block_cnt)), _has_pinned(false), _bitmap(bitmap_sz(_block_cnt)) {}

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
                auto [byte, in_byte] = std::pair { _i / 8, _i % 8};
                auto ref = std::atomic_ref { _pg->_bitmap.mark_bits()[byte] };
                return ref.load(std::memory_order_acquire) & (1 << in_byte);
            }
            size_t internal() const { return _i; };
        };

        managed* from_interior(uint8_t* interior) const {
            auto diff = interior - (uint8_t*)this;
            if (diff < _slot_start || diff >= (_slot_start + block_cnt() * _block_sz)) return nullptr;
            diff -= _slot_start;
            auto ptr = (managed*)(interior - diff % _block_sz);

            auto idx = (size_t)((uint8_t*)ptr - get_data()) / _block_sz;
            size_t* p =(size_t*)_bitmap.alloc_bits();
            if (p[idx / 64] & (1 << idx % 64)) return ptr;
            return nullptr;
        }

        size_t block_sz() const { return _block_sz; }
        uint16_t block_cnt() const { return _block_cnt; }
        uint16_t start_off() const { return _slot_start; }
        uint16_t live_count() const { return _num_live.load(std::memory_order_relaxed); }
        bool has_pinned() const { return _has_pinned.load(std::memory_order_relaxed); }
        void reset_trackers() {
            _num_live = 0;
            _has_pinned = false;
            _bitmap.flip();
        }
        void recycle() {
            _num_live = 0;
            _has_pinned = false;
            // Only empty pages get recycled, and their allocated bitmap is empty
            _bitmap.clear_mark();
        }

        bool record_mark(managed* ptr, bool is_pinned) {
            if (is_pinned) set_pinned(); // Pin regardless of the fact that the mark bit is set or not

            auto pos = (size_t)((uint8_t*)ptr - get_data()) / _block_sz;
            auto [byte, in_byte] = std::pair { pos / 8, pos % 8};

            auto ref = std::atomic_ref { _bitmap.mark_bits()[byte] };
            auto res = ref.fetch_or(1 << in_byte, std::memory_order_acq_rel);
            if (res & (1 << in_byte)) return false;
            add_live();
            assert(live_count() <= block_cnt());
            return true;
        }
        void clear_mark_bitmap() const { _bitmap.clear_mark(); }
        size_t& alloc_word(uint16_t pos) const {
            size_t* ptr =(size_t*)_bitmap.alloc_bits();
            return ptr[pos / 64];
        }
    };

    inline pg_meta* pg_from_obj(managed* obj) {
        return (pg_meta*)((size_t)obj & ~(config::page_sz - 1));
    }

    // sizeof(header) + 2 * sizeof(bitmap of 1 element) + object sz
    inline size_t large_pg_sz(size_t obj_sz) {
        return sizeof(pg_meta) + 2*8 + obj_sz;
    }

    inline bool is_large_pg(pg_meta* pg) {
        return config::sz_to_class(pg->block_sz()) == -1;
    }
}