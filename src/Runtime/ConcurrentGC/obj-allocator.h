#pragma once
#include <assert.h>

#include "pg-meta.h"

namespace gc::detail {
    class obj_allocator {
        pg_meta* _pg;
        uint64_t _cache;
        uint16_t _pos;
        // Cached so we don't need to access pg on the hot path, kind of breaks whole encapsulation thing
        // must never be mutated, not marked const since it deletes the move assignment operator
        uint16_t _pg_cnt;
        uint16_t _pg_off;
        uint16_t _pg_blk_sz;
        static constexpr uint64_t FULL = ~0ull;

        [[gnu::hot, gnu::always_inline]] inline int8_t get_cached() {
            if (_cache != FULL) [[likely]] return std::countr_one(_cache);
            // It's very important that alloc bits not get written when the cache fills up
            // otherwise the following could happen:
            /*
             thdA: allocate and flush cache       thdB: start stack scan
             thdA: in process of setting up obj O thdB: finds object O because it conservativly scans stack
             thdA: in process of setting up obj   thdB: pushes object O to mark and then flushes its mark buf
             thdA: in process of setting up obj   thdGC: starts tracing O while its fields are not set up
             ------O is now treated as marked even though its fields were garbage, could also crash if typeid is garbage-----
            */
            // Flushing cache before allocating means:
            // all objects in cache have been set up correctly(have to watch out for allocators in constructors of managed objs)

            _pg->store_alloc_word(_pos, _cache);

            _pos += 64; // To avoid past end reads
            while (_pos < _pg_cnt) {
                _cache = _pg->load_alloc_word(_pos);
                if (_cache != FULL) return std::countr_one(_cache);
                _pos += 64;
            }
            return -1;
        }
        void cache_mark(uint64_t pos) { _cache |= 1ull << pos; }
        managed* calc_obj(uint64_t cache_pos) {
            return (managed*)((uint8_t*)_pg + _pg_off + (_pos + cache_pos)*_pg_blk_sz);
        }
    public:
        obj_allocator() : _pg(nullptr), _cache(0), _pos(0), _pg_cnt(0), _pg_off(0), _pg_blk_sz(0) {}
        explicit obj_allocator(pg_meta& pg) : _pg(&pg), _cache(_pg->load_alloc_word(0)), _pos(0), _pg_cnt(_pg->block_cnt()),
            _pg_off(_pg->start_off()), _pg_blk_sz(_pg->block_sz()) {}

        [[gnu::hot, gnu::always_inline]] managed* allocate() {
            assert(_pg);
            auto cache_pos = get_cached();
            if (cache_pos < 0 || (_pos + cache_pos) >= _pg_cnt) return nullptr;
            cache_mark(cache_pos);
            auto res = calc_obj(cache_pos);
            return res;
        }

        void flush_cache() {
            _pg->store_alloc_word(_pos, _cache);
        }

        pg_meta* get_pg() const { return _pg; }
    };
}