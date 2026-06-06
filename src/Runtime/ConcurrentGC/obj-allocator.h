#pragma once
#include <assert.h>

#include "pg-meta.h"

// TODO: this can be improved to not load from the pg pointer if there is something in the cache
namespace gc::detail {
    class obj_allocator {
        pg_meta* _pg;
        size_t _cache;
        uint16_t _pos;

        uint16_t num_allocated() const {
            return _pos * 8 + std::countr_one(_cache);
        }
        [[gnu::hot]] int8_t get_cached() {
            auto free_in_cache = std::countr_one(_cache);
            while (free_in_cache == 64 && _pos < _pg->block_cnt()) {
                _pos += 8;
                _cache = _pg->alloc_word(_pos);
                free_in_cache = std::countr_one(_cache);
            }
            return free_in_cache == 64 ? -1 : free_in_cache;
        }
        void cache_mark(size_t pos) {
            _cache |= 1 << pos;
            if (std::countr_one(_cache) == 64) _pg->alloc_word(_pos) = _cache;
        }
    public:
        obj_allocator() : _pg(nullptr), _cache(0), _pos(0) {}
        explicit obj_allocator(pg_meta& pg) : _pg(&pg), _cache(_pg->alloc_word(0)), _pos(0) {}

        managed* allocate() {
            assert(_pg);
            if (num_allocated() >= _pg->block_cnt()) [[unlikely]] return nullptr;
            auto cache_pos = get_cached();
            if (cache_pos < 0) return nullptr;
            cache_mark(cache_pos);
            return pg_meta::pg_slots_iter { _pg, (uint16_t)(_pos * 8 + cache_pos) }.get();
        }

        void flush_cache() {
            _pg->alloc_word(_pos) = _cache;
        }

        pg_meta* get_pg() const { return _pg; }
    };
}