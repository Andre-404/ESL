#pragma once
#include <cassert>
#include <cstdint>
#include <new>

#include "../gc-config.h"
#include "os-memory.h"

namespace gc::detail {
    class pg_meta;

    // heap | headers | bits | reloc hdrs | reloc units
    // One reservation aligned to the heap size, so masking a heap or header address with
    // heap_bits gives the start of its region and everything else is a fixed offset from there
    class heap_layout {
        static constexpr std::size_t hdrs_off = config::heap_max_sz;
        static constexpr std::size_t bits_off = hdrs_off + config::hdr_region_sz;
        static constexpr std::size_t relocs_off = bits_off + config::bits_region_sz;
        static constexpr std::size_t units_off = relocs_off + config::reloc_region_sz;
        static constexpr std::size_t total_sz = units_off + config::reloc_units_sz;

        inline static uint8_t* _base = nullptr;
    public:
        // One heap per process, the barrier and heal paths find it by address alone
        heap_layout() {
            assert(!_base && "one heap per process");
            _base = (uint8_t*)os::reserve(total_sz, config::heap_max_sz);
            if (!_base) throw std::bad_alloc{};
        }
        ~heap_layout() { os::release(_base, total_sz); _base = nullptr; }
        heap_layout(const heap_layout&) = delete;
        heap_layout& operator=(const heap_layout&) = delete;

        uint8_t* heap() const { return _base; }
        pg_meta* hdrs() const { return hdrs(_base); }
        uint8_t* bits() const { return bits(_base); }
        uint8_t* relocs() const { return relocs(_base); }
        uint8_t* reloc_units() const { return reloc_units(_base); }

        // The same regions found from any address inside the heap
        static uint8_t* heap(const void* in_heap) { return (uint8_t*)((uintptr_t)in_heap & ~(config::heap_max_sz - 1)); }
        static pg_meta* hdrs(const void* in_heap) { return (pg_meta*)(heap(in_heap) + hdrs_off); }
        static uint8_t* bits(const void* in_heap) { return heap(in_heap) + bits_off; }
        static uint8_t* relocs(const void* in_heap) { return heap(in_heap) + relocs_off; }
        static uint8_t* reloc_units(const void* in_heap) { return heap(in_heap) + units_off; }
        // By address alone, p is never read
        static bool in_heap(const void* p) { return _base && heap(p) == _base; }
        // Masking a header lands on the start of the header region, the heap sits right below it
        static uint8_t* heap_from_hdr(const void* hdr) { return heap(hdr) - hdrs_off; }

        static size_t page_idx(const void* page) {
            return ((uint8_t*)page - heap(page)) / config::page_sz;
        }
    };
}
