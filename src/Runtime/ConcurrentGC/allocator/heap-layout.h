#pragma once
#include <cstdint>
#include <new>

#include "../gc-config.h"
#include "os-memory.h"

namespace gc::detail {
    class pg_meta;

    // heap | headers | bits | reloc hdrs
    // One reservation aligned to the heap size, so masking a heap or header address with
    // heap_bits gives the start of its region and everything else is a fixed offset from there
    class heap_layout {
        static constexpr std::size_t hdrs_off = config::heap_max_sz;
        static constexpr std::size_t bits_off = hdrs_off + config::hdr_region_sz;
        static constexpr std::size_t relocs_off = bits_off + config::bits_region_sz;
        static constexpr std::size_t total_sz = relocs_off + config::reloc_region_sz;

        uint8_t* _base;
    public:
        heap_layout() : _base((uint8_t*)os::reserve(total_sz, config::heap_max_sz)) {
            if (!_base) throw std::bad_alloc{};
        }
        ~heap_layout() { os::release(_base, total_sz); }
        heap_layout(const heap_layout&) = delete;
        heap_layout& operator=(const heap_layout&) = delete;

        uint8_t* heap() const { return _base; }
        pg_meta* hdrs() const { return (pg_meta*)(_base + hdrs_off); }
        uint8_t* bits() const { return _base + bits_off; }
        uint8_t* relocs() const { return _base + relocs_off; }
    };
}
