#pragma once
#include "../pg-meta.h"
#include "gc-bits.h"
#include "heap-layout.h"
#include "pg-tables.h"
#include "reloc-hdrs.h"
#include "scavenger.h"
#include "span-allocator.h"

namespace gc::detail {
    // Declaration order is construction order: everything after _layout lives inside it, and
    // the scavenger thread has to stop before _spans goes away
    class pg_allocator {
        heap_layout _layout;
        reloc_hdrs _relocs;
        pg_tables _tables;
        span_allocator _spans;
        scavenger _scavenger;
        gc_bits _bits;

    public:
        explicit pg_allocator(scavenge_policy policy = {})
            : _relocs(_layout.relocs(), _layout.reloc_units()), _tables(_layout.hdrs(), _relocs), _spans(_layout.heap()),
            _scavenger(_spans, policy), _bits(_layout.bits()) {}

        uint8_t* heap_base() const { return _layout.heap(); }
        pg_meta* meta_base() const { return _layout.hdrs(); }
        // The alloc/mark bitmap arena the headers point into; the collector drives its flip.
        gc_bits& bits() { return _bits; }
        reloc_hdrs& relocs() { return _relocs; }
        bool pg_active(pg_meta* pg) { return _tables.pg_active(pg); }

        // An aligned run starts on a multiple of its own length in pages
        pg_meta* alloc_pg(size_t block_sz, size_t num_pgs, bool aligned = false);
        void free_pgs(pg_meta* start);
    };
}
