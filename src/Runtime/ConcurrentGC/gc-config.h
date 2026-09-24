#pragma once
#include <array>
#include <cstdint>
#include <cassert>

namespace gc {
    namespace config {
        constexpr std::size_t szclass_cnt =  48;
        constexpr std::size_t small_granularity = 16;
        constexpr std::size_t small_sz_classes = 32;
        constexpr std::size_t med_granularity = 128;
        constexpr std::size_t med_sz_classes = szclass_cnt - small_sz_classes;
        constexpr uint8_t large_class = szclass_cnt;

        constexpr auto sz_classes = []() constexpr {
            auto arr = std::array<uint16_t, szclass_cnt+1> {};
            for(std::size_t i = 0; i < small_sz_classes; i++){
                arr[i] = (i+1)*small_granularity;
            }
            for(std::size_t i = 0; i < med_sz_classes; i++){
                arr[small_sz_classes+i] = (i+1)*med_granularity + small_sz_classes*small_granularity;
            }
            // Small optimizations for branchless code in pg_meta, is it safe?
            arr[szclass_cnt] = 1;
            return arr;
        }();

        // The offsets are there because a class is a ceiling, not a floor:
        // small class k holds (k+1)*16 bytes and so covers (16k, 16k+16], open at the
        // bottom, which is what the -1 accounts for. The medium run starts one byte past
        // the last small class for the same reason. Both granularities are powers of two,
        // so neither division emits a div.
        constexpr uint8_t sz_to_class(std::size_t sz) {
            assert(sz > 0);
            auto last_small = sz_classes[small_sz_classes - 1];
            auto last_medium = sz_classes[szclass_cnt - 1];
            if (sz > last_medium) return large_class;
            if (sz > last_small)
                return small_sz_classes + ((sz - (last_small + 1)) / med_granularity);
            return (sz - 1) / small_granularity;
        }

        // Allocator stuff
        constexpr std::size_t heap_bits = 40;
        constexpr std::size_t pg_bits = 16;

        constexpr std::size_t page_sz = 1ull << pg_bits;
        constexpr std::size_t small_obj_pgs = 1;
        constexpr std::size_t heap_max_sz = 1ull << heap_bits;

        constexpr uint16_t blocks_in_pg(uint8_t szclass) {
            if (szclass == large_class) return 1;
            return page_sz*small_obj_pgs / sz_classes[szclass];
        }

        constexpr std::size_t total_pages = heap_max_sz / page_sz;
        constexpr std::size_t chunk_pages = 512;
        constexpr std::size_t chunk_words = chunk_pages / 64;
        // Pages committed in one syscall, so the commit rate stays decoupled from page_sz.
        constexpr std::size_t commit_granule = 8;
        constexpr std::size_t commit_syscall_sz = 64ull << 10;

        static_assert(total_pages % chunk_pages == 0);
        static_assert(total_pages % (chunk_pages * commit_granule) == 0);

        // size of pg_meta, used by dual_bitmap since it has no access to pg_meta
        constexpr std::size_t hdr_entry_sz = 16;
        constexpr std::size_t hdr_region_sz = total_pages * hdr_entry_sz;

        // Worst case is one bit per 16 byte block, i.e. heap/128 of live bitmaps per half;
        // doubled because a run created and freed inside one cycle keeps its slot until the flip
        constexpr std::size_t bits_arena_bits = heap_bits - 6;
        constexpr std::size_t bits_arena_sz = 1ull << bits_arena_bits;
        constexpr std::size_t bits_region_sz = 2 * bits_arena_sz;

        // Bitmaps in pg_meta are stored as an offset(in words) into the bitmap array
        // -1 since offsets are in range [0, 2^32)
        static_assert((config::bits_region_sz / sizeof(uint64_t))-1 <= UINT32_MAX);

        // Sized to mark region, objects that need their pointers updated are mark & card
        constexpr std::size_t card_region_sz = bits_arena_sz;

        // live:13 | age:3 = 16 bits
        constexpr std::size_t history_entry_sz = 2;
        constexpr std::size_t history_region_sz = total_pages * history_entry_sz;

        constexpr std::size_t pg_live_bits = 13;
        constexpr std::size_t pg_age_cnt = 8;
        // This will need to be revised if we ever switch to a page size other than 8kb,
        // but that seems highly unlikely so for now just guard with assert
        static_assert(blocks_in_pg(0) <= (1u << pg_live_bits) - 1);
        static_assert(pg_age_cnt == (1u << (16 - pg_live_bits)));

        // Heuristics
        constexpr int64_t debt_trigger = 128 * 1024;

        constexpr std::size_t empty_mark_bufs_limit = 256;
        constexpr std::size_t trace_batch = 16 * (1 << 20);
        constexpr double copy_evac_threshold = 0.85;
        // Nomination is a prediction and only has to be a superset of what split_pages picks
        // with copy_evac_threshold and exact counts, so it sits deliberately above it
        constexpr double nominate_threshold = 0.9;
        static_assert(nominate_threshold >= copy_evac_threshold);
        // Seeded pessimistically enough that a cold table neither 
        // hoards nor evacuates the whole heap on the first copying cycle
        constexpr double survival_alpha = 0.3;
        constexpr double survival_seed  = 0.5;
        constexpr std::size_t free_batch_sz = 128 * 1024 * 1024;
    }

}