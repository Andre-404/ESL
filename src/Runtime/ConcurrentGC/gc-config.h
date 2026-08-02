#pragma once
#include <array>
#include <cstdint>
#include <cassert>

namespace gc {
    namespace config {
        constexpr std::size_t szclass_cnt =  48;
        constexpr std::size_t smalL_granularity = 16;
        constexpr std::size_t small_sz_classes = 32;
        constexpr std::size_t med_granularity = 128;
        constexpr std::size_t med_sz_classes = szclass_cnt - small_sz_classes;
        constexpr std::size_t large_class = szclass_cnt;

        constexpr auto sz_classes = []() constexpr {
            auto arr = std::array<uint16_t, szclass_cnt> {};
            for(std::size_t i = 0; i < small_sz_classes; i++){
                arr[i] = (i+1)*smalL_granularity;
            }
            for(std::size_t i = 0; i < med_sz_classes; i++){
                arr[small_sz_classes+i] = (i+1)*med_granularity + small_sz_classes*smalL_granularity;
            }

            return arr;
        }();

        // Inverting is a divide by the granularity, because each run is an arithmetic
        // progression. The offsets are there because a class is a ceiling, not a floor:
        // small class k holds (k+1)*16 bytes and so covers (16k, 16k+16], open at the
        // bottom, which is what the -1 accounts for. The medium run starts one byte past
        // the last small class for the same reason. Both granularities are powers of two,
        // so neither division emits a div.
        // sz == 0 is asserted instead of handled: sz - 1 would wrap and name a class that
        // does not exist, and no caller can reach it since every allocation is at least
        // sizeof(managed).
        constexpr int8_t sz_to_class(std::size_t sz) {
            assert(sz > 0);
            auto last_small = sz_classes[small_sz_classes - 1];
            auto last_medium = sz_classes[szclass_cnt - 1];
            if (sz > last_medium) return large_class;
            if (sz > last_small)
                return small_sz_classes + ((sz - (last_small + 1)) / med_granularity);
            return (sz - 1) / smalL_granularity;
        }

        // Allocator stuff
        constexpr std::size_t heap_bits = 40;
        constexpr std::size_t pg_bits = 16;
        constexpr std::size_t l1_bits = 14;
        constexpr std::size_t l2_bits = heap_bits - l1_bits - pg_bits;

        constexpr std::size_t page_sz = 1ull << pg_bits;
        constexpr std::size_t heap_max_sz = 1ull << heap_bits;
        constexpr std::size_t l1_sz = 1ull << l1_bits;
        constexpr std::size_t l2_sz = 1ull << l2_bits;

        constexpr std::size_t total_pages = heap_max_sz / page_sz;
        constexpr std::size_t chunk_pages = 512;
        constexpr std::size_t chunk_words = chunk_pages / 64;
        // Pages committed in one syscall, so the commit rate stays decoupled from page_sz.
        constexpr std::size_t commit_granule = 8;

        static_assert(total_pages % chunk_pages == 0);
        static_assert(total_pages % (chunk_pages * commit_granule) == 0);

        // Heuristics
        constexpr int64_t debt_trigger = 128 * 1024;

        constexpr std::size_t empty_mark_bufs_limit = 256;
        constexpr std::size_t trace_batch = 4 * (1 << 20);
        constexpr double copy_evac_threshold = 0.85;
        constexpr std::size_t free_batch_sz = 128 * 1024 * 1024;
    }

}