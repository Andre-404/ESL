#pragma once
#include <array>
#include <cstdint>

namespace gc {
    namespace config {
        constexpr std::size_t szclass_cnt =  48;
        constexpr std::size_t smalL_granularity = 16;
        constexpr std::size_t small_sz_classes = 32;
        constexpr std::size_t med_granularity = 128;
        constexpr std::size_t med_sz_classes = szclass_cnt - small_sz_classes;

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

        constexpr auto sz_to_class_table = []() constexpr {
            auto arr = std::array<uint8_t, sz_classes[szclass_cnt - 1] / 16> {};
            uint8_t cur_sz_class = 0;
            for (std::size_t i = 0; i < sz_classes[szclass_cnt - 1]; i += 16) {
                if (i >= sz_classes[cur_sz_class]) cur_sz_class++;
                arr[i / 16] = cur_sz_class;
            }
            return arr;
        }();


        constexpr int8_t sz_to_class(std::size_t sz) {
            if (sz > sz_classes[szclass_cnt - 1]) return -1;
            return sz == 0 ? 0 : sz_to_class_table[(sz-1) / 16];
        }

        constexpr std::size_t heap_bits = 48; // TODO: shrink to 40 bits once we have a proper allocator
        constexpr std::size_t pg_bits = 16;
        constexpr std::size_t l1_bits = 16;
        constexpr std::size_t l2_bits = heap_bits - l1_bits - pg_bits;

        constexpr std::size_t page_sz = 1ull << pg_bits;
        constexpr std::size_t heap_max_sz = 1ull << heap_bits;
        constexpr std::size_t l1_sz = 1ull << l1_bits;
        constexpr std::size_t l2_sz = 1ull << l2_bits;

        constexpr int64_t debt_trigger = 128 * 1024;

        constexpr std::size_t empty_mark_bufs_limit = 256;
        constexpr double dead_commited_to_alloced_ratio = 1.0;
        constexpr std::size_t trace_batch = 4 * (1 << 20);
        constexpr double copy_evac_threshold = 0.85;
    }

}