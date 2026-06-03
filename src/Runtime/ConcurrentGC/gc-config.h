#pragma once
#include <array>

namespace gc {
    namespace config {
        constexpr size_t szclass_cnt =  48;
        constexpr size_t smalL_granularity = 16;
        constexpr size_t small_sz_classes = 32;
        constexpr size_t med_granularity = 128;
        constexpr size_t med_sz_classes = szclass_cnt - small_sz_classes;

        constexpr auto sz_classes = []() constexpr {
            auto arr = std::array<uint16_t, szclass_cnt> {};
            for(size_t i = 0; i < small_sz_classes; i++){
                arr[i] = (i+1)*smalL_granularity;
            }
            for(size_t i = 0; i < med_sz_classes; i++){
                arr[small_sz_classes+i] = (i+1)*med_granularity + small_sz_classes*smalL_granularity;
            }

            return arr;
        }();

        constexpr auto sz_to_class = []() constexpr {
            auto arr = std::array<uint8_t, sz_classes[szclass_cnt - 1] / 16> {};
            uint8_t cur_sz_class = 0;
            for (size_t i = 0; i < sz_classes[szclass_cnt - 1]; i += 16) {
                if (i >= sz_classes[cur_sz_class]) cur_sz_class++;
                arr[i / 16] = cur_sz_class;
            }
            return arr;
        }();

        constexpr size_t heap_bits = 48; // TODO: shrink to 40 bits once we have a proper allocator
        constexpr size_t pg_bits = 16;
        constexpr size_t l1_bits = 16;
        constexpr size_t l2_bits = heap_bits - l1_bits - pg_bits;

        constexpr size_t page_sz = 1 << pg_bits;
        constexpr size_t heap_max_sz = 1ull << heap_bits;
        constexpr size_t l1_sz = 1ull << l1_bits;
        constexpr size_t l2_sz = 1ull << l2_bits;
    }

}