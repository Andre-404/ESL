#pragma once

#include <cstdint>
#include <cstring>
#include <functional>
#include "../customization.h"
#include "../gc-config.h"

namespace gc::detail::test_custom {

    struct hook_set {
        std::function<bool(managed*)>                                              obj_traceable
                = [](managed*) { return true; };
        std::function<size_t(managed*)>                                            obj_size
                = [](managed*) { return size_t{64}; };
        std::function<void(managed*, std::function<void(managed*)>&)>              obj_trace
                = [](managed*, auto&) {};

        // Words are raw pointers, either color
        std::function<managed*(size_t)>                                            to_accurate_ptr
                = [](size_t w) { return reinterpret_cast<managed*>(w & ~config::color_bit); };
        std::function<uint8_t*(size_t)>                                            to_possible_ptr
                = [](size_t w) { return reinterpret_cast<uint8_t*>(w & ~config::color_bit); };
        std::function<size_t(managed*)>                                            ptr_to_word
                = [](managed* p) { return reinterpret_cast<size_t>(p); };

        void reset() { *this = hook_set{}; }
    };

    inline hook_set hooks;

}