#pragma once
// IWYU pragma: private, include "platform-specific.h"
#include <cstdint>

// Unit tests aren't built with r14 reserved, so the color lives in memory. They use the real with_regs_pushed
namespace gc::platform {
    inline thread_local uint64_t color_reg = 0;
    inline uint64_t read_color() { return color_reg; }
    inline void write_color(uint64_t c) { color_reg = c; }
}
