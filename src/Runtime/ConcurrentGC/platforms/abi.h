#pragma once
// IWYU pragma: private, include "platform-specific.h"
#include <cstddef>

namespace gc::platform {
    // Pushes the callee-saved registers and calls fn(arg, sp), sp being where they are. Until fn returns,
    // everything the calling frames hold is at sp or above
    void with_regs_pushed(void* arg, void (*fn)(void* arg, std::size_t* sp)) asm("gc_platform_with_regs_pushed");
}
