#pragma once
#include <cstdint>

namespace gc::detail::platform {
#ifdef __x86_64
    #include "platforms/x86-64.inc"
#endif
}