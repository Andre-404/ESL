#pragma once
#include <cstdint>
#include <cstddef>
#include <cstring>

namespace gc::detail::platform {
#ifdef UNIT_TESTS
    #include "platforms/testing.inc"
#elif __x86_64
    #include "platforms/x86-64.inc"
#endif
}