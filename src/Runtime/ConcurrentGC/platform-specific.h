#pragma once
// The platform layer's one header: it exports the ABI's entry and the ISA's registers
#include "platforms/abi.h"  // IWYU pragma: export
#ifdef UNIT_TESTS
    #include "platforms/testing.h"  // IWYU pragma: export
#elif __x86_64
    #include "platforms/x86-64.h"  // IWYU pragma: export
#endif
