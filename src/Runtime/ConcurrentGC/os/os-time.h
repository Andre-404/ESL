#pragma once
#include <chrono>

// Thin wrappers over the platform's clocks, for what std::chrono doesn't have
namespace gc::detail::os {
    // CPU time the calling thread has used so far
    std::chrono::nanoseconds thread_cpu_time();
}
