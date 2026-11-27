#pragma once
#include <cstdint>
#include <windows.h>

#include "os-time.h"

namespace gc::detail::os {
    // Windows only updates these on the scheduler tick, so they move in steps of about 15.6 ms
    std::chrono::nanoseconds thread_cpu_time() {
        FILETIME created, exited, kernel, user;
        GetThreadTimes(GetCurrentThread(), &created, &exited, &kernel, &user);
        // Counts of 100 ns
        auto ticks = [](FILETIME f) { return (uint64_t(f.dwHighDateTime) << 32) | f.dwLowDateTime; };
        return std::chrono::nanoseconds { (ticks(kernel) + ticks(user)) * 100 };
    }
}
