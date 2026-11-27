#pragma once
#include <time.h>

#include "os-time.h"

namespace gc::detail::os {
    std::chrono::nanoseconds thread_cpu_time() {
        timespec ts;
        clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
        return std::chrono::seconds { ts.tv_sec } + std::chrono::nanoseconds { ts.tv_nsec };
    }
}
