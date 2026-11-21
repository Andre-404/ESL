#include <condition_variable>
#include <mutex>

#include "span-allocator.h"
#include "scavenger.h"

using namespace gc::detail;

void scavenger::loop(std::stop_token st, span_allocator& spans, scavenge_policy policy) {
    auto h = scavenge_heuristic { policy };
    auto mtx = std::mutex {};
    // Held across the loop; wait_for is what drops and retakes it
    auto lk = std::unique_lock { mtx };
    auto cv = std::condition_variable_any {};

    using clock = std::chrono::steady_clock;

    while (!st.stop_requested()) {
        auto now = clock::now();

        auto granules = h.plan(now, { spans.take_in_use_peak(), spans.resident() });
        auto released = spans.scavenge(granules);

        auto work_time = clock::now() - now;
        auto sleep = h.pace(released, work_time);

        cv.wait_for(lk, st, sleep, [&]{ return st.stop_requested(); });
    }
}
