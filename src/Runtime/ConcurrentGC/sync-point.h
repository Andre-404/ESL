#pragma once
#include <mutex>
#include <condition_variable>

namespace gc::detail {
    class sync_point {
        std::mutex _mtx;
        std::condition_variable _cv;
        size_t _waiting, _expected;
        size_t _phase;

    public:
        sync_point() : _waiting(0), _expected{0}, _phase(0) {}
        void register_waiter()  { std::lock_guard lk(_mtx); ++_expected; }
        void deregister_waiter() {
            auto lk = std::unique_lock { _mtx };
            auto my_phase = _phase;
            if (_waiting == --_expected) {
                ++_phase;
                _waiting = 0;
                _cv.notify_all();
            } else {
                _cv.wait(lk, [&]{ return _phase != my_phase; });
            }
        }

        void arrive_and_wait() {
            auto lk = std::unique_lock { _mtx };
            auto my_phase = _phase;
            if (++_waiting == _expected) {
                ++_phase;
                _waiting = 0;
                _cv.notify_all();
            } else {
                _cv.wait(lk, [&]{ return _phase != my_phase; });
            }
        }

        void arrive() {
            auto lk = std::lock_guard { _mtx };
            if (++_waiting == _expected) {
                ++_phase;
                _waiting = 0;
                _cv.notify_all();
            }
        }
    };
}
