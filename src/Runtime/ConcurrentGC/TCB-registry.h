#pragma once

#include <mutex>
#include "../../Includes/unorderedDense.h"

namespace gc::detail {
    class tcb;
    class tcb_registry {
        std::mutex _mtx;
        ankerl::unordered_dense::set<tcb*> _registry;
    public:
        tcb_registry() {};

        template<typename F>
        void add(tcb* tcb, F under_lock) {
            auto lk = std::lock_guard { _mtx };
            _registry.insert(tcb);
            under_lock();
        }
        template<typename F>
        void remove(tcb* tcb, F under_lock) {
            auto lk = std::lock_guard { _mtx };
            _registry.erase(tcb);
            under_lock();
        }

        template<typename F>
        void with_snapshot(F consume) {
            auto lk = std::lock_guard { _mtx };
            consume(_registry);
        }

        template<typename F>
        void under_lock(F f) {
            auto lk = std::lock_guard { _mtx };
            f();
        }
    };
}