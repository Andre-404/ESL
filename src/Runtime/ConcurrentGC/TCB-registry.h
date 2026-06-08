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
        void remove(tcb* tcb) {
            auto lk = std::lock_guard { _mtx };
            _registry.erase(tcb);
        }

        template<typename F>
        void with_snapshot(F consume) {
            auto lk = std::lock_guard { _mtx };
            consume(_registry);
        }
    };
}