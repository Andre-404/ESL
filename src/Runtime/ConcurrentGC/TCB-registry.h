#pragma once

#include <mutex>
#include "../../Includes/unorderedDense.h"

namespace gc::detail {
    class tcb;

    class tcb_registry {
        std::mutex _mtx;
        ankerl::unordered_dense::set<tcb*> _registry;
    public:
        tcb_registry();

        void add(tcb* tcb) {
            auto lk = std::lock_guard { _mtx };
            _registry.insert(tcb);
        }
        void remove(tcb* tcb) {
            auto lk = std::lock_guard { _mtx };
            _registry.erase(tcb);
        }

        template<typename F>
        void iterate(F consume) {
            auto lk = std::lock_guard { _mtx };
            for (const auto& tcb : _registry) consume(tcb);
        }
    };
}