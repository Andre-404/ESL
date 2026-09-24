#pragma once

#include <algorithm>
#include <array>
#include <span>
#include <atomic>
#include <cstdint>

#include "gc-config.h"

namespace gc::detail {
    // A page whose contents were mostly replaced goes back to age 0 (see pruner::record_history), so age 0
    // means "a population about one cycle old" and its rate is measured from such pages once
    // they stop being filled. A minority refill leaves the age alone, which is what keeps the
    // pages the allocator consolidates into from being read as nurseries.
    class survival_model {
        struct ratio { std::atomic<uint64_t> before { 0 }, after { 0 }; };

        std::array<ratio, config::pg_age_cnt> _by_age;
        std::array<std::atomic<double>, config::pg_age_cnt> _rate;

        static void blend(std::atomic<double>& rate, ratio& r) {
            auto before = r.before.exchange(0, std::memory_order_relaxed);
            auto after  = r.after.exchange(0, std::memory_order_relaxed);
            // No page of this age came through the cycle untouched, so the last estimate is
            // still the best one we have
            if (before == 0) return;

            auto sampled = std::min(1.0, double(after) / double(before));
            auto prev = rate.load(std::memory_order_relaxed);
            rate.store((1.0 - config::survival_alpha) * prev + config::survival_alpha * sampled,
                       std::memory_order_relaxed);
        }

    public:
        survival_model() {
            for (auto& r : _rate) r.store(config::survival_seed, std::memory_order_relaxed);
        }
        survival_model(const survival_model&) = delete;
        survival_model& operator=(const survival_model&) = delete;

        class batch {
            survival_model& _model;
            std::array<std::pair<uint64_t, uint64_t>, config::pg_age_cnt> _by_age {};

        public:
            explicit batch(survival_model& model) : _model(model) {}
            ~batch() { _model.merge(_by_age); }
            batch(const batch&) = delete;
            batch& operator=(const batch&) = delete;

            void add_retained(uint8_t age, uint64_t live_before, uint64_t live_now) {
                auto& [before, after] = _by_age[std::min<uint8_t>(age, config::pg_age_cnt - 1)];
                before += live_before;
                after  += live_now;
            }

        };

        batch start_batch() { return batch { *this }; }

        void merge(const std::span<std::pair<uint64_t, uint64_t>, config::pg_age_cnt> by_age) {
            for (size_t age = 0; age < by_age.size(); age++) {
                auto [before, after] = by_age[age];
                if (before == 0) continue;
                _by_age[age].before.fetch_add(before, std::memory_order_relaxed);
                _by_age[age].after.fetch_add(after, std::memory_order_relaxed);
            }
        }

        void end_cycle() {
            for (size_t age = 0; age < _rate.size(); age++) blend(_rate[age], _by_age[age]);
        }

        double survival(uint8_t age) const {
            return _rate[std::min<uint8_t>(age, config::pg_age_cnt - 1)].load(std::memory_order_relaxed);
        }
    };
}
