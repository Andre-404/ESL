#pragma once

#include <atomic>
#include <cstdint>

namespace gc::detail {
    class collection_request {
        enum class type : uint8_t {
            none    = 0,  // nothing pending
            normal  = 1,  // heap trigger reached, collect when convenient
            express = 2,  // a mutator is out of memory and is blocked waiting
            end     = 3   // shut the collector down
        };
        std::atomic<type> _req { type::none };
        std::atomic<size_t> _cycles_completed { 0 };

    public:
        // ---- mutator side ----

        bool request_normal() {
            auto expected = type::none;
            if (_req.compare_exchange_strong(expected, type::normal, std::memory_order_acq_rel)) {
                _req.notify_all();
                return true;
            }
            return false;
        }

        // The caller is expected to block until express is served
        // Refuses to request express if end is posted
        void request_express() {
            auto cur = _req.load(std::memory_order_relaxed);
            while (cur != type::express && cur != type::end)
                if (_req.compare_exchange_weak(cur, type::express, std::memory_order_acq_rel)) {
                    _req.notify_all();
                    return;
                }
        }

        void await_express_served() {
            _req.wait(type::express);
        }

        void await_cycle() {
            auto seen = _cycles_completed.load(std::memory_order_acquire);
            if (!request_normal() && _req.load(std::memory_order_acquire) == type::end) return;
            _cycles_completed.wait(seen, std::memory_order_acquire);
        }

        // ---- collector side ----

        // Block until any request arrives. Returns true if it was a shutdown.
        bool await_request() {
            _req.wait(type::none);
            return _req.load(std::memory_order_relaxed) == type::end;
        }

        // True while an express (out-of-memory) request is being serviced.
        bool is_express() const {
            return _req.load(std::memory_order_acquire) == type::express;
        }

        void clear(bool served_express) {
            auto expected = served_express ? type::express : type::normal;
            if (_req.compare_exchange_strong(expected, type::none, std::memory_order_release, std::memory_order_relaxed))
                _req.notify_all();
            // After the CAS, so a collect woken here can request the next cycle
            _cycles_completed.fetch_add(1, std::memory_order_release);
            _cycles_completed.notify_all();
        }

        // Request shutdown and wake the worker.
        void shutdown() {
            _req.store(type::end);
            _req.notify_all();
            // Releases collect, which may be waiting for a cycle that never runs
            _cycles_completed.fetch_add(1, std::memory_order_release);
            _cycles_completed.notify_all();
        }
    };
}
