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

    public:
        // ---- mutator side ----

        // Raise a normal, heuristic-triggered collection. No-op if a request is
        // already outstanding. Returns true iff this call started the request.
        bool request_normal() {
            auto expected = type::none;
            if (_req.compare_exchange_strong(expected, type::normal, std::memory_order_acq_rel)) {
                _req.notify_all();
                return true;
            }
            return false;
        }

        // Raise an urgent, out-of-memory collection, overriding any pending
        // normal request. The caller is expected to block until it is served
        // Refuses to request express if end is posted
        void request_express() {
            auto expected = type::none;
            if (_req.compare_exchange_strong(expected, type::express, std::memory_order_acq_rel)) {
                _req.notify_all();
            }
            expected = type::normal;
            if (_req.compare_exchange_strong(expected, type::express, std::memory_order_acq_rel)) {
                _req.notify_all();
            }
        }

        void await_express_served() {
            _req.wait(type::express);
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

        // CAS so that we don't overwrite end request
        void clear() {
            auto expected = _req.load(std::memory_order_relaxed);
            if (expected == type::end ) return;
            _req.compare_exchange_strong(expected, type::none, std::memory_order_release);
            _req.notify_all();
        }

        // Request shutdown and wake the worker.
        void shutdown() {
            _req.store(type::end);
            _req.notify_all();
        }
    };
}
