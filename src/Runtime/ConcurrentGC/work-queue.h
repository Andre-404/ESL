#pragma once
#include <atomic>
#include <cstddef>
#include <vector>

namespace gc::detail {
    // Work handed out to whichever thread asks first. One thread fills it while nobody takes,
    // then any number of threads take items until it runs dry
    template<typename T>
    class work_queue {
        std::vector<T> _items;
        std::atomic<size_t> _next;
    public:
        work_queue() : _items(), _next(0) {}

        void fill(std::vector<T> items) {
            _items = std::move(items);
            _next.store(0, std::memory_order_relaxed);
        }
        // Thread safe, nullptr once every item has been taken
        T* take() {
            auto i = _next.fetch_add(1, std::memory_order_relaxed);
            return i < _items.size() ? &_items[i] : nullptr;
        }
        void clear() { fill({}); }
        bool empty() const { return _items.empty(); }
    };
}
