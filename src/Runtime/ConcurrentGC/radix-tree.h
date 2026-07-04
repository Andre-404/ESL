#pragma once
#include <cstdint>
#include <tuple>

namespace gc::detail {
    class summary {
        std::size_t _data;
    public:
        summary() : _data(0) {}
        summary(std::size_t data) : _data(data) {}

        std::tuple<uint32_t, uint32_t, uint32_t, bool> get();
    };
}
