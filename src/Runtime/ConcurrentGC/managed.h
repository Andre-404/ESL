#pragma once
#include <cstdint>

namespace gc {
    class managed {
        uint8_t _type_id;
    public:
        static constexpr bool needs_zeroing = true;

        explicit managed(uint8_t type_id) : _type_id(type_id) {}

        uint8_t get_type_id() const { return _type_id; }
    };
}
