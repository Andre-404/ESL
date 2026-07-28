#pragma once
#include <cstdint>
#include <atomic>

namespace gc {
    enum class move_state : uint8_t { none = 0, moved = 1, pinned = 2, temp_pinned = 3, unmanaged = 4 };
    constexpr uint8_t operator+ (move_state const val) { return static_cast<uint8_t>(val); }

    class managed {
        move_state _move_state;
        uint8_t _type_id;
    public:
        managed(uint8_t type_id, move_state move_state) : _move_state(move_state), _type_id(type_id) {}

        // Using atomic ref so that objects can still be moveable
        // (moving happens in strictly controlled conditions so no worries about writes to old location)
        move_state state() {
            auto ref = std::atomic_ref { _move_state };
            return ref.load(std::memory_order_relaxed);
        }
        void set_state(move_state val) {
            auto ref = std::atomic_ref { _move_state };
            ref.store(val, std::memory_order_relaxed);
        }

        uint8_t get_type_id() const { return _type_id; }
    };
}
