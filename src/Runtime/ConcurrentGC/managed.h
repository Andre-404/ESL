#pragma once
#include <cstdint>
#include <atomic>

namespace gc {
    enum class move_state : uint8_t { none = 0, moved = 1, pinned = 2, temp_pinned = 3, unmanaged = 4 };
    constexpr uint8_t operator+ (move_state const val) { return static_cast<uint8_t>(val); }

    // Using atomic ref on move_state so that objects can still be moveable
    // (moving happens in strictly controlled conditions so no worries about writes to old location)
    class managed {
        move_state _state;
        uint8_t _type_id;

        auto sref() { return std::atomic_ref { _state }; }
    public:
        managed(uint8_t type_id, move_state move_state) : _type_id(type_id) {
            sref().store(move_state, std::memory_order_release);
        }

        move_state state() {
            return sref().load(std::memory_order_acquire);
        }
        void set_state(move_state val) {
            sref().store(val, std::memory_order_release);
        }

        uint8_t get_type_id() const { return _type_id; }
    };
}
