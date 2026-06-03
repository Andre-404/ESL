#pragma once
#include <cstdint>
#include <atomic>

namespace gc {
    enum class obj_move_state : uint8_t { none = 0, moved = 1, pinned = 2, temp_pinned = 3, unmanaged = 4 };
    constexpr uint8_t operator+ (obj_move_state const val) { return static_cast<uint8_t>(val); }

    class managed {
        obj_move_state _move_state;
        uint8_t _type_id;
    public:
        managed(uint8_t type_id, obj_move_state move_state) : _move_state(move_state), _type_id(type_id) {}

        obj_move_state get_move_state() const { return _move_state; }

        void set_move_state(obj_move_state val) { _move_state = val; }

        uint8_t get_type_id() const { return _type_id; }
    };
}
