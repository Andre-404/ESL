#pragma once

#include <vector>
#include <span>

#include "managed.h"
#include "pg-meta.h"

namespace gc::detail {
    class pg_meta;
    // Reduces fragmentation through copying and resets color byte to first epoch
    class copier {
        double _evac_threshold;

        std::pair<std::vector<pg_meta*>, std::vector<pg_meta*>> split_pages(pg_meta* pg_list) const;

    public:
        copier(double evac_threshold) : _evac_threshold(evac_threshold) {}

        void copy_objects(pg_meta* pg_list) const;

        void update_ptrs(pg_meta* pg) const;
        void update_globals(std::span<size_t*> roots);
    };

    // Relies on little endian
    inline void set_moved(managed* src, managed* dest) {
        // Save the bottom 16 bits so that we can read the move correctly
        src->set_state(move_state::moved);
        auto& w = *(size_t*)src;
        w = (w & 0x000000000000ffffull) | ((size_t)dest << 16);
    }

    inline managed* get_moved(managed* obj) {
        if (pg_meta::pg_from_ptr(obj)->is_active() || obj->state() != move_state::moved)
            return obj;
        auto w = *(size_t*)obj;
        w = w >> 16;
        return (managed*)w;
    }
}
