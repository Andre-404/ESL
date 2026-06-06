#pragma once

#include <vector>

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
}
