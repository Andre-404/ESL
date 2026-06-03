#include <span>

#include "copier.h"
#include "pg-meta.h"
#include "customization.h"

using namespace gc::detail;

struct pg_slot { gc::managed* obj; bool is_pod; bool marked; };

class pg_list_slot_iter {
    std::span<pg_meta*> _pages;
    size_t _cnt;
    pg_meta::pg_slots_iter _cur_iter;
public:
    explicit pg_list_slot_iter(std::span<pg_meta*> pages)
        : _pages(pages), _cnt(0), _cur_iter(_pages.front(), (uint16_t)0) {}

    pg_list_slot_iter& operator++() {
        _cur_iter.next();
        if (_cur_iter.at_end() && ++_cnt < _pages.size())
            _cur_iter = pg_meta::pg_slots_iter { _pages[_cnt], (uint16_t)0 };
        return *this;
    }

    pg_slot operator*() const { return { _cur_iter.get(), _pages[_cnt]->is_pod(), _cur_iter.is_marked() }; }
    bool operator==(std::default_sentinel_t) const { return _cnt == _pages.size(); }

    pg_list_slot_iter& begin() { return *this; }
    std::default_sentinel_t end() const { return {}; }

    pg_meta* get_pg() const { return _pages[_cnt]; }
};

void copier::copy_objects(pg_meta *pg_list) const {
    auto [target, source] = split_pages(pg_list);
    if (target.empty() || source.empty()) return;

    auto target_iter = pg_list_slot_iter { target };

    for (auto [src, src_is_pod, marked] : pg_list_slot_iter { source }) {
        if (!marked) {
            if (!src_is_pod) obj_destroy(src);
            continue;
        }

        while ((*target_iter).marked) ++target_iter;

        auto [dest, dest_is_pod, _] = *target_iter;
        if (!dest_is_pod) obj_destroy(dest);
        obj_copy(src, dest);
        src->set_move_state(obj_move_state::moved);
        *(size_t*)src |= (size_t)dest & 0xffffffffffff; // Cut the top 16 bits, overwrite stale data of object that was moved
        // Need to update the mark bitmap with the new object
        target_iter.get_pg()->record_mark(dest, false);
    }
    for (auto pg : source) pg->reset_trackers();
}

void copier::update_ptrs(pg_meta *pg) const  {
    if (pg->live_count() == 0) return;

    auto iter = pg_meta::pg_slots_iter { pg, (uint16_t)0 };

    while (!iter.at_end()) {
        auto obj = iter.get();
        if (!iter.is_marked()) continue;
        obj_update_ptrs(obj);
        iter.next();
    }
}

std::pair<std::vector<pg_meta *>, std::vector<pg_meta *> > copier::split_pages(pg_meta *pg_list) const {
    auto target = std::vector<pg_meta*> {};
    auto source = std::vector<pg_meta*> {};
    size_t needed_space = 0;
    // Pages that have pinned objects and that are above threshold(but not completely full) become targets
    for (auto pg = pg_list; pg; pg = pg->next()) {
        const auto live = pg->live_count();
        const auto cap  = pg->block_cnt();
        if (pg->has_pinned() || live >= _evac_threshold*cap) {
            if (live < cap) target.push_back(pg);
            needed_space -= cap - live;
        } else {
            needed_space += live;
            if (live > 0) source.push_back(pg);
        }
    }
    std::ranges::sort(source, {}, &pg_meta::live_count);
    while (needed_space > 0) {
        auto pg = source.back();
        source.pop_back();
        needed_space -= pg->block_cnt();
        target.push_back(pg);
    }
    std::ranges::sort(target);
    std::ranges::sort(source);
    return { std::move(target), std::move(source) };
}
