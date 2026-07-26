#include <span>
#include <cassert>
#include <algorithm>

#include "copier.h"
#include "pg-meta.h"
#include "customization.h"

using namespace gc::detail;

struct pg_slot { gc::managed* obj; bool marked; };

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
            _cur_iter = pg_meta::pg_slots_iter { _pages[_cnt], 0 };
        return *this;
    }

    pg_slot operator*() const { return { _cur_iter.get(), _cur_iter.is_marked() }; }
    bool operator==(std::default_sentinel_t) const { return _cnt == _pages.size(); }

    pg_list_slot_iter& begin() { return *this; }
    std::default_sentinel_t end() const { return {}; }

    pg_meta* get_pg() const { return _pages[_cnt]; }
};


void copier::copy_objects(pg_meta *pg_list) const {
    auto [target, source] = split_pages(pg_list);
    if (target.empty() || source.empty()) return;

    auto target_iter = pg_list_slot_iter { target };

    for (auto [src, marked] : pg_list_slot_iter { source }) {
        if (!marked) continue;

        while ((*target_iter).marked) {
            // Should never be hit since split pages guarantees enough room for copying
            assert(target_iter != target_iter.end());
            ++target_iter;
        }

        auto [dest, _] = *target_iter;
        obj_copy(src, dest);
        set_moved(src, dest);
        // Need to update the mark bitmap with the new object
        target_iter.get_pg()->record_mark(dest, false);
    }
    // Clear both bitmaps so that pruner correctly deduces compute_live -> 0
    for (auto pg : source) pg->recycle();
}

void copier::update_ptrs(pg_meta *pg) const  {
    while (pg) {
        if (pg->live_count() == 0) {
            pg = pg->next();
            continue;
        }
        auto iter = pg_meta::pg_slots_iter { pg, 0 };

        while (!iter.at_end()) {
            auto obj = iter.get();
            if (iter.is_marked()) {
                if (obj->state() == move_state::temp_pinned) obj->set_state(move_state::none);
                obj_update_ptrs(obj);
            }
            iter.next();
        }
        pg = pg->next();
    }
}

void copier::update_globals(std::span<size_t*> roots) {
    for (auto root : roots) {
        auto ptr = to_accurate_ptr(*root);
        if (!ptr) continue;
        ptr = get_moved(ptr);
        *root = ptr_to_word(ptr);
    }
}

std::pair<std::vector<pg_meta *>, std::vector<pg_meta *> > copier::split_pages(pg_meta *pg_list) const {
    auto target = std::vector<pg_meta*> {};
    auto source = std::vector<pg_meta*> {};
    int64_t needed_space = 0;
    // Pages that have pinned objects and that are above threshold(but not completely full) become targets
    for (auto pg = pg_list; pg; pg = pg->next()) {
        pg->compute_live();
        const auto live = pg->live_count();
        const auto cap  = pg->block_cnt();
        if (pg->has_pinned() || live >= _evac_threshold*cap) {
            if (live < cap) target.push_back(pg);
            needed_space -= cap - live;
            assert(live <= cap);
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
