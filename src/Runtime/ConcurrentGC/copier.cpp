#include <span>
#include <cassert>
#include <algorithm>

#include "copier.h"
#include "pg-meta.h"
#include "customization.h"

using namespace gc::detail;

struct pg_slot { gc::managed* obj; bool marked; bool dirty; };

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

    pg_slot operator*() const { return { _cur_iter.get(), _cur_iter.is_marked(), _cur_iter.is_dirty() }; }
    bool operator==(std::default_sentinel_t) const { return _cnt == _pages.size(); }

    pg_list_slot_iter& begin() { return *this; }
    std::default_sentinel_t end() const { return {}; }

    void set_marked() { _cur_iter.set_marked(); }
    void set_dirty() { _cur_iter.set_dirty(); }
};


void copier::copy_objects(pg_meta *pg_list) const {
    auto [target, source] = split_pages(pg_list);
    if (target.empty() || source.empty()) return;

    auto target_iter = pg_list_slot_iter { target };

    for (auto [src, marked, dirty] : pg_list_slot_iter { source }) {
        if (!marked) continue;

        while ((*target_iter).marked) {
            // Should never be hit since split pages guarantees enough room for copying
            assert(target_iter != target_iter.end());
            ++target_iter;
        }

        auto dest = (*target_iter).obj;
        obj_copy(src, dest);
        set_moved(src, dest);
        // Need to update the mark and copy bitmap with the new object
        target_iter.set_marked();
        // While marking is unconditional (only a live object gets copied, copy card dirtying isnt)
        if (dirty)
            target_iter.set_dirty();
    }
    // Source pages are now full of dead objects
    for (auto pg : source) pg->mark_inactive();
}

void copier::update_ptrs(pg_meta *pg) const  {
    for (; pg; pg = pg->next()) {
        if (!pg->is_active() || !pg->any_dirty()) continue;
        for (auto it = pg_meta::pg_slots_iter { pg, 0 }; !it.at_end(); it.next()) {
            if (!it.is_marked() || !it.is_dirty()) continue;
            obj_update_ptrs(it.get());
        }
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
    struct candidate { pg_meta* pg; uint16_t live; };

    auto target = std::vector<pg_meta*> {};
    auto source = std::vector<candidate> {};
    int64_t needed_space = 0;
    for (auto pg = pg_list; pg; pg = pg->next()) {
        const auto live = pg->compute_live();
        const auto cap  = pg->block_cnt();
        assert(live <= cap);
        // Dont fill already empty pages, we want to hand those back
        if (live == 0) continue;
        // Nomination ran before we had exact numbers, a predicted sparse page that turned out
        // to be dense gets demoted (no point in compacting already compact things)
        if (!pg->is_source() || live >= _evac_threshold * cap) {
            pg->demote();
            if (live < cap) target.push_back(pg);
            needed_space -= cap - live;
            continue;
        }
        needed_space += live;
        source.push_back({ pg, (uint16_t)live });
    }
    // Give up the densest pages first (we get the least benefit from copying them)
    if (needed_space > 0)
        std::ranges::sort(source, {}, &candidate::live);
    while (needed_space > 0) {
        auto [pg, live] = source.back();
        source.pop_back();
        pg->demote();
        needed_space -= pg->block_cnt();
        target.push_back(pg);
    }

    auto sources = std::vector<pg_meta*> {};
    sources.reserve(source.size());
    for (auto [pg, live] : source) sources.push_back(pg);

    std::ranges::sort(target);
    std::ranges::sort(sources);
    return { std::move(target), std::move(sources) };
}
