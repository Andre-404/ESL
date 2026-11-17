#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <queue>
#include <span>

#include "copier.h"
#include "customization.h"
#include "ref-slot.h"

using namespace gc::detail;

void copier::gather(pg_meta* list) {
    if (!list || list->szclass() == config::large_class) return;

    auto cap = list->block_cnt();
    auto local = list_cands {};
    size_t free_blocks = 0;
    for (auto pg = list; pg; pg = pg->next()) {
        if (!pg->is_active() || pg->has_pinned()) continue;
        auto live = pg->compute_live();
        // Empty runs are retired by the pruner anyway, dense ones aren't worth moving
        if (live == 0 || live >= _evac_threshold * cap) continue;
        local.push_back({ pg, uint32_t(live) });
        free_blocks += cap - live;
    }
    // Packing all of them still wouldn't empty a single run
    if (free_blocks < cap) return;

    std::ranges::sort(local, {}, &candidate::live);
    auto lk = std::lock_guard { _mtx };
    _lists.push_back(std::move(local));
}

copy_plan copier::plan_from(std::span<const candidate> runs, size_t runs_freed) {
    auto size = runs.front().pg->block_sz();
    auto plan = copy_plan { {}, 0, runs_freed * runs.front().pg->num_pages() * config::page_sz };
    for (auto& c : runs) {
        plan.runs.push_back(c.pg);
        plan.bytes += c.live * size;
    }
    // The run order is the definition of where everything goes, address order is the natural one
    std::ranges::sort(plan.runs);
    return plan;
}

std::vector<copy_plan> copier::plans_from(const std::vector<selection>& sels) const {
    auto plans = std::vector<copy_plan> {};
    for (size_t l = 0; l < _lists.size(); ++l)
        if (sels[l].runs_freed)
            plans.push_back(plan_from(std::span { _lists[l] }.first(sels[l].runs), sels[l].runs_freed));
    return plans;
}

std::optional<copier::extension> copier::try_extend(size_t list, const selection& sel) const {
    auto& cands = _lists[list];
    const size_t cap = cands.front().pg->block_cnt(), size = cands.front().pg->block_sz();
    auto live = sel.live_blocks;
    for (auto runs = sel.runs; runs < cands.size();) {
        live += cands[runs++].live;
        if (runs - (live + cap - 1) / cap > sel.runs_freed)
            return extension { (live - sel.live_blocks) * size, list, runs - sel.runs, live };
    }
    return std::nullopt;
}

std::vector<copier::selection> copier::select_within_budget(size_t budget) const {
    auto sels = std::vector<selection>(_lists.size());
    // Note: this code assumes all runs are the same size,
    // if we start compacting large objects this no longer holds and we need to sort on free space
    auto cheapest = [](const extension& a, const extension& b) { return a.byte_cost > b.byte_cost; };
    auto exts = std::priority_queue<extension, std::vector<extension>, decltype(cheapest)> { cheapest };
 
    for (size_t l = 0; l < _lists.size(); ++l)
        if (auto e = try_extend(l, sels[l])) exts.push(*e);

    for (size_t spent = 0; !exts.empty();) {
        auto e = exts.top();
        exts.pop();
        // Every later extension of e.list builds on this one, so the list is done
        if (spent + e.byte_cost > budget) continue;
        spent += e.byte_cost;
        auto& sel = sels[e.list];
        sel = { sel.runs + e.runs_added, e.live_blocks, sel.runs_freed + 1 };
        if (auto next = try_extend(e.list, sel)) exts.push(*next);
    }
    return sels;
}

size_t copier::select(size_t budget, size_t min_gain) {
    auto plans = plans_from(select_within_budget(budget));
    size_t freed = 0;
    for (auto& p : plans) freed += p.freed;
    if (freed < min_gain) return 0;
    // Biggest first, so the threads finish close together
    std::ranges::sort(plans, std::ranges::greater {}, &copy_plan::bytes);
    _plans.fill(std::move(plans));
    return freed;
}

namespace {
    // The plan's runs read as one strip of slots. Survivor g goes to slot g: block k of run q
    class strip {
        std::span<pg_meta* const> _runs;
        size_t _size, _cap, _q, _k;
    public:
        explicit strip(std::span<pg_meta* const> runs)
            : _runs(runs), _size(runs.front()->block_sz()), _cap(runs.front()->block_cnt()), _q(0), _k(0) {}

        // The next free slot, and whether taking it moved on to the next kept run
        std::pair<uint8_t*, bool> take() {
            auto crossed = _k == _cap;
            if (crossed) { ++_q; _k = 0; }
            return { _runs[_q]->get_data() + _k++ * _size, crossed };
        }

        void remark() {
            for (size_t i = 0; i < _runs.size(); i++) {
                auto live_blks = i < _q ? _cap : i == _q ? _k : 0;
                if (live_blks) _runs[i]->mark_prefix(live_blks);
                else _runs[i]->set_active(false);
            }
        }
    };

    class page_table {
        static constexpr size_t no_page = ~size_t(0);

        reloc_hdrs& _relocs;
        size_t _size;
        size_t _page;
        size_t _split;
        size_t _first, _end;    // the slots used, from the page's first survivor to its last
        std::array<reloc_unit, reloc::max_table_units> _units;
    public:
        page_table(reloc_hdrs& relocs, size_t block_sz)
            : _relocs(relocs), _size(block_sz), _page(no_page), _split(reloc::split_none), _first(0), _end(0), _units() {}

        void update(size_t new_page, size_t new_pos, bool crossed) {
            if (new_page == _page) {
                if (crossed) [[unlikely]] _split = new_pos;
                return;
            }
            flush();
            std::fill(_units.begin() + _first, _units.begin() + _end, reloc_unit {});
            _page = new_page;
            _split = reloc::split_none;
            _first = _end = 0;
        }
        void file(size_t pos, [[maybe_unused]] uint8_t* from, uint8_t* to) {
            auto s = reloc::unit_of(pos, _split);
            if (_end == 0) _first = s;
            _end = s + 1;
            auto& u = _units[s];
            if (!u.live) u.dest = to;
            u.live |= 1ull << (pos & 63);
        }
        void flush() {
            if (_page == no_page) return;
            _relocs.set(_page, _size, _split, std::span(_units).first(_end), _first);
            _page = no_page;
        }
    };

    // Moves src's survivors into the strip's next slots and files each one in its page's table.
    // Returns the bytes moved
    size_t relocate(pg_meta* src, strip& dst, page_table& table) {
        const size_t size = src->block_sz();
        auto data = src->get_data();
        [[maybe_unused]] auto crossings = 0;

        size_t moved = 0;
        src->for_each_marked([&](uint16_t b) {
            // A block belongs to the page its first byte is in, positions count from that page's start
            auto from = data + size_t(b) * size;
            auto page = heap_layout::page_idx(from);
            auto pos = ((uint64_t)from & (gc::config::page_sz - 1)) / size;

            auto [to, crossed] = dst.take();
            table.update(page, pos, crossed);
            // An old run holds no more survivors than a kept run has blocks
            assert((crossings += crossed) <= 1);
            
            table.file(pos, from, to);

            assert(to <= from);
            if (from != to) {
                obj_copy((gc::managed*)from, (gc::managed*)to);
                moved += size;
            }
        });
        return moved;
    }
}

void copier::compact(const copy_plan& plan, reloc_hdrs& relocs) {
    auto dst = strip { plan.runs };
    auto table = page_table { relocs, plan.runs.front()->block_sz() };
    size_t moved = 0;
    for (auto run : plan.runs) moved += relocate(run, dst, table);

    table.flush();
    dst.remark();
    _moved.fetch_add(moved, std::memory_order_relaxed);
}

size_t copier::end_copy() {
    _lists.clear();
    _plans.clear();
    return _moved.exchange(0, std::memory_order_relaxed);
}

void copier::update_ptrs(pg_meta *pg) const  {
    for (; pg; pg = pg->next()) {
        if (!pg->is_active()) continue;
        auto data = pg->get_data();
        auto size = pg->block_sz();
        pg->for_each_marked([&](uint16_t b) { obj_update_ptrs((gc::managed*)(data + size_t(b) * size)); });
    }
}

void copier::update_globals(std::span<size_t*> roots) {
    // Stw, so nothing else touches the roots and relaxed in load_raw is enough
    for (auto root : roots) {
        auto ptr = to_accurate_ptr(load_raw(*root));
        if (!ptr) continue;
        store_raw(*root, ptr_to_word(forward(ptr)));
    }
}

// Nothing flips the color yet, so a bad one means a store skipped the color or r14 wasn't loaded
size_t gc::heal_ptr(size_t* slot, size_t seen) {
    fprintf(stderr, "bad pointer color in slot %p: %#zx\n", (void*)slot, seen);
    std::abort();
}
size_t gc::heal_word(size_t* slot, size_t seen) {
    fprintf(stderr, "bad word color in slot %p: %#zx\n", (void*)slot, seen);
    std::abort();
}
