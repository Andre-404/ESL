#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstring>
#include <ranges>
#include <queue>
#include <span>

#include "copier.h"

using namespace gc::detail;

[[gnu::hot]] size_t count_holes(std::span<const uint64_t> s) {
    if (s.size() == 0) return 0;
    size_t starts = __builtin_popcountll(s[0] & ~(s[0] << 1));
    for (size_t i = 1; i < s.size(); i++) {
        uint64_t prev = s[i - 1] >> 63;
        starts += __builtin_popcountll(s[i] & ~((s[i] << 1) | prev));
    }
    return starts ? starts - 1 : 0;
}

void copier::gather(pg_meta* list) {
    if (!list || list->szclass() == config::large_class) return;

    auto cap = list->block_cnt();
    auto local = list_cands {};
    size_t free_blocks = 0;
    for (auto pg = list; pg; pg = pg->next()) {
        if (!pg->is_active() || pg->has_pinned()) continue;
        auto live = pg->compute_live();
        auto holes = count_holes(pg->mark_words());
        // Empty runs are retired by the pruner anyway, dense ones aren't worth moving
        if (live == 0 || live >= _evac_threshold * cap) continue;
        local.push_back({ pg, uint32_t(live), uint32_t(holes) });
        free_blocks += cap - live;
    }
    // Packing all of them still wouldn't empty a single run
    if (free_blocks < cap) return;

    std::ranges::sort(local, {}, &candidate::live);
    auto lk = std::lock_guard { _mtx };
    _clists.push_back(std::move(local));
}

std::optional<copier::extension> copier::try_extend(size_t list, const selection& sel) const {
    auto& cands = _clists[list];
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
    auto sels = std::vector<selection>(_clists.size());
    // Note: this code assumes all runs are the same size,
    // if we start compacting large objects this no longer holds and we need to sort on free space
    auto cheapest = [](const extension& a, const extension& b) { return a.byte_cost > b.byte_cost; };
    auto exts = std::priority_queue<extension, std::vector<extension>, decltype(cheapest)> { cheapest };
 
    for (size_t l = 0; l < _clists.size(); ++l)
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

// Every stride-th run from the offset-th sparsest, so dense and sparse runs spread evenly over the plans
std::optional<copy_plan> copier::plan_from(std::span<const candidate> runs, size_t offset, size_t stride) {
    const size_t cap = runs.front().pg->block_cnt(), size = runs.front().pg->block_sz();
    size_t n = 0, live = 0, holes = 0;
    for (auto i = offset; i < runs.size(); i += stride) ++n, live += runs[i].live, holes += runs[i].holes;
    auto freed = n - (live + cap - 1) / cap;
    if (!freed && holes < config::copy_hole_threshold * (n * cap - live)) return std::nullopt;
    auto plan = copy_plan { {}, live * size, freed * runs.front().pg->num_pages() * config::page_sz };
    plan.runs.reserve(n);
    for (auto i = offset; i < runs.size(); i += stride) plan.runs.push_back(runs[i].pg);
    std::ranges::sort(plan.runs);
    return plan;
}


std::vector<copy_plan> copier::plans_from(const std::vector<selection>& sels, size_t threads) const {
    size_t total = 0;
    for (size_t l = 0; l < _clists.size(); ++l)
        if (sels[l].runs_freed) total += sels[l].live_blocks * _clists[l].front().pg->block_sz();

    auto plans = std::vector<copy_plan> {};
    plans.reserve(_clists.size() + threads);
    for (size_t l = 0; l < _clists.size(); ++l) {
        auto& sel = sels[l];
        if (!sel.runs_freed) continue;
        auto bytes = sel.live_blocks * _clists[l].front().pg->block_sz();
        auto stride = std::clamp((bytes * threads + total - 1) / total, size_t(1), sel.runs_freed);
        for (size_t offset = 0; offset < stride; ++offset)
            if (auto plan = plan_from(std::span { _clists[l] }.first(sel.runs), offset, stride))
                plans.push_back(std::move(*plan));
    }
    return plans;
}


size_t copier::select(size_t budget, size_t min_gain, size_t threads) {
    auto plans = plans_from(select_within_budget(budget), threads);
    size_t freed = 0;
    for (auto& p : plans) freed += p.freed;
    if (freed < min_gain) return 0;
    // Biggest first, so the threads finish close together
    std::ranges::sort(plans, std::ranges::greater {}, &copy_plan::bytes);
    _plans.fill(std::move(plans));
    return freed;
}

namespace {
    constexpr size_t prefetch_ahead = 4096;
    constexpr size_t big_block = 512;

    // The plan's runs read as one strip of slots. Survivor g goes to slot g: block k of run q
    class strip {
        std::span<pg_meta* const> _runs;
        size_t _size, _cap, _q, _k;
    public:
        explicit strip(std::span<pg_meta* const> runs)
            : _runs(runs), _size(runs.front()->block_sz()), _cap(runs.front()->block_cnt()), _q(0), _k(0) {}

        // The free slots left in the current kept run
        size_t room() const { return _cap - _k; }
        // The first of the next n slots, all of them in the current kept run
        uint8_t* take(size_t n) {
            assert(n <= room());
            auto at = _runs[_q]->get_data() + _k * _size;
            _k += n;
            return at;
        }
        // On to the next kept run
        void cross() { ++_q; _k = 0; }

        void remark() {
            for (size_t i = 0; i < _runs.size(); i++) {
                auto live_blks = i < _q ? _cap : i == _q ? _k : 0;
                if (live_blks) _runs[i]->mark_prefix(live_blks);
                else _runs[i]->set_active(false);
            }
        }
    };

    // Reloc table of the old run: unit u holds its blocks [64u, 64u + 64), the units past the split one later
    class run_table {
        reloc_hdrs& _relocs;
        size_t _page, _size;
        size_t _split;
        size_t _first, _end;
        std::array<reloc_unit, reloc::max_table_units> _units;
    public:
        run_table(reloc_hdrs& relocs, pg_meta* run)
            : _relocs(relocs), _page(heap_layout::page_idx(run->get_data())), _size(run->block_sz()),
              _split(reloc::split_none), _first(0), _end(0), _units() {}

        void split(size_t pos) {
            assert(_split == reloc::split_none && "an old run's survivors cross into the next kept run at most once");
            _split = pos;
        }
        // Pos is the start of a (potential) run on live objects that go into dest
        void file(size_t pos, uint64_t live, uint8_t* dest) {
            auto s = reloc::unit_of(pos, _split);
            if (_end == 0) _first = s;
            _end = s + 1;
            auto& u = _units[s];
            if (!u.live) u.dest = dest;
            u.live |= live;
        }
        void flush() {
            if (_end)
                _relocs.set(_page, _size, _split, std::span(_units).first(_end), _first);
        }
    };

    void prefetch(const uint8_t* run, size_t w, size_t bsz) {
        for (auto m = w; m; m &= m - 1)
            for (size_t o = 0; o < bsz; o += 64)
                __builtin_prefetch(run + std::countr_zero(m) * bsz + o);
    }

    // Moves the survivors set in live, counted from block b, to consecutive slots from to. A block never
    // overlaps its new home (both are block aligned, to < from). Returns the bytes moved
    size_t move(const pg_meta* run, size_t b, uint64_t live, uint8_t* to) {
        size_t moved = 0;
        auto bsz = run->block_sz();
        auto data = run->get_data();
        for (; live; live &= live - 1, to += bsz) {
            auto from = run->get_data() + (b + std::countr_zero(live)) * bsz;
            assert(to <= from);
            if (from == to) continue;
            // Size classes are multiples of 16, so small blocks need no call to pick a path by size
            if (bsz > big_block) std::memcpy(to, from, bsz);
            else for (size_t o = 0; o < bsz; o += 16) std::memcpy(to + o, from + o, 16);
            moved += bsz;
        }
        return moved;
    }

    // Moves the survivors set in live, counted from block b, to the strip's next slots and files them.
    // Returns the bytes moved
    size_t place(const pg_meta* run, size_t b, uint64_t live, strip& dst, run_table& table) {
        auto put = [&](uint64_t bits) {
            auto to = dst.take(std::popcount(bits));
            table.file(b + std::countr_zero(bits), bits, to);
            return move(run, b, bits, to);
        };
        size_t moved = 0;
        // If there is more in this chunk than there is room, split it to 2 units
        if (auto room = dst.room(); size_t(std::popcount(live)) > room) {
            // The kept run fills up, the rest start the next one and a unit of their own
            auto rest = live;
            for (; room; --room) rest &= rest - 1;
            if (rest != live) moved += put(live ^ rest);
            dst.cross();
            table.split(b + std::countr_zero(rest));
            live = rest;
        }
        return moved + put(live);
    }

    // Moves src's survivors into the strip's next slots and files them, a mark word at a time.
    // Returns the bytes moved
    size_t relocate(pg_meta* src, strip& dst, reloc_hdrs& relocs) {
        // In whole mark words, big blocks are left to the hardware prefetcher
        auto bsz = src->block_sz();
        const auto ahead = bsz > big_block ? 0 : std::max<size_t>(1, prefetch_ahead / (64 * bsz));
        auto table = run_table { relocs, src };
        size_t moved = 0;
        auto mark = src->mark_words();
        for (auto [i, live] : std::views::enumerate(mark)) {
            if (ahead && i + ahead < mark.size())
                prefetch(src->get_data() + (i + ahead) * 64 * bsz, mark[i + ahead], bsz);
            if (live) moved += place(src, i * 64, live, dst, table);
        }
        table.flush();
        return moved;
    }
}

void copier::compact(const copy_plan& plan, reloc_hdrs& relocs) {
    auto dst = strip { plan.runs };
    size_t moved = 0;
    for (auto run : plan.runs) moved += relocate(run, dst, relocs);

    dst.remark();
    _moved.fetch_add(moved, std::memory_order_relaxed);
}

size_t copier::end_copy() {
    _clists.clear();
    _plans.clear();
    return _moved.exchange(0, std::memory_order_relaxed);
}
