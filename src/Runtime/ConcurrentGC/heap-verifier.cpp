#include "heap-verifier.h"

#include <bit>
#include <cstdlib>
#include <cstring>
#include <ranges>

#include "../../Includes/fmt/core.h"
#include "customization.h"

using namespace gc;
using namespace gc::detail;

template<typename F>
static void for_each_bit(size_t word, size_t base, F fn) {
    for (; word; word &= word - 1) fn(base + std::countr_zero(word));
}

std::vector<violation> heap_verifier::check() {
    std::vector<violation> found;
    auto check_ref = [&](managed* holder, managed* ref) {
        // Constants compiled into the program live outside the heap
        if (!heap_layout::in_heap(ref)) return;
        auto pg = _pgs.pg_from_ptr(ref);
        if (!pg || pg->from_interior((uint8_t*)ref) != ref)
            found.push_back({ violation::not_a_block, holder, ref });
        else if (!pg->is_marked(ref))
            found.push_back({ violation::unmarked, holder, ref });
    };
    for (auto run : _pgs.runs()) {
        auto marks = run->mark_words();
        for (size_t w = 0; w < marks.size(); w++)
            for_each_bit(marks[w], w * 64, [&](size_t i) {
                auto obj = (managed*)(run->get_data() + i * run->block_sz());
                if (!(run->load_alloc_word(i) & (1ull << i % 64)))
                    found.push_back({ violation::unallocated, obj, nullptr });
                else if (obj_traceable(obj))
                    obj_trace(obj, [&](managed* ref) { check_ref(obj, ref); });
            });
    }
    return found;
}

void heap_verifier::poison() {
    for (auto run : _pgs.runs()) {
        auto marks = run->mark_words();
        for (size_t w = 0; w < marks.size(); w++)
            for_each_bit(run->load_alloc_word(w * 64) & ~marks[w], w * 64, [&](size_t i) {
                std::memset(run->get_data() + i * run->block_sz(), poison_byte, run->block_sz());
            });
    }
}

void heap_verifier::after_mark() {
    if (!_on) return;
    auto found = check();
    if (!found.empty()) {
        static constexpr const char* what[] = { "unmarked", "not a block", "marked but unallocated" };
        fmt::print(stderr, "heap verifier: {} violations after the mark\n", found.size());
        for (auto& v : found | std::views::take(16))
            fmt::print(stderr, "heap verifier: {} -> {}: {}\n", (void*)v.holder, v.ref, what[v.what]);
        std::abort();
    }
    poison();
}
