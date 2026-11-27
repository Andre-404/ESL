#pragma once
#include <vector>

#include "managed.h"
#include "pg-manager.h"

namespace gc::detail {
    struct violation {
        enum kind : uint8_t {
            unmarked,     // holder points at an allocated block the mark missed
            not_a_block,  // holder points at something that isn't the start of an allocated block
            unallocated,  // holder is marked but its alloc bit is clear, ref is null
        };
        kind what;
        managed* holder;
        void* ref;
        bool operator==(const violation&) const = default;
    };

    class heap_verifier {
        bool _on;
        pg_manager& _pgs;

        void poison();
    public:
        static constexpr uint8_t poison_byte = 0xDB;

        heap_verifier(bool on, pg_manager& pgs) : _on(on), _pgs(pgs) {}
        // Every violation, for tests
        std::vector<violation> check();
        // Aborts if check finds anything, then poisons the dead blocks
        void after_mark();
    };
}
