#pragma once
#include <algorithm>
#include <atomic>
#include <mutex>

#include "../gc-config.h"
#include "os-memory.h"
#include "../pg-meta.h"
#include "reloc-hdrs.h"

namespace gc::detail {
    // The tables indexed by page number: pg headers and reloc hdrs. Committed together up to
    // a high-water mark and never decommitted, so any address that ever held a page can be read
    class pg_tables {
        static constexpr std::size_t per_chunk = config::commit_syscall_sz / sizeof(pg_meta);
        static_assert(config::total_pages % per_chunk == 0);

        pg_meta* _hdrs;
        reloc_hdrs& _relocs;
        std::mutex _mtx;
        std::atomic<std::size_t> _committed;

    public:
        pg_tables(pg_meta* hdrs, reloc_hdrs& relocs) : _hdrs(hdrs), _relocs(relocs), _committed(0) {}

        bool ensure(std::size_t start, std::size_t n) {
            auto need = std::min(start + n + 1, config::total_pages);
            if (need <= _committed.load(std::memory_order_acquire)) return true;

            auto lk = std::lock_guard { _mtx };
            auto from = _committed.load(std::memory_order_relaxed);
            if (need <= from) return true;

            auto to = (need + per_chunk - 1) / per_chunk * per_chunk;
            if (!os::commit(_hdrs + from, (to - from) * sizeof(pg_meta))) return false;
            if (!_relocs.commit(from, to)) return false;

            _committed.store(to, std::memory_order_release);
            return true;
        }

        pg_meta* hdrs() const { return _hdrs; }
        bool pg_active(pg_meta* pg) const {
            auto off = pg - _hdrs;
            if (off < 0 || (std::size_t)off >= _committed.load(std::memory_order_acquire)) return false;
            return pg->is_active();
        }
    };
}
