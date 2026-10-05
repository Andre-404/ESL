#pragma once

#include "gc-config.h"
#include "pg-meta.h"
#include "survival-model.h"

namespace gc::detail {
    // Cycle-start pass over one page list, outside the pause and on the list's owning thread
    // It records page survival samples and on copying cycles nominates the pages it predicts are sparse:
    //     pred_live = occ * survival(age),  nominate when pred_live < threshold * cap
    inline void update_page_age(pg_meta* list, const survival_model& rates, bool nominate) {
        for (auto pg = list; pg; pg = pg->next()) {
            if (!pg->is_active()) continue;
            auto occ = uint16_t(pg->compute_alloc());
            auto age = pg->history().age();
            pg->set_history({ occ, age });
            // Large objects never move, and occ == 0 is either an empty page the sweep will
            // retire or the one the allocator is filling right now - a target either way
            if (!nominate || pg->szclass() == config::large_class || occ == 0) continue;
            if (occ * rates.survival(age) < config::nominate_threshold * pg->block_cnt())
                pg->nominate();
        }
    }
}
