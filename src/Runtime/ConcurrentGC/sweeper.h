#pragma once
#include <functional>

#include "customization.h"
#include "pg-meta.h"

namespace gc {
    class sweeper {
    public:
        sweeper() {};

        void sweep_pg(pg_meta* pg) const {
            auto iter = pg_meta::pg_slots_iter { pg, 0 };
            while (!iter.at_end()) {
                auto obj = iter.get();
                obj_destroy(obj);
                iter.next();
            }
        }
    };
}