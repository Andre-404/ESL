#pragma once

#include "memoryPool.h"

class ThreadArena {
public:
    void *alloc();
};