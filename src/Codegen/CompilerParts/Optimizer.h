#pragma once
#include "llvm/IR/Module.h"

// LLVM's O3 pipeline, then the GC passes that have to run on its output
void optimize(llvm::Module& module);
