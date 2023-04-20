#pragma once
#include "../common.h"
#include "../Codegen/codegenDefs.h"

int disassembleInstruction(Chunk* chunk, int offset, int constantsOffset);