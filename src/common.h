#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <iostream>

using std::string;
using std::vector;
using std::unordered_map;

typedef unsigned int uInt;
typedef unsigned long long uInt64;
typedef unsigned short uInt16;
typedef unsigned char byte;

//Using epsilon value because of floating point precision
#define DBL_EPSILON      2.2204460492503131e-016 // smallest such that 1.0+DBL_EPSILON != 1.0
#define FLOAT_EQ(x,v) (fabs(x - v) <= DBL_EPSILON)
#define IS_INT(num) (FLOAT_EQ(std::floor(num), num))

#define AST_DEBUG
#define COMPILER_DEBUG
//#define COMPILER_USE_LONG_INSTRUCTION
#define GC_PRINT_HEAP
