#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <iostream>
#include <bitset>

using std::string;
using std::vector;
using std::unordered_map;

typedef unsigned int uInt;
typedef unsigned long long uInt64;
typedef unsigned short uInt16;
typedef unsigned char byte;
typedef uint64_t Value;

//Using epsilon value because of floating point precision
#define DBL_EPSILON      2.2204460492503131e-016 // smallest such that 1.0+DBL_EPSILON != 1.0
#define FLOAT_EQ(x,v) (fabs(x - v) <= DBL_EPSILON)
#define LOCAL_MAX 256
#define UPVAL_MAX 256

//#define AST_DEBUG
#define COMPILER_DEBUG
//#define DEBUG_MODE
//#define COMPILER_USE_LONG_INSTRUCTION
//#define DEBUG_TRACE_EXECUTION
