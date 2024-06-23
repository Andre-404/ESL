#pragma once

#include <bitset>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using std::string;
using std::unordered_map;
using std::vector;

typedef unsigned int uInt;
typedef unsigned long long uInt64;
typedef unsigned short uInt16;
typedef unsigned char byte;
typedef uint64_t Value;

// Using epsilon value because of floating point precision
#define DBL_EPSILON                                                            \
  2.2204460492503131e-016 // smallest such that 1.0+DBL_EPSILON != 1.0
#define FLOAT_EQ(x, v) (fabs(x - v) <= DBL_EPSILON)
#define LOCAL_MAX 256
#define UPVAL_MAX 256
#define PAGE_SIZE 64 * 1024

#ifdef _MSC_VER
#define NOINLINE __declspec(noinline)
#else
#define NOINLINE __attribute__((noinline))
#endif

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __attribute__((visibility("default")))
#endif

// #define AST_DEBUG
#define COMPILER_DEBUG
#define DEBUG_MODE
// #define COMPILER_USE_LONG_INSTRUCTION
// #define DEBUG_TRACE_EXECUTION
//#define GC_DEBUG
