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
// We use this macro for metaprogramming related to memory pools.
#define MP_CNT 6
#define PAGE_SIZE (64 * 1024)

// Oh yeah baby, the loops be looping
#define M_LOOP_1(X, S)  X(S)
#define M_LOOP_2(X, S)  X(S) M_LOOP_1(X, S+1)
#define M_LOOP_3(X, S)  X(S) M_LOOP_2(X, S+1)
#define M_LOOP_4(X, S)  X(S) M_LOOP_3(X, S+1)
#define M_LOOP_5(X, S)  X(S) M_LOOP_4(X, S+1)
#define M_LOOP_6(X, S)  X(S) M_LOOP_5(X, S+1)
#define M_LOOP_7(X, S)  X(S) M_LOOP_6(X, S+1)
#define M_LOOP_8(X, S)  X(S) M_LOOP_7(X, S+1)
#define M_LOOP_9(X, S)  X(S) M_LOOP_8(X, S+1)
#define M_LOOP_(N, X, S) M_LOOP_ ## N(X, S)
#define M_LOOP(N, X, S) M_LOOP_(N, X, S)

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
#define GC_DEBUG
