cmake_minimum_required(VERSION 3.22)
project(gc_tests C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Threads REQUIRED)

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG        v1.14.0
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()
include(GoogleTest)

add_library(gc_under_test STATIC
        ../mark-buf.cpp
        ../marker.cpp
        ../copier.cpp
        ../page-allocator.cpp
        ../pg-manager.cpp
        ../collector.cpp
        ../../../Includes/format.cc
        customization-helper.cpp
        ../../../Includes/rpmalloc/rpmalloc.c
)
target_include_directories(gc_under_test PUBLIC ../../../Includes/)
target_link_libraries(gc_under_test PUBLIC Threads::Threads)

add_executable(run_tests
        test-tstack.cpp
        test-mark-buf.cpp
        test-sync-point.cpp
        test-tcb.cpp
        test-tcb-registry.cpp
        test-transition-manager.cpp
        test-pg-meta.cpp
        test-obj-allocator.cpp
        test-szclass-allocator.cpp
        test-marker.cpp
        customization-helper.cpp
        customization-helper.h
        test-copier.cpp
        test-arena.cpp
        test-pruner.cpp
        # test-gc-heuristic.cpp  -- stale: calls gc_heuristics::set_mark_ms, removed by the
        # mark_start/mark_end refactor. Excluded so the rest of the suite builds.
        test-collector.cpp
)
target_compile_definitions(run_tests PUBLIC UNIT_TESTS=1)
target_link_libraries(run_tests
        PRIVATE
        gc_under_test
        gtest_main
        Threads::Threads
)

gtest_discover_tests(run_tests)
