cmake_minimum_required(VERSION 3.25)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    project(gc_tests C CXX ASM)
    list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../../cmake")
    include(ESLBuildOptions)
    esl_use_mold()
    # Must run before googletest is fetched so it gets instrumented too.
    esl_apply_sanitizer()
endif()

set(CMAKE_CXX_STANDARD 23)
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

include(${CMAKE_CURRENT_SOURCE_DIR}/../platforms/abi/abi.cmake)
add_library(gc_under_test STATIC
        ../mark-buf.cpp
        ../marker.cpp
        ../copier.cpp
        ../ref-slot.cpp
        ../os/os-memory.cpp
        ../os/os-time.cpp
        ../allocator/gc-bits.cpp
        ../allocator/span-allocator.cpp
        ../allocator/pg-allocator.cpp
        ../allocator/scavenger.cpp
        ../pg-manager.cpp
        ../collector.cpp
        ../cleanups.cpp
        ../gc-api.cpp
        ${GC_ABI_SOURCE}
        ../../../Includes/format.cc
        customization-helper.cpp
        ../../../Includes/rpmalloc/rpmalloc.c
)
target_include_directories(gc_under_test PUBLIC ../../../Includes/)
target_link_libraries(gc_under_test PUBLIC Threads::Threads)
# As in the top-level CMakeLists: no malloc override
set_source_files_properties(../../../Includes/rpmalloc/rpmalloc.c PROPERTIES COMPILE_DEFINITIONS ENABLE_OVERRIDE=0)

# PUBLIC, and on the library rather than the executable: platform-specific.h
# selects platforms/testing.h on this define, and the library and the tests
# must inline the same platform functions.
target_compile_definitions(gc_under_test PUBLIC UNIT_TESTS=1)

add_executable(run_tests
        test-tstack.cpp
        test-mark-buf.cpp
        test-sync-point.cpp
        test-tcb.cpp
        test-tcb-registry.cpp
        test-transition-manager.cpp
        test-pg-meta.cpp
        test-pg-list.cpp
        test-obj-allocator.cpp
        test-szclass-allocator.cpp
        test-marker.cpp
        customization-helper.cpp
        customization-helper.h
        test-copier.cpp
        test-arena.cpp
        test-pruner.cpp
        test-radix-tree.cpp
        test-bitmap.cpp
        test-span-allocator.cpp
        test-gc-bits.cpp
        test-gc-heuristic.cpp
        test-collector.cpp
        test-cleanups.cpp
        test-ref-slot.cpp
        test-gc-api.cpp
)
target_link_libraries(run_tests
        PRIVATE
        gc_under_test
        gtest_main
        Threads::Threads
)

esl_sanitizer_test_launcher(run_tests)
gtest_discover_tests(run_tests)
