# Build knobs shared by the top level project and the standalone GC test build.
#
# The GC tests can be configured two ways: as a subdirectory of the top level
# project, or directly against src/Runtime/ConcurrentGC/tests. Both include this
# file so a sanitizer or linker choice means the same thing either way.
include_guard(GLOBAL)

set(ESL_SANITIZER "none" CACHE STRING
    "Sanitizer to build with: none, address, thread, undefined, address+undefined")
set_property(CACHE ESL_SANITIZER PROPERTY STRINGS
    none address thread undefined address+undefined)

# Applies the selected sanitizer to every target created after this point in the
# current directory and below, including FetchContent dependencies such as
# googletest. Instrumenting everything keeps thread sanitizer from reporting
# races it cannot see both sides of.
function(esl_apply_sanitizer)
    if(ESL_SANITIZER STREQUAL "none")
        return()
    endif()

    if(ESL_SANITIZER STREQUAL "address")
        set(_flags -fsanitize=address)
    elseif(ESL_SANITIZER STREQUAL "thread")
        set(_flags -fsanitize=thread)
    elseif(ESL_SANITIZER STREQUAL "undefined")
        # Recovering would let a test pass after reporting; make the first report fatal.
        set(_flags -fsanitize=undefined -fno-sanitize-recover=undefined)
    elseif(ESL_SANITIZER STREQUAL "address+undefined")
        set(_flags -fsanitize=address,undefined -fno-sanitize-recover=undefined)
    else()
        message(FATAL_ERROR "ESL_SANITIZER: unknown value '${ESL_SANITIZER}'")
    endif()

    # -O1 keeps the sanitized build usable without optimizing away the frames the
    # reports are built from.
    add_compile_options(${_flags} -fno-omit-frame-pointer -g)
    add_link_options(${_flags})

    message(STATUS "Sanitizer: ${ESL_SANITIZER}")
endfunction()

# GCC only accepts the bare names bfd/gold/lld/mold for -fuse-ld, and it looks
# them up through the compiler's -B search path rather than PATH. /usr/bin is
# already on that path, but a mold installed from source lands somewhere the
# driver will not look, so point -B at whichever directory actually holds the
# ld.mold we found. Falls back to the default linker when mold is absent.
function(esl_use_mold)
    if(WIN32)
        return()
    endif()

    find_program(ESL_MOLD_EXECUTABLE
        NAMES ld.mold
        HINTS /usr/local/bin /usr/bin /opt/mold/bin
        DOC "ld.mold used to link ESL")

    if(NOT ESL_MOLD_EXECUTABLE)
        message(STATUS "Linker: system default (no ld.mold found)")
        return()
    endif()

    cmake_path(GET ESL_MOLD_EXECUTABLE PARENT_PATH _mold_dir)
    add_link_options(-B${_mold_dir} -fuse-ld=mold)
    message(STATUS "Linker: ${ESL_MOLD_EXECUTABLE}")
endfunction()

# Runs a sanitized test binary with ASLR disabled.
#
# Thread sanitizer maps its shadow memory at fixed addresses and aborts with
# "unexpected memory mapping" when the loader places the binary outside the range
# it expects. Ubuntu 24.04 ships vm.mmap_rnd_bits=32, which is enough entropy to
# trigger that on every run. Lowering the sysctl to 28 fixes it system wide;
# setarch -R fixes it for this binary without needing root. Harmless for the
# other sanitizers, and it makes their reports reproducible run to run.
#
# gtest_discover_tests reads CROSSCOMPILING_EMULATOR, so this covers both the
# discovery pass at build time and the tests ctest runs.
function(esl_sanitizer_test_launcher target)
    if(ESL_SANITIZER STREQUAL "none" OR NOT UNIX OR APPLE)
        return()
    endif()

    find_program(ESL_SETARCH_EXECUTABLE setarch)
    if(NOT ESL_SETARCH_EXECUTABLE)
        message(WARNING
            "setarch not found; sanitized tests run with ASLR on. If thread "
            "sanitizer aborts with 'unexpected memory mapping', lower "
            "vm.mmap_rnd_bits to 28.")
        return()
    endif()

    set_property(TARGET ${target} PROPERTY CROSSCOMPILING_EMULATOR
        "${ESL_SETARCH_EXECUTABLE}" "${CMAKE_SYSTEM_PROCESSOR}" -R)
    message(STATUS "Sanitized tests run under: setarch ${CMAKE_SYSTEM_PROCESSOR} -R")
endfunction()
