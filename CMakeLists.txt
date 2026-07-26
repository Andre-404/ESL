cmake_minimum_required(VERSION 3.25)
project(ESL CXX C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(ESLBuildOptions)

option(ESL_LLVM_STATIC  "Link LLVM's static component libraries instead of libLLVM.so" OFF)
option(ESL_LTO          "Build the compiler with link time optimization" OFF)
option(ESL_BUILD_COMPILER "Build the ESL compiler executable" ON)
option(ESL_BUILD_TESTS  "Build the concurrent GC test suite" ON)

# Hardcoded llvm paths
if(WIN32)
    set(LLVM_DIR "C:/tools/llvm/lib/cmake/llvm" CACHE PATH "Location of LLVMConfig.cmake")
else()
    set(LLVM_DIR "/usr/lib/llvm-23/lib/cmake/llvm" CACHE PATH "Location of LLVMConfig.cmake")
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
endif()

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
    set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
endif()

add_compile_options(-march=native -fno-rtti)
if(NOT WIN32)
    add_link_options(-Wl,-no-as-needed -rdynamic)
endif()

esl_use_lld()
# Must run before any target (including googletest) is created.
esl_apply_sanitizer()

set(LLVM_ENABLE_ASSERTIONS OFF)
find_package(LLVM 23.0.0 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

if(ESL_LLVM_STATIC)
    # The component list the dylib would have covered. Kept explicit so a missing
    # component fails at configure time rather than as an undefined symbol.
    llvm_map_components_to_libnames(ESL_LLVM_LIBS
        support core irreader orcjit passes scalaropts ipo target option
        analysis transformutils instcombine bitreader object instrumentation
        x86codegen x86asmparser x86desc x86disassembler x86info
        jitlink executionengine debuginfodwarf orctargetprocess symbolize)
    message(STATUS "LLVM: static components")
else()
    set(ESL_LLVM_LIBS LLVM)
    message(STATUS "LLVM: shared libLLVM")
endif()

if(ESL_BUILD_COMPILER)

add_executable(ESL src/main.cpp src/moduleDefs.h src/common.h src/files.h src/files.cpp src/Runtime/Values/valueHelpers.h src/Runtime/Values/valueHelpers.cpp src/Codegen/compiler.h src/Codegen/compiler.cpp src/ErrorHandling/errorHandler.h src/ErrorHandling/errorHandler.cpp src/Runtime/Objects/objects.h src/Runtime/Objects/objects.cpp src/AST/ASTDefs.h src/AST/ASTProbe.h src/AST/ASTProbe.cpp src/Parsing/parser.h src/Parsing/parser.cpp src/Preprocessing/scanner.h src/Preprocessing/scanner.cpp src/Preprocessing/preprocessor.h src/Preprocessing/preprocessor.cpp src/Includes/format.cc src/Includes/format.cc src/Includes/format.cc src/Includes/fmt/color.h src/Includes/fmt/ostream.h src/Includes/fmt/std.h src/Runtime/oldNativeFunctions.h src/Runtime/oldNativeFunctions.cpp src/Parsing/macroExpander.h src/Parsing/macroExpander.cpp src/Runtime/Values/valueHelpersInline.cpp src/Includes/unorderedDense.h src/Codegen/Passes/AST/closureConverter.h src/Codegen/Passes/AST/closureConverter.cpp src/SemanticAnalysis/semanticAnalyzer.h src/SemanticAnalysis/semanticAnalyzer.cpp src/Codegen/LLVMHelperFunctions.h src/Codegen/LLVMHelperFunctions.cpp src/Runtime/JIT/JIT.h src/Runtime/LLVMHelperExports.h src/TypedAST/Types.h src/TypedAST/Types.cpp src/TypedAST/TypedASTDefs.h src/Codegen/Passes/AST/ASTToTypedAST.h src/Codegen/Passes/AST/ASTToTypedAST.cpp src/AST/ASTDebugInfo.h
        src/Codegen/Passes/computeClassHierarchy.h
        src/Codegen/Passes/computeClassHierarchy.cpp
        src/Runtime/nativeFunctionsExports.h
        src/Includes/rpmalloc/rpmalloc.h
        src/Includes/rpmalloc/rpmalloc.c
        src/Includes/rapidhash.h
        src/Codegen/DebugEmitter.cpp
        src/Codegen/DebugEmitter.h
        src/Runtime/JIT/JIT.cpp
        src/Runtime/JIT/DebugInfoPlugin.cpp
        src/Runtime/JIT/DebugInfoPlugin.h
        src/Codegen/Passes/AST/ASTOptimization.cpp
        src/Codegen/Passes/AST/ASTOptimization.h
        src/Codegen/Passes/AST/ASTVerifier.cpp
        src/Codegen/Passes/AST/ASTVerifier.h
        src/Codegen/Passes/CFG/TypeInference.cpp
        src/Codegen/Passes/CFG/TypeInference.h
        src/Codegen/Passes/CFG/CFGPrinter.h
        src/Codegen/CompilerParts/ComptimeValues.cpp
        src/Codegen/CompilerParts/ComptimeValues.h
        src/Codegen/CompilerParts/RuntimeTypecheck.cpp
        src/Codegen/CompilerParts/RuntimeTypecheck.h
        src/Codegen/CompilerParts/TypeHelper.cpp
        src/Codegen/CompilerParts/TypeHelper.h
        src/Codegen/CompilerParts/InstBuilder.cpp
        src/Codegen/CompilerParts/InstBuilder.h
        src/Runtime/ConcurrentGC/gc-config.h
        src/Runtime/ConcurrentGC/pg-meta.h
        src/Runtime/ConcurrentGC/managed.h
        src/Runtime/ConcurrentGC/pg-manager.h
        src/Runtime/ConcurrentGC/page-allocator.h
        src/Runtime/ConcurrentGC/page-allocator.cpp
        src/Runtime/ConcurrentGC/pg-manager.cpp
        src/Runtime/ConcurrentGC/tstack.h
        src/Runtime/ConcurrentGC/sync-point.h
        src/Runtime/ConcurrentGC/copier.h
        src/Runtime/ConcurrentGC/copier.cpp
        src/Runtime/ConcurrentGC/mark-buf.h
        src/Runtime/ConcurrentGC/pruner.h
        src/Runtime/ConcurrentGC/marker.h
        src/Runtime/ConcurrentGC/mark-buf.cpp
        src/Runtime/ConcurrentGC/marker.cpp
        src/Runtime/ConcurrentGC/customization.h
        src/Runtime/ConcurrentGC/obj-allocator.h
        src/Runtime/ConcurrentGC/TCB.h
        src/Runtime/ConcurrentGC/TCB-registry.h
        src/Runtime/ConcurrentGC/collector.h
        src/Runtime/ConcurrentGC/collector.cpp
        src/Runtime/ConcurrentGC/gc-heuristic.h
        src/Runtime/ConcurrentGC/arena.h
        src/Runtime/ConcurrentGC/szclass-allocator.h
        src/Runtime/ConcurrentGC/platform-specific.h
        src/Runtime/ConcurrentGC/radix-tree.h
        src/Runtime/ConcurrentGC/transition-manager.h
        src/Runtime/ConcurrentGC/gc-api.h
        src/Runtime/ConcurrentGC/gc-api.cpp
        src/Runtime/Objects/gc-customization.cpp
        src/Runtime/esl-gc-helpers.h
        src/Runtime/Objects/string-interner.h
        src/Runtime/stub.cpp
        src/Runtime/ConcurrentGC/bitmap.h
)
target_compile_features(ESL PRIVATE cxx_std_20)
# r15 holds the TCB pointer; see ConcurrentGC/platforms/x86-64.inc.
target_compile_options(ESL PRIVATE -ffixed-r15)
target_link_libraries(ESL ${ESL_LLVM_LIBS})

if(ESL_LTO)
    include(CheckIPOSupported)
    check_ipo_supported(RESULT _ipo_ok OUTPUT _ipo_why)
    if(_ipo_ok)
        set_property(TARGET ESL PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
        message(STATUS "LTO: enabled")
    else()
        message(WARNING "LTO requested but unsupported: ${_ipo_why}")
    endif()
endif()

endif() # ESL_BUILD_COMPILER

if(ESL_BUILD_TESTS)
    # Needed here, not just in the subdirectory, or ctest run from the top level
    # build directory finds no tests to descend into.
    enable_testing()
    add_subdirectory(src/Runtime/ConcurrentGC/tests)
endif()