cmake_minimum_required(VERSION 3.22)
project(ESL)

# Hardcoded llvm paths
if(WIN32)
    set(LLVM_DIR "C:/Temp/LLVM-19/Install/lib/cmake/llvm")
    set(CMAKE_CXX_FLAGS "-march=native")
else()
    set(LLVM_DIR "/usr/local/llvm/lib")
    set(CMAKE_CXX_FLAGS "-Wl,-no-as-needed -rdynamic -march=native")
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
endif()
set(LLVM_ENABLE_ASSERTIONS OFF)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(ESL src/main.cpp src/moduleDefs.h src/common.h src/files.h src/files.cpp src/Runtime/Values/valueHelpers.h src/Runtime/Values/valueHelpers.cpp src/Codegen/compiler.h src/Codegen/compiler.cpp src/ErrorHandling/errorHandler.h src/ErrorHandling/errorHandler.cpp src/Runtime/MemoryManagment/garbageCollector.h src/Runtime/MemoryManagment/garbageCollector.cpp src/Runtime/Objects/objects.h src/Runtime/Objects/objects.cpp src/AST/ASTDefs.h src/AST/ASTProbe.h src/AST/ASTProbe.cpp src/Parsing/parser.h src/Parsing/parser.cpp src/Preprocessing/scanner.h src/Preprocessing/scanner.cpp src/Preprocessing/preprocessor.h src/Preprocessing/preprocessor.cpp src/Includes/format.cc src/Includes/format.cc src/Includes/format.cc src/Includes/fmt/color.h src/Includes/fmt/ostream.h src/Includes/fmt/std.h src/Runtime/oldNativeFunctions.h src/Runtime/oldNativeFunctions.cpp src/Parsing/macroExpander.h src/Parsing/macroExpander.cpp src/Runtime/Values/valueHelpersInline.cpp src/Includes/unorderedDense.h src/Codegen/Passes/closureConverter.h src/Codegen/Passes/closureConverter.cpp src/SemanticAnalysis/semanticAnalyzer.h src/SemanticAnalysis/semanticAnalyzer.cpp src/Codegen/LLVMHelperFunctions.h src/Codegen/LLVMHelperFunctions.cpp src/Runtime/JIT/JIT.h src/Runtime/LLVMHelperExports.h src/TypedAST/Types.h src/TypedAST/Types.cpp src/TypedAST/TypedASTDefs.h src/Codegen/Passes/ASTToTypedAST.h src/Codegen/Passes/ASTToTypedAST.cpp src/Codegen/Passes/typeUnification.h src/Codegen/Passes/typeUnification.cpp src/AST/ASTDebugInfo.h
        src/Codegen/Passes/computeClassHierarchy.h
        src/Codegen/Passes/computeClassHierarchy.cpp
        src/Runtime/nativeFunctionsExports.h
        src/Runtime/MemoryManagment/threadArena.cpp
        src/Runtime/MemoryManagment/threadArena.h
        src/Includes/rpmalloc/rpmalloc.h
        src/Includes/rpmalloc/rpmalloc.c
        src/Runtime/MemoryManagment/heapPageManager.cpp
        src/Runtime/MemoryManagment/heapPageManager.h
        src/Includes/rapidhash.h
        src/Codegen/DebugEmitter.cpp
        src/Codegen/DebugEmitter.h
        src/Runtime/JIT/JIT.cpp
        src/Runtime/JIT/SEHFrameRegistrar.h
        src/Runtime/JIT/SEHFrameRegistrar.cpp
)

llvm_map_components_to_libnames(llvm_libs
        support core irreader orcjit native passes scalaropts ipo target option analysis transformutils instcombine bitreader object option instrumentation)
message(STATUS "All libraries: ${llvm_libs}")

# Link against LLVM libraries
target_link_libraries(ESL "${llvm_libs}")

target_compile_features(ESL PUBLIC cxx_std_20)
