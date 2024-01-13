cmake_minimum_required(VERSION 3.22)
project(ESL)

if(WIN32)
    set(LLVM_DIR "C:/tools/llvm/lib/cmake/llvm")
else()
    set(LLVM_DIR "~/Git/llvm-project/build-static/lib/cmake/llvm")
endif()
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
#set(CMAKE_EXE_LINKER_FLAGS "-static")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(ESL src/main.cpp src/moduleDefs.h src/common.h src/files.h src/files.cpp src/Codegen/valueHelpers.h src/Codegen/valueHelpers.cpp src/Codegen/compiler.h src/Codegen/compiler.cpp src/ErrorHandling/errorHandler.h src/ErrorHandling/errorHandler.cpp src/MemoryManagment/garbageCollector.h src/MemoryManagment/garbageCollector.cpp src/Objects/objects.h src/Objects/objects.cpp src/AST/ASTDefs.h src/AST/ASTProbe.h src/AST/ASTProbe.cpp src/Parsing/parser.h src/Parsing/parser.cpp src/Preprocessing/scanner.h src/Preprocessing/scanner.cpp src/Preprocessing/preprocessor.h src/Preprocessing/preprocessor.cpp src/Runtime/vm.h src/Runtime/vm.cpp src/Runtime/thread.h src/Runtime/thread.cpp src/Includes/format.cc src/Includes/format.cc src/Includes/format.cc src/Includes/fmt/color.h src/Includes/fmt/ostream.h src/Includes/fmt/std.h src/Runtime/nativeFunctions.h src/Runtime/nativeFunctions.cpp src/Parsing/macroExpander.h src/Parsing/macroExpander.cpp src/Codegen/valueHelpersInline.cpp src/Includes/unorderedDense.h src/Codegen/Passes/closureConverter.h src/Codegen/Passes/closureConverter.cpp src/SemanticAnalysis/semanticAnalyzer.h src/SemanticAnalysis/semanticAnalyzer.cpp src/Codegen/LLVMHelperFunctions.h src/Codegen/LLVMHelperFunctions.cpp src/Codegen/JIT.h src/Codegen/LLVMNativeFunctionImplementation.h src/TypedAST/Types.h src/TypedAST/Types.cpp src/TypedAST/TypedASTDefs.h src/Codegen/Passes/ASTToTypedAST.h src/Codegen/Passes/ASTToTypedAST.cpp src/Codegen/Passes/typeUnification.h src/Codegen/Passes/typeUnification.cpp src/AST/ASTDebugInfo.h
)

#execute_process(COMMAND llvm-config)

llvm_map_components_to_libnames(llvm_libs
        support core irreader orcjit native passes scalaropts ipo target option analysis transformutils instcombine bitreader object option instrumentation)
message(STATUS "All libraries: ${llvm_libs}")

# Link against LLVM libraries
target_link_libraries(ESL "${llvm_libs}")
