cmake_minimum_required(VERSION 3.24)
project(ESL)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(ESL src/main.cpp src/moduleDefs.h src/common.h src/files.h src/files.cpp src/codegen/codegenDefs.h src/codegen/codegenDefs.cpp src/codegen/compiler.h src/codegen/compiler.cpp src/DebugPrinting/ASTPrinter.h src/DebugPrinting/ASTPrinter.cpp src/DebugPrinting/BytecodePrinter.h src/DebugPrinting/BytecodePrinter.cpp src/ErrorHandling/errorHandler.h src/ErrorHandling/errorHandler.cpp src/Includes/robin_hood.h src/MemoryManagment/garbageCollector.h src/MemoryManagment/garbageCollector.cpp src/Objects/objects.h src/Objects/objects.cpp src/Parsing/ASTDefs.h src/Parsing/ASTProbe.h src/Parsing/ASTProbe.cpp src/Parsing/parser.h src/Parsing/parser.cpp src/Preprocessing/scanner.h src/Preprocessing/scanner.cpp src/Preprocessing/preprocessor.h src/Preprocessing/preprocessor.cpp src/Runtime/vm.h src/Runtime/vm.cpp src/Runtime/thread.h src/Runtime/thread.cpp src/Includes/format.cc src/Includes/format.cc src/Includes/format.cc src/Includes/fmt/color.h src/Includes/fmt/ostream.h src/Includes/fmt/std.h src/Parsing/MacroExpander.h src/Parsing/MacroExpander.cpp)