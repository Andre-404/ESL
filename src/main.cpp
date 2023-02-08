#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "Runtime/vm.h"
#include <chrono>
#include <windows.h>

static void windowsSetTerminalProcessing(){
    // Windows supports ANSI escape sequences, but they must be manually enabled
    HANDLE handleOut = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD consoleMode;
    GetConsoleMode( handleOut , &consoleMode);
    consoleMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    consoleMode |= ENABLE_PROCESSED_OUTPUT;
    SetConsoleMode( handleOut , consoleMode );
};


int main(int argc, char* argv[]) {
    windowsSetTerminalProcessing();
    preprocessing::Preprocessor p;
    p.preprocessProject("C:\\Temp\\main.csl");
    vector<CSLModule*> modules = p.getSortedUnits();
    AST::Parser pa;
    pa.parse(modules);
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    compileCore::Compiler c(modules);
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    auto* vm = new runtime::VM(&c);
    auto t1 = std::chrono::high_resolution_clock::now();
    vm->execute();
    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << duration_cast<std::chrono::milliseconds>(t2 - t1).count() << " ms"<<std::endl;
    return 0;
}