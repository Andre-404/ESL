#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "Runtime/vm.h"
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

    string path;
    // For ease of use during development
    #ifdef DEBUG_MODE
    path = "C:\\Temp\\main.csl";
    #elif
    if(argc == 2) path = string(argv[1]);
    else{
        std::cout<<"Enter file path.\n";
        return 1;
    }
    #endif

    windowsSetTerminalProcessing();
    preprocessing::Preprocessor p;
    p.preprocessProject(path);
    vector<CSLModule*> modules = p.getSortedUnits();
    AST::Parser pa;
    pa.parse(modules);
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    compileCore::Compiler c(modules);
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    auto* vm = new runtime::VM(&c);
    vm->execute();
    return 0;
}