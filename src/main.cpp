#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "Runtime/vm.h"
<<<<<<< HEAD

#include <chrono>
=======
>>>>>>> cdfe446182b27cb4f97064b1a46a8899dcb9028b
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

<<<<<<< HEAD
int main() {
    windowsSetTerminalProcessing();
    preprocessing::Preprocessor preprocessor;
    preprocessor.preprocessProject("C:\\Temp\\main.csl");
    vector<CSLModule*> modules = preprocessor.getSortedUnits();

    AST::Parser parser;

    parser.parse(modules);


=======

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
>>>>>>> cdfe446182b27cb4f97064b1a46a8899dcb9028b
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);

    compileCore::Compiler compiler(modules);

    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
<<<<<<< HEAD

    auto vm = new runtime::VM(&compiler);

    auto t1 = std::chrono::high_resolution_clock::now();
    vm->execute();
    auto t2 = std::chrono::high_resolution_clock::now();

    std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count() << "ms" << std::endl;
=======
    auto* vm = new runtime::VM(&c);
    vm->execute();
>>>>>>> cdfe446182b27cb4f97064b1a46a8899dcb9028b
    return 0;
}
