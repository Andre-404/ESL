#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "SemanticAnalysis/semanticAnalyzer.h"
#include "Runtime/vm.h"
#include <chrono>

#if defined(_WIN32) || defined(WIN32)
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
#endif

int main(int argc, char* argv[]) {
    string path;
    string flag;
    // For ease of use during development
    #ifdef DEBUG_MODE
    #if defined(_WIN32) || defined(WIN32)
    path = "C:\\Temp\\mergeSort.esl";
    #else
    path = "/mnt/c/Temp/main.esl";
    #endif
    flag = "-run";
    #else
    if(argc == 2) {
        path = string(argv[1]);
        flag = "-run";
    }
    else if(argc == 3){
        path = string(argv[1]);
        flag = string(argv[2]);
    }
    else{
        std::cout<<"No filepath entered.\n";
        return 1;
    }
    #endif
    #if defined(_WIN32) || defined(WIN32)
    windowsSetTerminalProcessing();
    #endif
    if(flag == "-run") {
        preprocessing::Preprocessor preprocessor;
        preprocessor.preprocessProject(path);
        vector<CSLModule *> modules = preprocessor.getSortedUnits();

        AST::Parser parser;

        parser.parse(modules);

        errorHandler::showCompileErrors();
        if (errorHandler::hasErrors()) exit(64);

        compileCore::Compiler compiler(modules);

        errorHandler::showCompileErrors();
        if (errorHandler::hasErrors()) exit(64);

        auto vm = new runtime::VM(&compiler);

        vm->execute();
    }else if(flag == "-validate-file"){
        preprocessing::Preprocessor preprocessor;
        preprocessor.preprocessProject(path);
        vector<CSLModule *> modules = preprocessor.getSortedUnits();

        AST::Parser().parse(modules);

        SemanticAnalysis::SemanticAnalyzer semanticAnalyzer;
        std::cout << semanticAnalyzer.generateDiagnostics(modules);
    }else if(flag == "-semantic-analysis"){
        preprocessing::Preprocessor preprocessor;
        preprocessor.preprocessProject(path);
        vector<CSLModule *> modules = preprocessor.getSortedUnits();

        AST::Parser parser;

        parser.highlight(modules, path);
    }else{
        std::cout<<"Unrecognized flag.\n";
        return 1;
    }
    return 0;
}
