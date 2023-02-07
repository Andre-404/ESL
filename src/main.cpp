#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "Runtime/vm.h"
#include <chrono>


int main(int argc, char* argv[]) {
    preprocessing::Preprocessor p;
    p.preprocessProject("C:\\Temp\\main.csl");
    vector<CSLModule*> modules = p.getSortedUnits();
    AST::Parser pa;
    pa.parse(modules);
    compileCore::Compiler c(modules);
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    runtime::VM* vm = new runtime::VM(&c);
    auto t1 = std::chrono::high_resolution_clock::now();
    vm->execute();
    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << duration_cast<std::chrono::milliseconds>(t2 - t1).count() << std::endl;
    return 0;
}