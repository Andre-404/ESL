#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "JIT.h"
#include "llvm/Support/TargetSelect.h"
#include "SemanticAnalysis/semanticAnalyzer.h"
#include "Runtime/MemoryManagment/garbageCollector.h"
#include "Codegen/Passes/closureConverter.h"
#include "Codegen/Passes/ASTToTypedAST.h"
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
    path = "C:\\Temp\\ESL-prez\\main.esl";
#else
    path = string(argv[1]);
#endif
    flag = "-jit";
#else
    if(argc == 2) {
        path = string(argv[1]);
        flag = "-jit";
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
    preprocessing::Preprocessor preprocessor;
    preprocessor.preprocessProject(path);
    vector<ESLModule *> modules = preprocessor.getSortedUnits();

    AST::Parser parser;

    vector<AST::ASTModule> ASTmodules = parser.parse(modules);

    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);

    closureConversion::ClosureConverter finder;
    passes::typedASTParser::ASTTransformer transformer(ASTmodules);
    auto res = transformer.run(finder.generateFreevarMap(ASTmodules));
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);
    auto env = transformer.getTypeEnv();
    auto classes = transformer.getClassHierarchy();
    errorHandler::showCompileErrors();
    if (errorHandler::hasErrors()) exit(64);

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();
    std::unique_ptr<llvm::orc::LLJIT> JIT = std::move(setupJIT());
    compileCore::Compiler compiler(res.second, env, classes, transformer.getNativeFuncTypes(), JIT->getDataLayout());
    llvm::cantFail(JIT->addIRModule(std::move(compiler.compile(res.first, "func.main"))));

    llvm::orc::ExecutorAddr ExprSymbol = llvm::ExitOnError()(JIT->lookup("func.main"));
    int (*FP)(int, char*) = ExprSymbol.toPtr< int(*)(int, char*)>();
    FP(0, nullptr);
    return 0;
}
