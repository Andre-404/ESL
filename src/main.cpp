#include <iostream>
#include <filesystem>
#include "Preprocessing/preprocessor.h"
#include "ErrorHandling/errorHandler.h"
#include "Parsing/parser.h"
#include "Codegen/compiler.h"
#include "Runtime/JIT/JIT.h"
#include "llvm/Support/TargetSelect.h"
#include "SemanticAnalysis/semanticAnalyzer.h"
#include "Codegen/Passes/AST/closureConverter.h"
#include "Codegen/Passes/AST/ASTToTypedAST.h"
#include "Codegen/Passes/AST/ASTVerifier.h"
#include "Codegen/Passes/AST/ASTOptimization.h"
#include "Codegen/Passes/CFG/TypeInference.h"

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
    path = "/mnt/c/Temp/ESL-prez/main.esl";
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
    errorHandler::ErrorHandler handler;
    preprocessing::Preprocessor preprocessor(handler);
    preprocessor.preprocessProject(path);
    vector<ESLModule *> modules = preprocessor.getSortedUnits();
    if(handler.hasErrors()){
        handler.displayErrors();
        exit(64);
    }
    AST::Parser parser(handler);

    vector<AST::ASTModule> ASTmodules = parser.parse(modules);
    AST::ASTVerifier verifier(handler);
    verifier.process(ASTmodules);
    if(handler.hasErrors()){
        handler.displayErrors();
        exit(64);
    }
    AST::ASTOptimizer optimizer(handler);
    optimizer.process(ASTmodules);

    closureConversion::ClosureConverter finder;
    passes::typedASTParser::ASTTransformer transformer(ASTmodules, handler);
    auto res = transformer.run(finder.generateFreevarMap(ASTmodules));
    if(handler.hasErrors()){
        handler.displayErrors();
        exit(64);
    }
    TypeInferencePass pass;
    pass.run(res, true);
    auto classes = transformer.getClassHierarchy();
    if (handler.hasErrors()) {
        handler.displayErrors();
        exit(64);
    }

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();
    ESLJIT::createJIT();
    compileCore::Compiler compiler(res.second, classes, transformer.getNativeFuncTypes(),
                                   ESLJIT::getJIT().getDL(), handler);
    ESLJIT::getJIT().addIRModule(std::move(compiler.compile(res.first, "func.main")));
    MainFn mainFuncPtr = ESLJIT::getJIT().getMainFunc();
    mainFuncPtr(0, nullptr);
    return 0;
}
