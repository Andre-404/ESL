#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Module.h"
#include "../common.h"
#include "../Includes/unorderedDense.h"
#include "../moduleDefs.h"
#include "../TypedAST/Types.h"

namespace llvm{
    class DIScope;
    class DIType;
    class DIFile;
    class DICompileUnit;
}

namespace compileCore{
    class DebugEmitter {
    public:
        DebugEmitter() {}
        DebugEmitter(llvm::Module& module, File& mainModule, bool isOpt);
        void addMainFunc(llvm::Function* func);
        void addNewFunc(llvm::IRBuilder<>& IRBuilder, llvm::Function* func, types::FunctionType& fnty, Token& location);
        void addScope(llvm::IRBuilder<>& IRBuilder, Token& location);
        void popScope(llvm::IRBuilder<>& IRBuilder, Token& location);
        void emitNewLocation(llvm::IRBuilder<>& IRBuilder, Token& location);
        void addLocalVarDecl(llvm::IRBuilder<>& IRBuilder, llvm::Value* inst, Token& var, bool isParam, int argnum = 0);
        void addGlobalVar(llvm::GlobalVariable* var, Token& token);
        void finalize();
    private:
        vector<llvm::DIScope*> scopes;
        ankerl::unordered_dense::map<std::string_view, llvm::DIType*> types;
        ankerl::unordered_dense::map<string, llvm::DIFile*> files;
        std::unique_ptr<llvm::DIBuilder> builder;
        llvm::DICompileUnit* CU;

        llvm::DIFile* getFile(File& file);
    };
}
