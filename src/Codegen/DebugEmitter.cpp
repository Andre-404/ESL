#include "DebugEmitter.h"
using namespace compileCore;

DebugEmitter::DebugEmitter(llvm::Module& module, File& file, bool isOpt){
    builder = std::make_unique<llvm::DIBuilder>(module);
    llvm::DIFile* dfile = llvm::DIFile::get(module.getContext(), llvm::MDString::get(module.getContext(), file.name + ".esl"),
                                           llvm::MDString::get(module.getContext(), file.path));
    CU = builder->createCompileUnit(llvm::dwarf::DW_LANG_C, dfile,"ESL Compiler",isOpt,
                                    "", 0);
    types["any"] = builder->createBasicType("any", 64, llvm::dwarf::DW_ATE_float);
    types["void"] = builder->createUnspecifiedType("void");
    types["ptr"] = builder->createPointerType(types["void"], 64);
    types["int32"] = builder->createBasicType("int32", 32, llvm::dwarf::DW_ATE_signed);
}

void DebugEmitter::addMainFunc(llvm::Function* func){
    // 0th index is for return type
    llvm::DITypeRefArray typeVec = builder->getOrCreateTypeArray({types["void"], types["ptr"]});
    llvm::DISubroutineType* fnSig = builder->createSubroutineType(typeVec);
    llvm::DIFile* file = CU->getFile();
    llvm::DISubprogram* debugFunc = builder->createFunction(file, func->getName(), llvm::StringRef(),
                                                            file, 0, fnSig, 0,llvm::DINode::FlagPrototyped,
                                                            llvm::DISubprogram::SPFlagDefinition);
    func->setSubprogram(debugFunc);
    // Push main entrypoint because all code not in function goes to main
    scopes.push_back(debugFunc);
}
void DebugEmitter::addNewFunc(llvm::IRBuilder<>& IRBuilder, llvm::Function* func, types::FunctionType& fnty, Token& location){
    // 0th index is for return type
    vector<llvm::Metadata *> typeVec = {types["any"], types["ptr"], types["ptr"]};
    for(int i = 0; i < fnty.argCount; i++) typeVec.push_back(types["any"]);
    llvm::DISubroutineType* fnSig = builder->createSubroutineType(builder->getOrCreateTypeArray(typeVec));

    // TODO: can a synthetic token be passed here?
    llvm::DIFile* file = getFile(*location.str.sourceFile);
    int64_t line = location.str.computeLine();
    int64_t col = location.str.computeColumn();
    llvm::DISubprogram* debugFunc = builder->createFunction(file, func->getName(), llvm::StringRef(),
                                                            file, line, fnSig, line,
                                                            llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);
    func->setSubprogram(debugFunc);
    scopes.push_back(debugFunc);
    // Immediately set the debug location to avoid emitting debug info for wrong function
    llvm::DILocation* loc = llvm::DILocation::get(scopes.back()->getContext(), line, col, scopes.back());
    IRBuilder.SetCurrentDebugLocation(loc);
}
// Both addScope and popScope immediately set debug location to not emit any code to a closed scope
void DebugEmitter::addScope(llvm::IRBuilder<>& IRBuilder, Token& location){
    llvm::DIFile* file = getFile(*location.str.sourceFile);
    int64_t line = location.str.computeLine();
    int64_t col = location.str.computeColumn();
    scopes.push_back(builder->createLexicalBlock(scopes.back(), file, line, col));

    llvm::DILocation* loc = llvm::DILocation::get(scopes.back()->getContext(), line, col, scopes.back());
    IRBuilder.SetCurrentDebugLocation(loc);
}
void DebugEmitter::popScope(llvm::IRBuilder<>& IRBuilder, Token& location){
    scopes.pop_back();
    int64_t line = location.str.computeLine();
    int64_t col = location.str.computeColumn();

    llvm::DILocation* loc = llvm::DILocation::get(scopes.back()->getContext(), line, col, scopes.back());
    IRBuilder.SetCurrentDebugLocation(loc);
}
// We lose a bit of debug info by ignoring synthetic tokens, this should be fixed in the future
void DebugEmitter::emitNewLocation(llvm::IRBuilder<>& IRBuilder, Token& location){
    if(location.isSynthetic) return;
    int64_t line = location.str.computeLine();
    int64_t col = location.str.computeColumn();
    llvm::DILocation* loc = llvm::DILocation::get(scopes.back()->getContext(), line, col, scopes.back());
    IRBuilder.SetCurrentDebugLocation(loc);
}
void DebugEmitter::addLocalVarDecl(llvm::IRBuilder<>& IRBuilder, llvm::Value* alloca, Token& token, bool isParam, int argnum){
    if(token.isSynthetic) return;
    int64_t line = token.str.computeLine();
    int64_t col = token.str.computeColumn();
    llvm::DIFile* file = getFile(*token.str.sourceFile);
    llvm::DILocalVariable* var;
    if(isParam){
        var = builder->createParameterVariable(scopes.back(), token.getLexeme(), argnum, file, line, types["any"]);
    }else {
        var = builder->createAutoVariable(scopes.back(), token.getLexeme(), file, line, types["any"]);
    }
    llvm::DILocation* loc = llvm::DILocation::get(scopes.back()->getContext(), line, col, scopes.back());
    builder->insertDeclare(alloca, var, builder->createExpression(),loc,IRBuilder.GetInsertBlock());
}
void DebugEmitter::addGlobalVar(llvm::GlobalVariable* var, Token& token){
    int64_t line = token.str.computeLine();
    llvm::DIFile* file = getFile(*token.str.sourceFile);
    llvm::DIGlobalVariableExpression* debug = builder->createGlobalVariableExpression(scopes.back(), token.getLexeme(),
                                                                                      llvm::StringRef(), file, line,
                                                                                      types["any"], false);
    var->setMetadata(llvm::Metadata::MetadataKind::DIGlobalVariableExpressionKind, debug);
}
void DebugEmitter::finalize(){
    builder->finalize();
}
// TODO: for some reason LLVM decided to crash when using the DIFile constructor that takes StringRefs
// or when using builder->createFile, this is the only solution i could find
llvm::DIFile* DebugEmitter::getFile(File& file){
    return llvm::DIFile::get(CU->getContext(), llvm::MDString::get(CU->getContext(), file.name + ".esl"), llvm::MDString::get(CU->getContext(), file.path));
}