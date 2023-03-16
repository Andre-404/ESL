#include "vm.h"
#include "../codegen/compiler.h"
#include "../codegen/valueHelpersInline.cpp"
#include "nativeFunctions.h"

using std::get;
using namespace valueHelpers;

runtime::VM::VM(compileCore::Compiler* compiler) {
    // Have to do this before assigning compiler->mainCodeBlock to code because endFuncDecl mutates mainCodeBlock
    Value val = encodeObj(new object::ObjClosure(compiler->mainBlockFunc));
    // Main code block
    code = compiler->mainCodeBlock;
    // Used by all threads
    nativeFuncs = compiler->nativeFuncs;
    nativeClasses = runtime::createBuiltinClasses(compiler->baseClass);
    nativeClasses.push_back(compiler->baseClass);
    rng = std::mt19937_64(0);
    globals = compiler->globals;
    // For stack tracing during error printing
    sourceFiles = compiler->sourceFiles;
    memory::gc.vm = this;
    mainThread = new Thread(this);
    // First value on the stack is the future holding the thread, mainThread has nil
    mainThread->copyVal(encodeNil());
    mainThread->startThread(&val, 1);
}

void runtime::VM::mark(memory::GarbageCollector* gc) {
    for (Globalvar& var : globals) valueHelpers::mark(var.val);
    // All threads in vector are active, finished threads get deleted automatically
    for (Thread* t : childThreads) t->mark(gc);
    mainThread->mark(gc);
    for (Value& val : code.constants) valueHelpers::mark(val);
    for (auto func : nativeFuncs) func->marked = true;
    for(auto c : nativeClasses) gc->markObj(c);
}

void runtime::VM::execute() {
    mainThread->executeBytecode();
}

bool runtime::VM::allThreadsPaused() {
    // Another thread might try to add/remove a Thread object while the main thread is waiting for all threads to pause
    std::scoped_lock<std::mutex> lk(mtx);
    return threadsPaused == childThreads.size();
}

void runtime::VM::pauseAllThreads(){
    std::scoped_lock<std::mutex> lk(mtx);
    for(runtime::Thread* t : childThreads){
        t->pauseToken.store(true, std::memory_order_relaxed);
    }
    mainThread->pauseToken.store(true, std::memory_order_relaxed);
}

void runtime::VM::unpauseAllThreads(){
    std::scoped_lock<std::mutex> lk(mtx);
    for(runtime::Thread* t : childThreads){
        t->pauseToken.store(false, std::memory_order_relaxed);
    }
    mainThread->pauseToken.store(false, std::memory_order_relaxed);
}