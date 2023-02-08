#include "vm.h"
#include "../codegen/compiler.h"

using std::get;

runtime::VM::VM(compileCore::Compiler* compiler) {
	globals = compiler->globals;
	// For stack tracing during error printing
	sourceFiles = compiler->sourceFiles;
	// Have to do this before assigning compiler->mainCodeBlock to code because endFuncDecl mutates mainCodeBlock
	Value val = Value(new object::ObjClosure(compiler->mainBlockFunc));
	// Main code block
	code = compiler->mainCodeBlock;

	mainThread = new Thread(this);
	// First value on the stack is the future holding the thread, mainThread has nil
	mainThread->copyVal(Value::nil());
	mainThread->startThread(&val, 1);
}

void runtime::VM::mark(memory::GarbageCollector* gc) {
	for (Globalvar& var : globals) var.val.mark();
	// All threads in vector are active, finished threads get deleted automatically
	for (Thread* t : childThreads) t->mark(gc);
	mainThread->mark(gc);
	for (Value& val : code.constants) val.mark();
}

void runtime::VM::execute() {
	mainThread->executeBytecode();
}

bool runtime::VM::allThreadsPaused() {
	// Another thread might try to add/remove a Thread object while the main thread is waiting for all threads to pause
	std::scoped_lock<std::mutex> lk(mtx);
	return threadsPaused == childThreads.size();
}

