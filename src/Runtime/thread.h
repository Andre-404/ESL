#pragma once
#include "../codegen/codegenDefs.h"
#include "../Objects/objects.h"

namespace runtime {
	class VM;

	class Thread {
	public:
		Thread(VM* _vm);
		void executeBytecode();
		void startThread(Value* otherStack, int num);
		void mark(memory::GarbageCollector* gc);
		void copyVal(Value val);
	private:
		Value stack[STACK_MAX];
		Value* stackTop;
		CallFrame frames[FRAMES_MAX];
		int frameCount;

		VM* vm;
        string errorString;

		void push(Value val);
		Value pop();
		Value peek(int depth);

		void runtimeError(string err, int errorCode);

		void callValue(Value callee, int argCount);
		void call(object::ObjClosure* function, int argCount);

		void defineMethod(string& name);
		void bindMethod(object::ObjClass* klass, string& name);
		void invoke(string& fieldName, int argCount);
		void invokeFromClass(object::ObjClass* klass, string& fieldName, int argCount);
	};
}