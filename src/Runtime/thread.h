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

		byte getOp(uInt64 _ip);
		void push(Value val);
		Value pop();
		Value peek(int depth);

		void runtimeError(string err);

		bool callValue(Value callee, int argCount);
		bool call(object::ObjClosure* function, int argCount);
		object::ObjUpval* captureUpvalue(Value* local);

		void defineMethod(string& name);
		bool bindMethod(object::ObjClass* klass, string& name);
		bool invoke(string& fieldName, int argCount);
		bool invokeFromClass(object::ObjClass* klass, string& fieldName, int argCount);
	};
}