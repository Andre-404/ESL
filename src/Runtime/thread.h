#pragma once
#include "../codegen/codegenDefs.h"
#include "../Objects/objects.h"
#include "nativeFunctions.h"

namespace runtime {
	class VM;

	class Thread {
	public:
		Thread(VM* _vm);
		void executeBytecode();
		void startThread(Value* otherStack, int num);
		void mark(memory::GarbageCollector* gc);
		void copyVal(Value val);

        // Used by Thread and native functions
        VM* vm;
        void push(Value val);
        Value pop();
        void popn(int n);
        Value peek(int depth);
        std::atomic<bool> cancelToken;
        // Tells the thread that it should pause it's execution, merely setting this to true doesn't pause
        std::atomic<bool> pauseToken;

        void runtimeError(string err, int errorCode);

        void callValue(Value callee, int argCount);
	private:
		Value stack[STACK_MAX];
		CallFrame frames[FRAMES_MAX];
		int frameCount;
        Value* stackTop;

        string errorString;

		void call(object::ObjClosure* function, int argCount);

		void defineMethod(string& name);
        // True if method exists and was bound to receiver
		bool bindMethod(object::ObjClass* klass, string& name);
		void invoke(string& fieldName, int argCount);
        // True if method exists and was invoked
		bool invokeFromClass(object::ObjClass* klass, string& fieldName, int argCount);

        void bindMethodToPrimitive(Value receiver, string& methodName);
        BuiltinMethod& findNativeMethod(Value receiver, string& name);
	};
}