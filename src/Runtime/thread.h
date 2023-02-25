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
        Value peek(int8_t depth);
        std::atomic<bool> cancelToken;
        // Tells the thread that it should pause it's execution, merely setting this to true doesn't pause
        std::atomic<bool> pauseToken;
        Value* stackTop;

        void runtimeError(string err, int errorCode);

        void callValue(Value callee, int8_t argCount);

    private:
		Value stack[STACK_MAX];
		CallFrame frames[FRAMES_MAX];
        uint16_t frameCount;

        string errorString;

		void callFunc(object::ObjClosure* function, int8_t argCount);
        void callMethod(object::Method method, int8_t argCount);

		void bindMethod(object::ObjClass* klass, object::ObjString* name, Value receiver);
		void invoke(object::ObjString* fieldName, int8_t argCount);
		void invokeFromClass(object::ObjClass* klass, object::ObjString* fieldName, int8_t argCount);
	};
}