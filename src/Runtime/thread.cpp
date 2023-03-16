#include "thread.h"
#include "vm.h"
#include <iostream>
#include <utility>
#include "../Includes/fmt/format.h"
#include "../Includes/fmt/color.h"
#include "../codegen/valueHelpersInline.cpp"
#include "../DebugPrinting/BytecodePrinter.h"

using std::get;
using namespace valueHelpers;

runtime::Thread::Thread(VM* _vm){
	stackTop = stack;
	frameCount = 0;
    cancelToken.store(false);
    pauseToken.store(false);
	vm = _vm;
}

// Copies the callee and all arguments, otherStack points to the callee, arguments are on top of it on the stack
void runtime::Thread::startThread(Value* otherStack, int num) {
	memcpy(stackTop, otherStack, sizeof(Value) * num);
	stackTop += num;
	callValue(*otherStack, num - 1);
}

// Copies value to the stack
void runtime::Thread::copyVal(Value val) {
	push(val);
}

void runtime::Thread::mark(memory::GarbageCollector* gc) {
	for (Value* i = stack; i < stackTop; i++) {
		valueHelpers::mark(*i);
	}
	for (int i = 0; i < frameCount; i++) gc->markObj(frames[i].closure);
}

void runtime::Thread::push(Value val) {
	if (stackTop >= stack + STACK_MAX) {
		runtimeError("Stack overflow", 1);
	}
	*(stackTop++) = val;
}

Value runtime::Thread::pop() {
	return *(--stackTop);
}

void runtime::Thread::popn(int n) {
    stackTop-= n;
}

Value runtime::Thread::peek(int8_t depth) {
    return stackTop[-1 - depth];
}

void runtime::Thread::runtimeError(string err, int errorCode) {
    errorString = std::move(err);
    throw errorCode;
}

void runtime::Thread::callValue(Value callee, int8_t argCount) {
	if (isObj(callee)) {
		switch (decodeObj(callee)->type) {
		case object::ObjType::CLOSURE:
			return callFunc(asClosure(callee), argCount);
		case object::ObjType::NATIVE: {
            int8_t arity = asNativeFn(callee)->arity;
			//if arity is -1 it means that the function takes in a variable number of args
			if (arity != -1 && argCount != arity) {
				runtimeError(fmt::format("Function {} expects {} arguments but got {}.", asNativeFn(callee)->name, arity, argCount), 2);
			}
			object::NativeFn native = asNativeFn(callee)->func;
            native(this, argCount);
            //ObjNativeFunc is still on the stack and should be popped and replaced with the result
            stackTop[-2] = stackTop[-1];
            stackTop--;
            return;
		}
		case object::ObjType::CLASS: {
			// We do this so if a GC runs we safely update all the pointers(since the stack is considered a root)
            object::ObjClass* klass = asClass(callee);
			stackTop[-argCount - 1] = encodeObj(new object::ObjInstance(klass));
            auto it = klass->methods.find(klass->name);
			if (it != klass->methods.end()) {
				return callMethod(it->second, argCount);
			}
			else if (argCount != 0) {
				runtimeError(fmt::format("Class constructor expects 0 arguments but got {}.", argCount), 2);
			}
			return;
		}
		case object::ObjType::BOUND_METHOD: {
			//puts the receiver instance in the 0th slot of the current callframe('this' points to the 0th slot)
			object::ObjBoundMethod* bound = asBoundMethod(callee);
			stackTop[-argCount - 1] = bound->receiver;
			return callMethod(bound->method, argCount);
		}
		default:
			break; // Non-callable object type.
		}
	}
	runtimeError("Can only call functions and classes.", 3);
}

void runtime::Thread::callFunc(object::ObjClosure* closure, int8_t argCount) {
	if (argCount != closure->func->arity) {
		runtimeError(fmt::format("Expected {} arguments for function call but got {}.", closure->func->arity, argCount), 2);
	}

	if (frameCount == FRAMES_MAX) {
		runtimeError("Stack overflow.", 1);
	}

	CallFrame* frame = &frames[frameCount++];
	frame->closure = closure;
	frame->ip = &vm->code.bytecode[closure->func->bytecodeOffset];
	frame->slots = stackTop - argCount - 1;
}

void runtime::Thread::callMethod(object::Method method, int8_t argCount) {
    if(method->type == object::ObjType::CLOSURE) {
        auto closure = reinterpret_cast<ObjClosure*>(method);
        if (argCount != closure->func->arity) {
            runtimeError(
                    fmt::format("Function {} expectes {} arguments but got {}.", closure->func->name, closure->func->arity, argCount),
                    2);
        }

        if (frameCount == FRAMES_MAX) {
            runtimeError("Stack overflow.", 1);
        }

        CallFrame *frame = &frames[frameCount++];
        frame->closure = closure;
        frame->ip = &vm->code.bytecode[closure->func->bytecodeOffset];
        frame->slots = stackTop - argCount - 1;
        return;
    }
    auto native = reinterpret_cast<ObjNativeFunc*>(method);
    if (native->arity != -1 && argCount != native->arity) {
        runtimeError(fmt::format("Function {} expects {} arguments but got {}.", native->name, native->arity, argCount), 2);
    }
    return native->func(this, argCount);
}

static object::ObjUpval* captureUpvalue(Value* local) {
	return asUpvalue(*local);
}

inline static object::ObjClass* getClass(vector<object::ObjClass*>& classes, Value val){
    int8_t index = +runtime::Builtin::COMMON;
    if(isObj(val)){
        switch(decodeObj(val)->type){
            case object::ObjType::STRING: index = +runtime::Builtin::STRING; break;
            case object::ObjType::ARRAY: index = +runtime::Builtin::ARRAY; break;
            case object::ObjType::MUTEX: index = +runtime::Builtin::MUTEX; break;
            case object::ObjType::FILE: index = +runtime::Builtin::FILE; break;
            case object::ObjType::FUTURE: index = +runtime::Builtin::FUTURE; break;
        }
    }
    return classes[index];
}

void runtime::Thread::bindMethod(object::ObjClass* klass, object::ObjString* name, Value receiver) {
	auto it = klass->methods.find(name);
	if (it == klass->methods.end()) {
        runtimeError(fmt::format("Class '{}' doesn't contain method '{}'", klass->name->str, name->str), 4);
    }
	//peek(0) to get the ObjInstance
	auto* bound = new object::ObjBoundMethod(receiver, it->second);
    // Replace top of the stack
    push(encodeObj(bound));
}

void runtime::Thread::invoke(object::ObjString* fieldName, int8_t argCount) {
	Value receiver = peek(argCount);
    ObjClass* klass = nullptr;

	if (isInstance(receiver)) {
        object::ObjInstance* instance = asInstance(receiver);
        auto it = instance->fields.find(fieldName);
        if (it != instance->fields.end()) {
            stackTop[-argCount - 1] = it->second;
            return callValue(it->second, argCount);
        }
        klass = instance->klass;
	}else klass = getClass(vm->nativeClasses, receiver);
    invokeFromClass(klass, fieldName, argCount);
}

void runtime::Thread::invokeFromClass(object::ObjClass* klass, object::ObjString* methodName, int8_t argCount) {
	auto it = klass->methods.find(methodName);
	if (it == klass->methods.end()) {
        runtimeError(fmt::format("Class '{}' doesn't contain method '{}'.", klass->name->str, methodName->str), 4);
    }
	// The bottom of the call stack will contain the receiver instance
	callMethod(it->second, argCount);
}

static int32_t checkArrayBounds(runtime::Thread* t, Value& field, Value& callee, object::ObjArray* arr) {
    if (!isInt(field)) { t->runtimeError(fmt::format("Index must be an integer, got {}.", typeToStr(callee)), 3); }
    int32_t index = decodeInt(field);
    if (index < 0 || index > arr->values.size() - 1) { t->runtimeError(fmt::format("Index {} outside of range [0, {}].", index, arr->values.size() - 1), 9); }
    return index;
}

__attribute__((noinline)) static void deleteThread(object::ObjFuture* _fut, runtime::VM* vm) {
    std::condition_variable &cv = vm->mainThreadCv;
    // If execution is finishing and the main thread is waiting to run the gc
    // notify the main thread after deleting this thread object
    {
        // vm->pauseMtx to notify the main thread that this thread doesn't exist anymore,
        std::scoped_lock lk(vm->pauseMtx, vm->mtx);
        // Immediately delete the thread object to conserve memory
        for (auto it = vm->childThreads.begin(); it != vm->childThreads.end(); it++) {
            if (*it == _fut->thread) {
                delete* it;
                _fut->thread = nullptr;
                vm->childThreads.erase(it);
                break;
            }
        }
    }
    cv.notify_one();
}

__attribute__((noinline)) static bool handlePauseToken(runtime::Thread* t, object::ObjFuture* fut){
    auto vm = t->vm;
    if(t->cancelToken.load()) {
        // If this is a child thread that has a future attached to it, assign the value to the future
        fut->val = encodeNil();
        // If execution is finishing and the main thread is waiting to run the gc
        // notify the main thread after deleting this thread object
        // deleteThread locks vm->mtx to delete itself from the pool
        deleteThread(fut, vm);
        return true;
    }
    // If this thread is paused and is not cancelled, then it must be paused to run the GC
    if (!fut) {
        // If fut is null, this is the main thread of execution which runs the GC
        if (vm->allThreadsPaused()) {
            memory::gc.collect();
        } else {
            // If some threads aren't sleeping yet, use a cond var to wait, every child thread will notify the var when it goes to sleep
            std::unique_lock lk(vm->pauseMtx);
            vm->mainThreadCv.wait(lk, [&] { return vm->allThreadsPaused(); });
            // Release the mutex here so that GC can acquire it
            lk.unlock();
            // After all threads are asleep, run the GC and subsequently awaken all child threads
            memory::gc.collect();
        }
    } else {
        // If this is a child thread and the GC must run, notify the main thread that this one is paused
        // Main thread sends the notification when to awaken
        {
            std::scoped_lock lk(vm->pauseMtx);
            vm->threadsPaused.fetch_add(1);
        }
        // Only the main thread waits for mainThreadCv
        vm->mainThreadCv.notify_one();

        // No need to propagate this since the main thread won't be listening
        std::unique_lock lk(vm->pauseMtx);
        vm->childThreadsCv.wait(lk, [] { return !memory::gc.shouldCollect.load(); });
        vm->threadsPaused.fetch_sub(1);
        lk.unlock();
    }
    return false;
}

static void tryIncrement(runtime::Thread *t, byte arg, Value &val) {
    if (!isNumber(val)) t->runtimeError(fmt::format("Operand must be a number, got {}.", typeToStr(val)), 3);
    t->push(val);
    val = encodeNumber(decodeNumber(val) + (static_cast<int8_t>((arg & 0b00000001)*2) - 1));
    // True: prefix, false: postfix
    if(arg & 0b00000010) t->stackTop[-1] = val;
}

__attribute__((noinline)) static void printRuntimeError(CallFrame* frames, uint16_t frameCount, runtime::VM* vm, int errCode, string error){
    auto cyan = fmt::fg(fmt::color::cyan);
    auto white = fmt::fg(fmt::color::white);
    auto red = fmt::fg(fmt::color::red);
    auto yellow = fmt::fg(fmt::color::yellow);
    std::cout<<fmt::format("{} \n{}\n", fmt::styled("Runtime error: ", red), error);
    //prints callstack
    for (int i = frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &frames[i];
        object::ObjFunc* function = frame->closure->func;
        // Converts ip from a pointer to a index in the array
        uInt64 instruction = (frame->ip - 1) - vm->code.bytecode.data();
        codeLine line = vm->code.getLine(instruction);
        //fileName:line | in <func name>
        std::cout<<fmt::format("{}:{} | in {}\n",
                               fmt::styled(line.getFileName(vm->sourceFiles), yellow),
                               fmt::styled(std::to_string(line.line + 1), cyan),
                               (function->name.length() == 0 ? "script" : function->name));
    }
    fmt::print("\nExited with code: {}\n", errCode);
}

#pragma endregion

void runtime::Thread::executeBytecode() {

	#ifdef DEBUG_TRACE_EXECUTION
	std::cout << "-------------Code execution starts-------------\n";
	#endif // DEBUG_TRACE_EXECUTION
	// C++ is more likely to put these locals in registers which speeds things up
	CallFrame* frame = &frames[frameCount - 1];
    byte* ip = &vm->code.bytecode[frame->closure->func->bytecodeOffset];
	Value* slotStart = frame->slots;
    uint64_t constantOffset = frame->closure->func->constantsOffset;
    Value* constants = vm->code.constants.data();


	#pragma region Helpers & Macros
	#define READ_BYTE() (*ip++)
	#define READ_SHORT() (ip += 2, static_cast<uint16_t>((ip[-2] << 8) | ip[-1]))
	#define READ_CONSTANT() (constants[constantOffset + READ_BYTE()])
	#define READ_CONSTANT_LONG() (constants[constantOffset + READ_SHORT()])
	#define READ_STRING() (asString(READ_CONSTANT()))
	#define READ_STRING_LONG() (asString(READ_CONSTANT_LONG()))

	// Stores the ip to the current frame before a new one is pushed
	#define STORE_FRAME() frame->ip = ip
	// When a frame is pushed/popped load its variables to the locals
	#define LOAD_FRAME() (													\
	frame = &frames[frameCount - 1],										\
	slotStart = frame->slots,												\
	ip = frame->ip,                                                         \
    constantOffset = frame->closure->func->constantsOffset)

	#define BINARY_OP(op)                                                                                                                               \
        Value a = peek(1), b = peek(0);                                                                                                                 \
        if (!isNumber(a) || !isNumber(b)) { runtimeError(fmt::format("Operands must be numbers, got '{}' and '{}'.", typeToStr(a), typeToStr(b)), 3); } \
        *(--stackTop - 1) = encodeNumber(decodeNumber(a) op decodeNumber(b))                                                                            \


	#define INT_BINARY_OP(op)                                                                                                                      \
        Value a = peek(1), b = peek(0);                                                                                                            \
        if (!isInt(a) || !isInt(b)) { runtimeError(fmt::format("Operands must be integers, got '{}' and '{}'.", typeToStr(a), typeToStr(b)), 3); } \
        *(--stackTop - 1) = encodeNumber(decodeInt(a) op decodeInt(b));                                                                            \

    #pragma endregion

    #define DISPATCH() goto loop;
    try {
        loop:
        if(pauseToken.load(std::memory_order_relaxed)) {
            if(handlePauseToken(this, asFuture(stack[0]))) return;
        }

        #ifdef DEBUG_TRACE_EXECUTION
        std::cout << "          ";
            for (Value* slot = stack; slot < stackTop; slot++) {
                std::cout << "[";
                (*slot).print();
                std::cout << "] ";
            }
            std::cout << "\n";
            disassembleInstruction(&vm->code, ip - vm->code.bytecode.data(), frame->closure->func->constantsOffset);
        #endif
        switch(READ_BYTE()) {
            #pragma region Helper opcodes
            case +OpCode::POP:{
                stackTop--;
                DISPATCH();
            }
            case +OpCode::POPN:{
                uint8_t nToPop = READ_BYTE();
                stackTop -= nToPop;
                DISPATCH();
            }
            case +OpCode::LOAD_INT:{
                push(encodeNumber(READ_BYTE()));
                DISPATCH();
            }
            #pragma endregion

            #pragma region Constant opcodes
            case +OpCode::CONSTANT:
                push(READ_CONSTANT());
                DISPATCH();
            case +OpCode::CONSTANT_LONG:
                push(READ_CONSTANT_LONG());
                DISPATCH();
            case +OpCode::NIL:
                push(encodeNil());
                DISPATCH();
            case +OpCode::TRUE:
                push(encodeBool(true));
                DISPATCH();
            case +OpCode::FALSE:
                push(encodeBool(false));
                DISPATCH();
            #pragma endregion

            #pragma region Unary opcodes
            case +OpCode::NEGATE:{
                Value val = peek(0);
                if (!isNumber(val)) {
                    runtimeError(fmt::format("Operand must be a number, got {}.", typeToStr(val)), 3);
                }
                *(stackTop - 1) ^= (1ll << 63);
                DISPATCH();
            }
            case +OpCode::NOT:{
                push(encodeBool(isFalsey(pop())));
                DISPATCH();
            }
            case +OpCode::BIN_NOT:{
                if (!isNumber(peek(0))) {
                    runtimeError(fmt::format("Operand must be a number, got {}.", typeToStr(peek(0))), 3);
                }
                if (!isInt(peek(0))) {
                    runtimeError("Number must be a integer, got a float.", 3);
                }
                stackTop[-1] = encodeNumber(~decodeInt(peek(0)));
                DISPATCH();
            }
            case +OpCode::INCREMENT:{
                byte arg = READ_BYTE();

                byte type = arg >> 2;

                #define INCREMENT(val) tryIncrement(this, arg, val); DISPATCH();

                switch (type) {
                    case 0: {
                        byte slot = READ_BYTE();
                        Value &num = slotStart[slot];
                        INCREMENT(num);
                    }
                    case 1: {
                        byte slot = READ_BYTE();
                        Value &num = asUpvalue(slotStart[slot])->val;
                        INCREMENT(num);
                    }
                    case 2: {
                        byte slot = READ_BYTE();
                        Value &num = frame->closure->upvals[slot]->val;
                        INCREMENT(num);
                    }
                    case 3: {
                        byte index = READ_BYTE();
                        Globalvar &var = vm->globals[index];
                        if (!var.isDefined) {
                            runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                        }
                        INCREMENT(var.val);
                    }
                    case 4: {
                        byte index = READ_SHORT();
                        Globalvar &var = vm->globals[index];
                        if (!var.isDefined) {
                            runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                        }
                        INCREMENT(var.val);
                    }
                    case 5:[[fallthrough]];
                    case 6: {
                        Value inst = pop();
                        if (!isInstance(inst)) {
                            runtimeError(
                                    fmt::format("Only instances/structs have properties, got {}.", typeToStr(inst)),
                                    3);
                        }

                        object::ObjInstance *instance = asInstance(inst);
                        object::ObjString *str = (type == 5 ? READ_STRING() : READ_STRING_LONG());

                        auto it = instance->fields.find(str);
                        if (it == instance->fields.end()) {
                            runtimeError(fmt::format("Field '{}' doesn't exist.", str->str), 4);
                        }
                        Value &num = it->second;
                        INCREMENT(num);
                    }
                    case 7: {
                        Value field = pop();
                        Value callee = pop();

                        if (isArray(callee)) {
                            object::ObjArray *arr = asArray(callee);
                            uInt64 index = checkArrayBounds(this, field, callee, arr);
                            Value &num = arr->values[index];
                            INCREMENT(num);
                            return;
                        }
                        // If it's not an array nor a instance, throw type error
                        if (!isHashMap(callee))
                            runtimeError(fmt::format("Expected an array or hash map, got {}.", typeToStr(callee)), 3);
                        if (!isString(field))
                            runtimeError(fmt::format("Expected a string for field name, got {}.", typeToStr(field)), 3);

                        object::ObjHashMap *instance = asHashMap(callee);
                        object::ObjString *str = asString(field);

                        auto it = instance->fields.find(str);
                        if (it == instance->fields.end()) {
                            runtimeError(fmt::format("Field '{}' doesn't exist.", str->str), 4);
                        }
                        Value &num = it->second;
                        INCREMENT(num);
                    }
                    default:
                        runtimeError(fmt::format("Unrecognized argument in OpCode::INCREMENT"), 6);
                }
            }
            #pragma endregion

            #pragma region Binary opcodes
            case +OpCode::BITWISE_XOR: {
                INT_BINARY_OP(^);
                DISPATCH();
            }
            case +OpCode::BITWISE_OR: {
                INT_BINARY_OP(|);
                DISPATCH();
            }
            case +OpCode::BITWISE_AND: {
                INT_BINARY_OP(&);
                DISPATCH();
            }
            case +OpCode::ADD:{
                if (isNumber(peek(0)) && isNumber(peek(1))) {
                    BINARY_OP(+);
                } else if (isString(peek(0)) && isString(peek(1))) {
                    object::ObjString *b = asString(pop());
                    object::ObjString *a = asString(pop());

                    push(encodeObj(a->concat(b)));
                } else {
                    runtimeError(fmt::format("Operands must be two numbers or two strings, got {} and {}.",
                                             typeToStr(peek(1)), typeToStr(peek(0))), 3);
                }
                DISPATCH();
            }
            case +OpCode::SUBTRACT: {
                BINARY_OP(-);
                DISPATCH();
            }
            case +OpCode::MULTIPLY: {
                BINARY_OP(*);
                DISPATCH();
            }
            case +OpCode::DIVIDE: {
                BINARY_OP(/);
                DISPATCH();
            }
            case +OpCode::MOD: {
                INT_BINARY_OP(%);
                DISPATCH();
            }
            case +OpCode::BITSHIFT_LEFT: {
                INT_BINARY_OP(<<);
                DISPATCH();
            }
            case +OpCode::BITSHIFT_RIGHT: {
                INT_BINARY_OP(>>);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Binary opcodes that return bool
            case +OpCode::EQUAL:{
                Value b = pop();
                Value a = pop();
                push(encodeBool(equals(a, b)));
                DISPATCH();
            }
            case +OpCode::NOT_EQUAL:{
                Value b = pop();
                Value a = pop();
                push(encodeBool(!equals(a, b)));
                DISPATCH();
            }
            case +OpCode::GREATER:{
                Value a = peek(1), b = peek(0);
                if (!isNumber(a) || !isNumber(b)) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", typeToStr(peek(1)),
                                             typeToStr(peek(0))), 3);
                }
                *(--stackTop - 1) = encodeBool(decodeNumber(a) > decodeNumber(b));
                DISPATCH();
            }
            case +OpCode::GREATER_EQUAL:{
                //Have to do this because of floating point comparisons
                Value a = peek(1), b = peek(0);
                if (!isNumber(a) || !isNumber(b)) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", typeToStr(peek(1)),
                                             typeToStr(peek(0))), 3);
                }
                *(--stackTop - 1) = encodeBool(decodeNumber(a) >= decodeNumber(b) - DBL_EPSILON);
                DISPATCH();
            }
            case +OpCode::LESS:{
                Value a = peek(1), b = peek(0);
                if (!isNumber(a) || !isNumber(b)) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", typeToStr(peek(1)),
                                             typeToStr(peek(0))), 3);
                }
                *(--stackTop - 1) = encodeBool(decodeNumber(a) < decodeNumber(b));
                DISPATCH();
            }
            case +OpCode::LESS_EQUAL:{
                Value a = peek(1), b = peek(0);
                if (!isNumber(a) || !isNumber(b)) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", typeToStr(peek(1)),
                                             typeToStr(peek(0))), 3);
                }
                *(--stackTop - 1) = encodeBool(decodeNumber(a) < decodeNumber(b) + DBL_EPSILON);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Statements and var
            case +OpCode::GET_NATIVE:{
                push(encodeObj(vm->nativeFuncs[READ_SHORT()]));
                DISPATCH();
            }

            case +OpCode::GET_GLOBAL:{
                byte index = READ_BYTE();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                push(var.val);
                DISPATCH();
            }
            case +OpCode::GET_GLOBAL_LONG:{
                uInt index = READ_SHORT();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                push(var.val);
                DISPATCH();
            }

            case +OpCode::SET_GLOBAL:{
                byte index = READ_BYTE();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                var.val = peek(0);
                DISPATCH();
            }
            case +OpCode::SET_GLOBAL_LONG:{
                uInt index = READ_SHORT();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                var.val = peek(0);
                DISPATCH();
            }

            case +OpCode::GET_LOCAL:{
                push(slotStart[READ_BYTE()]);
                DISPATCH();
            }
            case +OpCode::SET_LOCAL:{
                slotStart[READ_BYTE()] = peek(0);
                DISPATCH();
            }

            case +OpCode::CREATE_UPVALUE: {
                uint8_t slot = READ_BYTE();
                auto* upval = new object::ObjUpval(slotStart[slot]);
                slotStart[slot] = encodeObj(upval);
                DISPATCH();
            }

            case +OpCode::GET_LOCAL_UPVALUE:{
                push(asUpvalue(slotStart[READ_BYTE()])->val);
                DISPATCH();
            }
            case +OpCode::SET_LOCAL_UPVALUE:{
                asUpvalue(slotStart[READ_BYTE()])->val = peek(0);
                DISPATCH();
            }

            case +OpCode::GET_UPVALUE:{
                uint8_t slot = READ_BYTE();
                push(frame->closure->upvals[slot]->val);
                DISPATCH();
            }
            case +OpCode::SET_UPVALUE:{
                uint8_t slot = READ_BYTE();
                frame->closure->upvals[slot]->val = peek(0);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Control flow
            case +OpCode::JUMP:{
                uint16_t offset = READ_SHORT();
                ip += offset;
                DISPATCH();
            }

            case +OpCode::JUMP_IF_FALSE:{
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) ip += offset;
                DISPATCH();
            }
            case +OpCode::JUMP_IF_TRUE:{
                uint16_t offset = READ_SHORT();
                if (!isFalsey(peek(0))) ip += offset;
                DISPATCH();
            }
            case +OpCode::JUMP_IF_FALSE_POP:{
                uint16_t offset = READ_SHORT();
                if (isFalsey(pop())) ip += offset;
                DISPATCH();
            }

            case +OpCode::LOOP_IF_TRUE:{
                uint16_t offset = READ_SHORT();
                if (!isFalsey(pop())) ip -= offset;
                DISPATCH();
            }
            case +OpCode::LOOP:{
                uint16_t offset = READ_SHORT();
                ip -= offset;
                DISPATCH();
            }

            case +OpCode::JUMP_POPN:{
                stackTop -= READ_BYTE();
                ip += READ_SHORT();
                DISPATCH();
            }

            case +OpCode::SWITCH:{
                Value val = pop();
                uInt caseNum = READ_SHORT();
                // Offset into jump indexes
                byte *offset = ip + caseNum;
                // Place in the bytecode where the jump is held
                byte *jumpOffset = nullptr;
                for (int i = 0; i < caseNum; i++) {
                    if (val == READ_CONSTANT()) {
                        jumpOffset = offset + (i * 2);
                        break;
                    }
                }
                // Default
                if (!jumpOffset) jumpOffset = offset + caseNum * 2;
                ip = jumpOffset;
                uInt debug = ip - vm->code.bytecode.data();
                uInt jmp = READ_SHORT();
                ip += jmp;
                DISPATCH();
            }
            case +OpCode::SWITCH_LONG:{
                Value val = pop();
                uInt caseNum = READ_SHORT();
                // Offset into jump indexes
                byte *offset = ip + caseNum * 2;
                // Place in the bytecode where the jump is held
                byte *jumpOffset = nullptr;
                for (int i = 0; i < caseNum; i++) {
                    if (val == READ_CONSTANT_LONG()) {
                        jumpOffset = offset + (i * 2);
                        break;
                    }
                }
                if (!jumpOffset) jumpOffset = offset + caseNum * 2;
                ip = jumpOffset;
                uInt jmp = READ_SHORT();
                ip += jmp;
                DISPATCH();
            }
            #pragma endregion

            #pragma region Functions
            case +OpCode::CALL:
            {
                // How many values are on the stack right now
                int argCount = READ_BYTE();
                STORE_FRAME();
                callValue(peek(argCount), argCount);
                // If the call is successful, there is a new call frame, so we need to update locals
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::RETURN:
            {
                Value result = pop();
                frameCount--;
                // If we're returning from the implicit function
                if (frameCount == 0) {
                    // Main thread doesn't have a future nor does it need to delete the thread
                    auto fut = asFuture(stack[0]);
                    if (fut == nullptr) return;

                    // If this is a child thread that has a future attached to it, assign the value to the future
                    fut->val = result;
                    deleteThread(fut, vm);
                    return;
                }
                stackTop = slotStart;
                push(result);
                // Update locals with the values of the frame below
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::CLOSURE: [[fallthrough]];
            case +OpCode::CLOSURE_LONG:{
                auto *closure = new object::ObjClosure(asFunction(*(ip - 1) == +OpCode::CLOSURE ? READ_CONSTANT() : READ_CONSTANT_LONG()));
                for (auto &upval: closure->upvals) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        upval = asUpvalue(slotStart[index]);
                    } else {
                        upval = frame->closure->upvals[index];
                    }
                }
                push(encodeObj(closure));
                DISPATCH();
            }
            #pragma endregion

            #pragma region Multithreading
            case +OpCode::LAUNCH_ASYNC:
            {
                byte argCount = READ_BYTE();
                auto *t = new Thread(vm);
                auto *newFut = new object::ObjFuture(t);
                // Ensures that ObjFuture tied to this thread lives long enough for the thread to finish execution
                t->copyVal(encodeObj(newFut));
                // Copies the function being called and the arguments
                t->startThread(&stackTop[-1 - argCount], argCount + 1);
                stackTop -= argCount + 1;
                {
                    // Only one thread can add/remove a new child thread at any time
                    std::lock_guard<std::mutex> lk(vm->mtx);
                    vm->childThreads.push_back(t);
                }
                newFut->startParallelExecution();
                push(encodeObj(newFut));
                DISPATCH();
            }

            case +OpCode::AWAIT:
            {
                Value val = pop();
                if (!isFuture(val))
                    runtimeError(fmt::format("Await can only be applied to a future, got {}", typeToStr(val)), 3);
                object::ObjFuture *futToAwait = asFuture(val);
                futToAwait->fut.wait();
                // Immediately delete the thread object to conserve memory
                deleteThread(futToAwait, vm);
                // Can safely access fut->val from this thread since the value is being read and won't be written to again
                push(futToAwait->val);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Objects, arrays and maps
            case +OpCode::CREATE_ARRAY:{
                uInt64 size = READ_BYTE();
                uInt64 i = 0;
                auto *arr = new object::ObjArray(size);
                while (i < size) {
                    //size-i to because the values on the stack are in reverse order compared to how they're supposed to be in a array
                    Value val = pop();
                    //if numOfHeapPtr is 0 we don't trace or update the array when garbage collecting
                    if (isObj(val)) arr->numOfHeapPtr++;

                    arr->values[size - i - 1] = val;
                    i++;
                }
                push(encodeObj(arr));
                DISPATCH();
            }

            case +OpCode::GET:{
                // Structs and objects also get their own +OpCode::GET_PROPERTY operator for access using '.'
                // Use peek because in case this is a get call to a instance that has a defined "access" method
                // We want to use these 2 values as args and receiver
                Value field = pop();
                Value callee = pop();

                if (isArray(callee)) {
                    object::ObjArray *arr = asArray(callee);
                    uInt64 index = checkArrayBounds(this, field, callee, arr);
                    push(arr->values[index]);
                    DISPATCH();
                    // Only hash maps can be access with [](eg. hashMap["field"]
                } else if (isHashMap(callee)) {
                    if (!isString(field)) {
                        runtimeError(fmt::format("Expected a string for field name, got {}.", typeToStr(field)), 3);
                    }

                    object::ObjHashMap *instance = asHashMap(callee);
                    object::ObjString *name = asString(field);
                    auto it = instance->fields.find(name);
                    if (it != instance->fields.end()) {
                        push(it->second);
                        DISPATCH();
                    }
                    runtimeError(fmt::format("Field '{}' doesn't exist.", name->str), 4);
                }
                runtimeError(fmt::format("Expected an array or hash map, got {}.", typeToStr(callee)), 3);
            }

            case +OpCode::SET:{
                //structs and objects also get their own +OpCode::SET_PROPERTY operator for setting using '.'
                Value field = pop();
                Value callee = pop();
                Value val = peek(0);

                if (isArray(callee)) {
                    object::ObjArray *arr = asArray(callee);
                    uInt64 index = checkArrayBounds(this, field, callee, arr);

                    //if numOfHeapPtr is 0 we don't trace or update the array when garbage collecting
                    if (isObj(val) && !isObj(arr->values[index])) arr->numOfHeapPtr++;
                    else if (!isObj(val) && isObj(arr->values[index])) arr->numOfHeapPtr--;
                    arr->values[index] = val;
                    DISPATCH();
                } else if (isHashMap(callee)) {
                    if (!isString(field)) {
                        runtimeError(fmt::format("Expected a string for field name, got {}.", typeToStr(field)), 3);
                    }

                    object::ObjHashMap *instance = asHashMap(callee);
                    object::ObjString *str = asString(field);
                    //setting will always succeed, and we don't care if we're overriding an existing field, or creating a new one
                    instance->fields.insert_or_assign(str, val);
                    DISPATCH();
                }
                runtimeError(fmt::format("Expected an array or hash map, got {}.", typeToStr(callee)), 3);
            }
            //TODO: implement hash map variation of these ops
            case +OpCode::GET_PROPERTY: [[fallthrough]];
            case +OpCode::GET_PROPERTY_LONG:{
                Value inst = pop();
                object::ObjString *name = (*(ip - 1) == +OpCode::GET_PROPERTY ? READ_STRING() : READ_STRING_LONG());
                object::ObjClass* klass = nullptr;
                if (isInstance(inst)) {
                    object::ObjInstance *instance = asInstance(inst);
                    auto it = instance->fields.find(name);
                    if (it != instance->fields.end()) {
                        push(it->second);
                        DISPATCH();
                    }
                    klass = instance->klass;
                }
                else klass = getClass(vm->nativeClasses, inst);
                bindMethod(klass, name, inst);
                DISPATCH();
            }

            case +OpCode::SET_PROPERTY: [[fallthrough]];
            case +OpCode::SET_PROPERTY_LONG:{
                if (!isInstance(peek(0))) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", typeToStr(peek(0))), 3);
                }
                object::ObjInstance *instance = asInstance(pop());
                auto name = (*(ip - 1) == +OpCode::SET_PROPERTY ? READ_STRING() : READ_STRING_LONG());
                auto it = instance->fields.find(name);
                if (it == instance->fields.end()) {
                    runtimeError(fmt::format("Class '{}' doesn't contain field '{}'", instance->klass->name->str, name->str), 4);
                }
                it->second = peek(0);
                DISPATCH();
            }

            // Still need to do typechecking since 'this' could be a primitive
            case +OpCode::GET_PROPERTY_EFFICIENT:{
                object::ObjString *name = READ_STRING_LONG();
                Value val = *slotStart;
                object::ObjClass* klass = nullptr;
                if (isInstance(val)) {
                    object::ObjInstance *instance = asInstance(val);
                    auto it = instance->fields.find(name);
                    if (it != instance->fields.end()) {
                        push(it->second);
                        DISPATCH();
                    }
                    klass = instance->klass;
                }else klass = getClass(vm->nativeClasses, val);
                bindMethod(klass, name, val);
                DISPATCH();
            }
            case +OpCode::SET_PROPERTY_EFFICIENT:{
                object::ObjString *name = READ_STRING_LONG();
                if (!isInstance(*slotStart)) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", typeToStr(*slotStart)), 3);
                }
                object::ObjInstance *instance = asInstance(*slotStart);
                auto it = instance->fields.find(name);
                if (it == instance->fields.end()) {
                    runtimeError(fmt::format("Class '{}' doesn't contain field '{}'", instance->klass->name->str, name->str), 4);
                }
                it->second = peek(0);
                DISPATCH();
            }

            case +OpCode::INVOKE:{
                //gets the method and calls it immediately, without converting it to a objBoundMethod
                int argCount = READ_BYTE();
                object::ObjString *method = READ_STRING();
                STORE_FRAME();
                invoke(method, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            case +OpCode::INVOKE_LONG:{
                //gets the method and calls it immediately, without converting it to a objBoundMethod
                int argCount = READ_BYTE();
                object::ObjString *method = READ_STRING_LONG();
                STORE_FRAME();
                invoke(method, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            //TODO: fix this
            case +OpCode::INVOKE_FROM_STACK:{
                int argCount = READ_BYTE();
                Value field = pop();
                STORE_FRAME();
                if(isNumber(field) && isArray(peek(argCount))){

                }else if(isString(field) && isHashMap(peek(argCount))){

                }
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::CREATE_STRUCT: [[fallthrough]];
            case +OpCode::CREATE_STRUCT_LONG:{
                bool isShort = (*(ip - 1) == +OpCode::CREATE_STRUCT ? true : false);
                int numOfFields = READ_BYTE();

                //passing null instead of class signals to the VM that this is a struct, and not a instance of a class
                auto *inst = new object::ObjHashMap();

                //the compiler emits the fields in reverse order, so we can loop through them normally and pop the values on the stack
                for (int i = 0; i < numOfFields; i++) {
                    object::ObjString *name = (isShort ? READ_STRING() : READ_STRING_LONG());
                    inst->fields.insert_or_assign(name, pop());
                }
                push(encodeObj(inst));
                DISPATCH();
            }

            case +OpCode::GET_SUPER: [[fallthrough]];
            case +OpCode::GET_SUPER_LONG:{
                //super is ALWAYS followed by a field
                object::ObjString *name = *(ip - 1) == +OpCode::GET_SUPER ? READ_STRING() : READ_STRING_LONG();
                object::ObjClass *superclass = asClass(pop());

                bindMethod(superclass, name, pop());
                DISPATCH();
            }

            case +OpCode::SUPER_INVOKE:{
                //works same as +OpCode::INVOKE, but uses invokeFromClass() to specify the superclass
                int argCount = READ_BYTE();
                object::ObjString *method = READ_STRING();
                object::ObjClass *superclass = asClass(pop());
                STORE_FRAME();
                invokeFromClass(superclass, method, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            case +OpCode::SUPER_INVOKE_LONG:{
                //works same as +OpCode::INVOKE, but uses invokeFromClass() to specify the superclass
                int argCount = READ_BYTE();
                object::ObjString *method = READ_STRING_LONG();
                object::ObjClass *superclass = asClass(pop());
                STORE_FRAME();
                invokeFromClass(superclass, method, argCount);
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::INSTANCEOF:{
                auto klass = asClass(READ_CONSTANT_LONG());
                Value val = pop();
                if(!isInstance(val)){
                    runtimeError(fmt::format("Expected an instance of the class, got {}.", typeToStr(val)), 3);
                }
                auto checkClass = asInstance(val)->klass;
                do{
                    if(checkClass == klass){
                        push(encodeBool(true));
                        DISPATCH();
                    }
                    checkClass = checkClass->superclass;
                }while(checkClass->superclass);
                push(encodeBool(false));
                DISPATCH();
            }
            #pragma endregion
        }
    } catch(int errCode) {
        STORE_FRAME();
        printRuntimeError(frames, frameCount, vm, errCode, errorString);
    }
	#undef READ_BYTE
	#undef READ_SHORT
	#undef READ_CONSTANT
	#undef READ_CONSTANT_LONG
	#undef READ_STRING
	#undef READ_STRING_LONG
	#undef BINARY_OP
	#undef INT_BINARY_OP
}
