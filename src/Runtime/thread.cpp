#include "thread.h"
#include "vm.h"
#include <iostream>
#include <utility>
#include "../Includes/fmt/format.h"

using std::get;

runtime::Thread::Thread(VM* _vm){
	stackTop = stack;
	frameCount = 0;
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


#pragma region Helpers
void runtime::Thread::mark(memory::GarbageCollector* gc) {
	for (Value* i = stack; i < stackTop; i++) {
		i->mark();
	}
	for (int i = 0; i < frameCount; i++) gc->markObj(frames[i].closure);
}

void runtime::Thread::push(Value val) {
	if (stackTop >= stack + STACK_MAX) {
		runtimeError("Stack overflow", 1);
	}
	*stackTop = val;
	stackTop++;
}

Value runtime::Thread::pop() {
	stackTop--;
	return *stackTop;
}

Value runtime::Thread::peek(int depth) {
	return stackTop[-1 - depth];
}

void runtime::Thread::runtimeError(string err, int errorCode) {
    errorString = std::move(err);
    throw errorCode;
}

static bool isFalsey(Value value) {
	return ((value.isBool() && !get<bool>(value.value)) || value.isNil());
}

void runtime::Thread::callValue(Value callee, int argCount) {
	if (callee.isObj()) {
		switch (get<object::Obj*>(callee.value)->type) {
		case object::ObjType::CLOSURE:
			return call(callee.asClosure(), argCount);
		case object::ObjType::NATIVE: {
			int arity = callee.asNativeFn()->arity;
			//if arity is -1 it means that the function takes in a variable number of args
			if (arity != -1 && argCount != arity) {
				runtimeError(fmt::format("Expected {} arguments for function call but got {}.", arity, argCount), 2);
			}
			object::NativeFn native = callee.asNativeFn()->func;
			//native functions throw strings when a error has occured
			try {
				//fiber ptr is passes because a native function might create a new callstack or mutate the stack
				bool shouldPop = native(this, argCount, stackTop - argCount);
				//shouldPop is false if the native function already popped it's arguments(eg. if a native created a new callframe)
				if (shouldPop) {
					//right now the result of the native function sits above the arguments, so we first take the result
					Value top = pop();
					//pop the args + native function itself
					stackTop -= argCount + 1;
					//push the result of the native function back on top
					push(top);
				}
			}
			catch (string str) {
				//globals are guaranteed not to change after the native funcs have been defined
				runtimeError(fmt::format("Error: {}", str), 3);
			}
            return;
		}
		case object::ObjType::CLASS: {
			// We do this so if a GC runs we safely update all the pointers(since the stack is considered a root)
			stackTop[-argCount - 1] = Value(new object::ObjInstance(peek(0).asClass()));
			object::ObjClass* klass = callee.asClass();
			auto it = klass->methods.find(klass->name);
			if (it != klass->methods.end()) {
				return call(it->second.asClosure(), argCount);
			}
			else if (argCount != 0) {
				runtimeError(fmt::format("Class constructor expects 0 arguments but got {}.", argCount), 2);
			}
			return;
		}
		case object::ObjType::BOUND_METHOD: {
			//puts the receiver instance in the 0th slot of the current callframe('this' points to the 0th slot)
			object::ObjBoundMethod* bound = callee.asBoundMethod();
			stackTop[-argCount - 1] = bound->receiver;
			return call(bound->method, argCount);
		}
		default:
			break; // Non-callable object type.
		}
	}
	runtimeError("Can only call functions and classes.", 3);
}

void runtime::Thread::call(object::ObjClosure* closure, int argCount) {
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

object::ObjUpval* captureUpvalue(Value* local) {
	auto* upval = new object::ObjUpval(*local);
	*local = Value(upval);
	return upval;
}

void runtime::Thread::defineMethod(string& name) {
	//no need to typecheck since the compiler made sure to emit code in this order
	Value method = peek(0);
	object::ObjClass* klass = peek(1).asClass();
	klass->methods.insert_or_assign(name, method);
	//we only pop the method, since other methods we're compiling will also need to know their class
	pop();
}

void runtime::Thread::bindMethod(object::ObjClass* klass, string& name) {
	//At the start the instance whose method we're binding needs to be on top of the stack
	Value method;
	auto it = klass->methods.find(klass->name);
	if (it == klass->methods.end()) {
		runtimeError(fmt::format("{} doesn't contain method '{}'.", klass->name, name), 4);
	}
	//peek() to get the ObjInstance
	auto* bound = new object::ObjBoundMethod(peek(0), method.asClosure());
    *(stackTop - 1) = Value(bound);
}

void runtime::Thread::invoke(string& fieldName, int argCount) {
	Value receiver = peek(argCount);
	if (!receiver.isInstance()) {
		runtimeError(fmt::format("Only instances can call methods, got {}.", receiver.typeToStr()), 3);
	}

	object::ObjInstance* instance = receiver.asInstance();
	auto it = instance->fields.find(fieldName);
	if (it != instance->fields.end()) {
		stackTop[-argCount - 1] = it->second;
		return callValue(it->second, argCount);
	}
	//this check is used because we also use objInstance to represent struct literals
	//and if this instance is a struct it can only contain functions inside it's field table
	if (instance->klass == nullptr) {
		runtimeError(fmt::format("Undefined property '{}'.", fieldName), 4);
	}

	invokeFromClass(instance->klass, fieldName, argCount);
}

void runtime::Thread::invokeFromClass(object::ObjClass* klass, string& methodName, int argCount) {
	auto it = klass->methods.find(methodName);
	if (it == klass->methods.end()) {
		runtimeError(fmt::format("Class '{}' doesn't contain '{}'.", klass->name, methodName), 4);
	}
	//the bottom of the call stack will contain the receiver instance
	call(it->second.asClosure(), argCount);
}
#pragma endregion

void runtime::Thread::executeBytecode() {
	#ifdef DEBUG_TRACE_EXECUTION
	std::cout << "-------------Code execution starts-------------\n";
	#endif // DEBUG_TRACE_EXECUTION
	
	// If this is the main thread fut will be nullptr
	object::ObjFuture* fut = stack[0].asFuture();
	// C++ is more likely to put these locals in registers which speeds things up
	CallFrame* frame = &frames[frameCount - 1];
	byte* ip = &vm->code.bytecode[frame->closure->func->bytecodeOffset];
	Value* slotStart = frame->slots;
    uInt constantOffset = frame->closure->func->constantsOffset;


	#pragma region Helpers and Macros
	#define READ_BYTE() (*ip++)
	#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
	#define READ_CONSTANT() (vm->code.constants[constantOffset + READ_BYTE()])
	#define READ_CONSTANT_LONG() (vm->code.constants[constantOffset + READ_SHORT()])
	#define READ_STRING() (READ_CONSTANT().asString())
	#define READ_STRING_LONG() (READ_CONSTANT_LONG().asString())
	auto checkArrayBounds = [&](Value field, Value callee) {
		if (!field.isNumber()) runtimeError(fmt::format("Index must be a number, got {}.", callee.typeToStr()), 3);
		double index = get<double>(field.value);
		object::ObjArray* arr = callee.asArray();
		//Trying to access a variable using a float is a error
		if (!IS_INT(index)) runtimeError("Expected integer, got float.", 3);
		if (index < 0 || index > arr->values.size() - 1)
			runtimeError(fmt::format("Index {} outside of range [0, {}].", (uInt64)index, callee.asArray()->values.size() - 1), 4);
		return static_cast<uInt64>(index);
	};
	auto deleteThread = [](object::ObjFuture* _fut, VM* vm) {
		std::scoped_lock<std::mutex> lk(vm->mtx);
		// Immediately delete the thread object to conserve memory
		for (auto it = vm->childThreads.begin(); it != vm->childThreads.end(); it++) {
			if (*it == _fut->thread) {
				delete* it;
				_fut->thread = nullptr;
				vm->childThreads.erase(it);
				break;
			}
		}
	};

	// Stores the ip to the current frame before a new one is pushed
	#define STORE_FRAME() frame->ip = ip
	// When a frame is pushed/popped load its variables to the locals
	#define LOAD_FRAME() (													\
	frame = &frames[frameCount - 1],										\
	slotStart = frame->slots,												\
	ip = frame->ip,                                                         \
    constantOffset = frame->closure->func->constantsOffset)

	#define BINARY_OP(valueType, op) \
		do { \
			if (!peek(0).isNumber() || !peek(1).isNumber()) { \
				runtimeError(fmt::format("Operands must be numbers, got '{}' and '{}'.", peek(1).typeToStr(), peek(0).typeToStr()), 3); \
			} \
			double b = get<double>(pop().value); \
			Value* a = stackTop - 1; \
			a->value = get<double>(a->value) op b; \
		} while (false)

	#define INT_BINARY_OP(valueType, op)\
		do {\
			if (!peek(0).isNumber() || !peek(1).isNumber()) { \
				runtimeError(fmt::format("Operands must be numbers, got '{}' and '{}'.", peek(1).typeToStr(), peek(0).typeToStr()), 3); \
			} \
			if (!IS_INT(get<double>(peek(0).value)) || !IS_INT(get<double>(peek(1).value))) { \
				runtimeError("Operands must be a integers, got floats.", 3); \
			} \
			uInt64 b = static_cast<uInt64>(get<double>(pop().value)); \
			Value* a = stackTop - 1; \
			a->value = static_cast<double>(static_cast<uInt64>(get<double>(a->value)) op b); \
		} while (false)
    #pragma endregion

    #define DISPATCH() goto loop
    try {
        loop:
        #pragma region Multithreading
        if (!fut && memory::gc.shouldCollect.load()) {
            // If fut is null, this is the main thread of execution which runs the GC
            if (vm->allThreadsPaused()) {
                memory::gc.collect(vm);
            } else {
                // If some threads aren't sleeping yet, use a cond var to wait, every child thread will notify the var when it goes to sleep
                std::unique_lock lk(vm->pauseMtx);
                vm->mainThreadCv.wait(lk, [&] { return vm->allThreadsPaused(); });
                // Release the mutex here so that GC can aquire it
                lk.unlock();
                // After all threads are asleep, run the GC and subsequently awaken all child threads
                memory::gc.collect(vm);
            }
        } else if (fut && memory::gc.shouldCollect.load()) {
            // If this is a child thread and the GC must run, notify the main thread that this one is paused
            // Main thread sends the notification when to awaken
            {
                std::lock_guard<std::mutex> lk(vm->pauseMtx);
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
        #pragma endregion
        #ifdef DEBUG_TRACE_EXECUTION
        std::cout << "          ";
            for (Value* slot = stack; slot < stackTop; slot++) {
                std::cout << "[";
                (*slot).print();
                std::cout << "] ";
            }
            std::cout << "\n";
            disassembleInstruction(&frame->closure->func->body, frames[frameCount - 1].ip);
        #endif
        switch (READ_BYTE()) {
            #pragma region Helper opcodes
            case +OpCode::POP: {
                stackTop--;
                DISPATCH();
            }
            case +OpCode::POPN: {
                uint8_t nToPop = READ_BYTE();
                stackTop -= nToPop;
                DISPATCH();
            }
            case +OpCode::LOAD_INT: {
                push(Value(static_cast<double>(READ_BYTE())));
                DISPATCH();
            }
            #pragma endregion

            #pragma region Constant opcodes
            case +OpCode::CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                DISPATCH();
            }
            case +OpCode::CONSTANT_LONG: {
                Value constant = READ_CONSTANT_LONG();
                push(constant);
                DISPATCH();
            }
            case +OpCode::NIL:
                push(Value::nil());
                DISPATCH();
            case +OpCode::TRUE:
                push(Value(true));
                DISPATCH();
            case +OpCode::FALSE:
                push(Value(false));
                DISPATCH();
            #pragma endregion

            #pragma region Unary opcodes
            case +OpCode::NEGATE: {
                Value val = pop();
                if (!val.isNumber()) {
                    runtimeError(fmt::format("Operand must be a number, got {}.", val.typeToStr()), 3);
                }
                push(Value(-get<double>(val.value)));
                DISPATCH();
            }
            case +OpCode::NOT: {
                push(Value(isFalsey(pop())));
                DISPATCH();
            }
            case +OpCode::BIN_NOT: {
                // Doing implicit conversion from double to long long, could end up with precision errors
                Value val = pop();
                if (!val.isNumber()) {
                    runtimeError(fmt::format("Operand must be a number, got {}.", peek(0).typeToStr()), 3);
                }
                if (!IS_INT(get<double>(val.value))) {
                    runtimeError("Number must be a integer, got a float.", 3);
                }
                double num = get<double>(pop().value);
                // Cursed as shit
                auto temp = static_cast<long long>(num);
                temp = ~temp;
                push(Value(static_cast<double>(temp)));
                DISPATCH();
            }
            case +OpCode::INCREMENT: {
                byte arg = READ_BYTE();
                int8_t sign = (arg & 0b00000001) == 1 ? 1 : -1;
                // True: prefix, false: postfix
                bool isPrefix = (arg & 0b00000010) == 2;

                byte type = arg >> 2;

                auto tryIncrement = [&](Value &val) {
                    if (!val.isNumber())
                        runtimeError(fmt::format("Operand must be a number, got {}.", val.typeToStr()), 3);
                    if (isPrefix) {
                        val.value = get<double>(val.value) + sign;
                        push(val);
                    } else {
                        push(val);
                        val.value = get<double>(val.value) + sign;
                    }
                };

                #define INCREMENT(val) tryIncrement(val); DISPATCH();


                switch (type) {
                    case 0: {
                        byte slot = READ_BYTE();
                        Value &num = slotStart[slot];
                        // If this is a local upvalue
                        if (num.isUpvalue()) {
                            Value &temp = num.asUpvalue()->val;
                            INCREMENT(temp);
                        }
                        INCREMENT(num);
                    }
                    case 1: {
                        byte slot = READ_BYTE();
                        Value &num = frame->closure->upvals[slot]->val;
                        INCREMENT(num);
                    }
                    case 2: {
                        byte index = READ_BYTE();
                        Globalvar &var = vm->globals[index];
                        if (!var.isDefined) {
                            runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                        }
                        INCREMENT(var.val);
                    }
                    case 3: {
                        byte index = READ_SHORT();
                        Globalvar &var = vm->globals[index];
                        if (!var.isDefined) {
                            runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                        }
                        INCREMENT(var.val);
                    }
                    case 4: {
                        Value inst = pop();
                        if (!inst.isInstance()) {
                            runtimeError(
                                    fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()),
                                    3);
                        }

                        object::ObjInstance *instance = inst.asInstance();
                        object::ObjString *str = READ_STRING();
                        auto it = instance->fields.find(str->str);
                        if (it == instance->fields.end()) {
                            runtimeError(fmt::format("Field '{}' doesn't exist.", str->str), 4);
                        }
                        Value &num = it->second;
                        INCREMENT(num);
                    }
                    case 5: {
                        Value inst = pop();
                        if (!inst.isInstance()) {
                            runtimeError(
                                    fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()),
                                    3);
                        }

                        object::ObjInstance *instance = inst.asInstance();
                        object::ObjString *str = READ_STRING_LONG();

                        auto it = instance->fields.find(str->str);
                        if (it == instance->fields.end()) {
                            runtimeError(fmt::format("Field '{}' doesn't exist.", str->str), 4);
                        }
                        Value &num = it->second;
                        INCREMENT(num);
                    }
                    case 6: {
                        Value field = pop();
                        Value callee = pop();
                        if (!callee.isArray() && !callee.isInstance())
                            runtimeError(fmt::format("Expected a array or struct, got {}.", callee.typeToStr()), 3);

                        if (get<object::Obj *>(callee.value)->type == object::ObjType::ARRAY) {
                            object::ObjArray *arr = callee.asArray();
                            uInt64 index = checkArrayBounds(field, callee);
                            Value &num = arr->values[index];
                            INCREMENT(num);
                        }
                        if (!field.isString())
                            runtimeError(fmt::format("Expected a string for field name, got {}.", field.typeToStr()), 3);

                        object::ObjInstance *instance = callee.asInstance();
                        object::ObjString *str = field.asString();

                        auto it = instance->fields.find(str->str);
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
            case +OpCode::BITWISE_XOR:
                INT_BINARY_OP(NUMBER_VAL, ^);
                DISPATCH();
            case +OpCode::BITWISE_OR:
                INT_BINARY_OP(NUMBER_VAL, |);
                DISPATCH();
            case +OpCode::BITWISE_AND:
                INT_BINARY_OP(NUMBER_VAL, &);
                DISPATCH();
            case +OpCode::ADD: {
                if (peek(0).isNumber() && peek(1).isNumber()) {
                    double b = pop().asNumber();
                    Value *a = stackTop - 1;
                    a->value = a->asNumber() + b;
                } else if (peek(0).isString() && peek(1).isString()) {
                    object::ObjString *b = pop().asString();
                    object::ObjString *a = pop().asString();

                    push(Value(a->concat(b)));
                } else {
                    runtimeError(fmt::format("Operands must be two numbers or two strings, got {} and {}.",
                                             peek(1).typeToStr(), peek(0).typeToStr()), 3);
                }
                DISPATCH();
            }
            case +OpCode::SUBTRACT:
                BINARY_OP(NUMBER_VAL, -);
                DISPATCH();
            case +OpCode::MULTIPLY:
                BINARY_OP(NUMBER_VAL, *);
                DISPATCH();
            case +OpCode::DIVIDE:
                BINARY_OP(NUMBER_VAL, /);
                DISPATCH();
            case +OpCode::MOD:
                INT_BINARY_OP(NUMBER_VAL, %);
                DISPATCH();
            case +OpCode::BITSHIFT_LEFT:
                INT_BINARY_OP(NUMBER_VAL, <<);
                DISPATCH();
            case +OpCode::BITSHIFT_RIGHT:
                INT_BINARY_OP(NUMBER_VAL, >>);
                DISPATCH();
            #pragma endregion

            #pragma region Binary opcodes that return bool
            case +OpCode::EQUAL: {
                Value b = pop();
                Value a = pop();
                push(Value(a == b));
                DISPATCH();
            }
            case +OpCode::NOT_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(Value(a != b));
                DISPATCH();
            }
            case +OpCode::GREATER:
                BINARY_OP(BOOL_VAL, >);
                DISPATCH();
            case +OpCode::GREATER_EQUAL: {
                //Have to do this because of floating point comparisons
                if (!peek(0).isNumber() || !peek(1).isNumber()) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", peek(1).typeToStr(),
                                             peek(0).typeToStr()), 3);
                }
                double b = get<double>(pop().value);
                double a = get<double>(pop().value);
                if (a > b || FLOAT_EQ(a, b)) push(Value(true));
                else push(Value(false));
                DISPATCH();
            }
            case +OpCode::LESS:
                BINARY_OP(BOOL_VAL, <);
                DISPATCH();
            case +OpCode::LESS_EQUAL: {
                //Have to do this because of floating point comparisons
                if (!peek(0).isNumber() || !peek(1).isNumber()) {
                    runtimeError(fmt::format("Operands must be two numbers, got {} and {}.", peek(1).typeToStr(),
                                             peek(0).typeToStr()), 3);
                }
                double b = get<double>(pop().value);
                double a = get<double>(pop().value);
                if (a < b || FLOAT_EQ(a, b)) push(Value(true));
                else push(Value(false));
                DISPATCH();
            }
            #pragma endregion

            #pragma region Statements and vars
            case +OpCode::PRINT: {
                pop().print();
                std::cout << "\n";
                DISPATCH();
            }

            case +OpCode::DEFINE_GLOBAL: {
                byte index = READ_BYTE();
                vm->globals[index].val = pop();
                vm->globals[index].isDefined = true;
                DISPATCH();
            }
            case +OpCode::DEFINE_GLOBAL_LONG: {
                uInt index = READ_SHORT();
                vm->globals[index].val = pop();
                vm->globals[index].isDefined = true;
                DISPATCH();
            }

            case +OpCode::GET_GLOBAL: {
                byte index = READ_BYTE();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                push(var.val);
                DISPATCH();
            }
            case +OpCode::GET_GLOBAL_LONG: {
                uInt index = READ_SHORT();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                push(var.val);
                DISPATCH();
            }

            case +OpCode::SET_GLOBAL: {
                byte index = READ_BYTE();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                var.val = peek(0);
                DISPATCH();
            }
            case +OpCode::SET_GLOBAL_LONG: {
                uInt index = READ_SHORT();
                Globalvar &var = vm->globals[index];
                if (!var.isDefined) {
                    runtimeError(fmt::format("Undefined variable '{}'.", var.name), 5);
                }
                var.val = peek(0);
                DISPATCH();
            }

            case +OpCode::GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                Value &val = slotStart[slot];
                if (val.isUpvalue()) {
                    push(val.asUpvalue()->val);
                    DISPATCH();
                }
                push(slotStart[slot]);
                DISPATCH();
            }

            case +OpCode::SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                Value &val = slotStart[slot];
                if (val.isUpvalue()) {
                    val.asUpvalue()->val = peek(0);
                    DISPATCH();
                }
                slotStart[slot] = peek(0);
                DISPATCH();
            }

            case +OpCode::GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(frame->closure->upvals[slot]->val);
                DISPATCH();
            }
            case +OpCode::SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                frame->closure->upvals[slot]->val = peek(0);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Control flow
            case +OpCode::JUMP: {
                uint16_t offset = READ_SHORT();
                ip += offset;
                DISPATCH();
            }

            case +OpCode::JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) ip += offset;
                DISPATCH();
            }
            case +OpCode::JUMP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(peek(0))) ip += offset;
                DISPATCH();
            }
            case +OpCode::JUMP_IF_FALSE_POP: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(pop())) ip += offset;
                DISPATCH();
            }

            case +OpCode::LOOP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(pop())) ip -= offset;
                DISPATCH();
            }
            case +OpCode::LOOP: {
                uint16_t offset = READ_SHORT();
                ip -= offset;
                DISPATCH();
            }

            case +OpCode::JUMP_POPN: {
                stackTop -= READ_BYTE();
                ip += READ_SHORT();
                DISPATCH();
            }

            case +OpCode::SWITCH: {
                Value val = pop();
                uInt caseNum = READ_SHORT();
                // Offset into constant indexes
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
                uInt jmp = READ_SHORT();
                ip += jmp;
                DISPATCH();
            }
            case +OpCode::SWITCH_LONG: {
                Value val = pop();
                uInt caseNum = READ_SHORT();
                // Offset into constant indexes
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
            case +OpCode::CALL: {
                // How many values are on the stack right now
                int argCount = READ_BYTE();
                STORE_FRAME();
                callValue(peek(argCount), argCount);
                // If the call is successful, there is a new call frame, so we need to update locals
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::RETURN: {
                Value result = pop();
                frameCount--;
                // If we're returning from the implicit function
                if (frameCount == 0) {
                    // Main thread doesn't have a future nor does it need to delete the thread
                    if (fut == nullptr) return;

                    // If this is a child thread that has a future attached to it, assign the value to the future
                    fut->val = result;
                    // Since this thread gets deleted by deleteThread, cond var to notify the main thread must be cached in the function
                    std::condition_variable &cv = vm->mainThreadCv;
                    // If execution is finishing and the main thread is waiting to run the gc
                    // notify the main thread after deleting this thread object
                    {
                        // vm->pauseMtx to notify the main thread that this thread doesn't exist anymore,
                        std::scoped_lock<std::mutex> lk(vm->pauseMtx);
                        // deleteThread locks vm->mtx to delete itself from the pool
                        deleteThread(fut, vm);
                    }
                    cv.notify_one();
                    return;
                }
                stackTop = slotStart;
                push(result);
                // Update locals with the values of the frame below
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::CLOSURE: {
                auto *closure = new object::ObjClosure(READ_CONSTANT().asFunction());
                for (auto &upval: closure->upvals) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        upval = captureUpvalue(slotStart + index);
                    } else {
                        upval = frame->closure->upvals[index];
                    }
                }
                push(Value(closure));
                DISPATCH();
            }
            case +OpCode::CLOSURE_LONG: {
                auto *closure = new object::ObjClosure(READ_CONSTANT_LONG().asFunction());
                for (auto &upval: closure->upvals) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        upval = captureUpvalue(slotStart + index);
                    } else {
                        upval = frame->closure->upvals[index];
                    }
                }
                push(Value(closure));
                DISPATCH();
            }
            #pragma endregion

            #pragma region Multithreading
            case +OpCode::LAUNCH_ASYNC: {
                byte argCount = READ_BYTE();
                auto *t = new Thread(vm);
                auto *newFut = new object::ObjFuture(t);
                // Ensures that ObjFuture tied to this thread lives long enough for the thread to finish execution
                t->copyVal(Value(newFut));
                // Copies the function being called and the arguments
                t->startThread(&stackTop[-1 - argCount], argCount + 1);
                stackTop -= argCount + 1;
                {
                    // Only one thread can add/remove a new child thread at any time
                    std::lock_guard<std::mutex> lk(vm->mtx);
                    vm->childThreads.push_back(t);
                }
                newFut->startParallelExecution();
                push(Value(newFut));
                DISPATCH();
            }

            case +OpCode::AWAIT: {
                Value val = pop();
                if (!val.isFuture())
                    runtimeError(fmt::format("Await can only be applied to a future, got {}", val.typeToStr()), 3);
                object::ObjFuture *futToAwait = val.asFuture();
                futToAwait->fut.wait();
                // Immediately delete the thread object to conserve memory
                deleteThread(futToAwait, vm);
                // Can safely access fut->val from this thread since the value is being read and won't be written to again
                push(futToAwait->val);
                DISPATCH();
            }
            #pragma endregion

            #pragma region Objects, arrays and maps
            case +OpCode::CREATE_ARRAY: {
                uInt64 size = READ_BYTE();
                uInt64 i = 0;
                auto *arr = new object::ObjArray(size);
                while (i < size) {
                    //size-i to because the values on the stack are in reverse order compared to how they're supposed to be in a array
                    Value val = pop();
                    //if numOfHeapPtr is 0 we don't trace or update the array when garbage collecting
                    if (val.isObj()) arr->numOfHeapPtr++;

                    arr->values[size - i - 1] = val;
                    i++;
                }
                push(Value(arr));
                DISPATCH();
            }

            case +OpCode::GET: {
                //structs and objects also get their own +OpCode::GET_PROPERTY operator for access using '.'
                //use peek because in case this is a get call to a instance that has a defined "access" method
                //we want to use these 2 values as args and receiver
                Value field = pop();
                Value callee = pop();
                if (!callee.isArray() && !callee.isInstance())
                    runtimeError(fmt::format("Expected a array or struct, got {}.", callee.typeToStr()), 3);

                if (callee.asObj()->type == object::ObjType::ARRAY) {
                    object::ObjArray *arr = callee.asArray();
                    uInt64 index = checkArrayBounds(field, callee);
                    push(arr->values[index]);
                    DISPATCH();
                }
                if (!field.isString())
                    runtimeError(fmt::format("Expected a string for field name, got {}.", field.typeToStr()), 3);

                object::ObjInstance *instance = callee.asInstance();
                object::ObjString *name = field.asString();
                auto it = instance->fields.find(name->str);
                if (it != instance->fields.end()) {
                    push(it->second);
                    DISPATCH();
                }

                if (instance->klass) {
                    bindMethod(instance->klass, name->str);
                    DISPATCH();
                }
                runtimeError(fmt::format("Field '{}' doesn't exist.", name->str), 4);
                DISPATCH();
            }

            case +OpCode::SET: {
                //structs and objects also get their own +OpCode::SET_PROPERTY operator for setting using '.'
                Value field = pop();
                Value callee = pop();
                Value val = peek(0);

                if (!callee.isArray() && !callee.isInstance())
                    runtimeError(fmt::format("Expected a array or struct, got {}.", callee.typeToStr()), 3);
                if (callee.asObj()->type == object::ObjType::ARRAY) {
                    object::ObjArray *arr = callee.asArray();
                    uInt64 index = checkArrayBounds(field, callee);

                    //if numOfHeapPtr is 0 we don't trace or update the array when garbage collecting
                    if (val.isObj() && !arr->values[index].isObj()) arr->numOfHeapPtr++;
                    else if (!val.isObj() && arr->values[index].isObj()) arr->numOfHeapPtr--;
                    arr->values[index] = val;
                    DISPATCH();
                }
                if (!field.isString())
                    runtimeError(fmt::format("Expected a string for field name, got {}.", field.typeToStr()), 3);

                object::ObjInstance *instance = callee.asInstance();
                object::ObjString *str = field.asString();
                //setting will always succeed, and we don't care if we're overriding an existing field, or creating a new one
                instance->fields.insert_or_assign(str->str, val);
                DISPATCH();
            }

            case +OpCode::CLASS: {
                push(Value(new object::ObjClass(READ_STRING_LONG()->str)));
                DISPATCH();
            }

            case +OpCode::GET_PROPERTY: {
                Value inst = pop();
                if (!inst.isInstance()) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()), 3);
                }

                object::ObjInstance *instance = inst.asInstance();
                object::ObjString *name = READ_STRING();

                auto it = instance->fields.find(name->str);
                if (it != instance->fields.end()) {
                    push(it->second);
                    DISPATCH();
                }

                if (instance->klass) {
                    bindMethod(instance->klass, name->str);
                    DISPATCH();
                }
                runtimeError(fmt::format("Field '{}' doesn't exist.", name->str), 4);
                DISPATCH();
            }
            case +OpCode::GET_PROPERTY_LONG: {
                Value inst = pop();
                if (!inst.isInstance()) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()), 3);
                }

                object::ObjInstance *instance = inst.asInstance();
                object::ObjString *name = READ_STRING_LONG();

                auto it = instance->fields.find(name->str);
                if (it != instance->fields.end()) {
                    push(it->second);
                    DISPATCH();
                }

                if (instance->klass) {
                    bindMethod(instance->klass, name->str);
                    DISPATCH();
                }
                runtimeError(fmt::format("Field '{}' doesn't exist.", name->str), 4);
                DISPATCH();
            }

            case +OpCode::SET_PROPERTY: {
                Value inst = pop();
                if (!inst.isInstance()) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()), 3);
                }
                object::ObjInstance *instance = inst.asInstance();

                //we don't care if we're overriding or creating a new field
                instance->fields.insert_or_assign(READ_STRING()->str, peek(0));
                DISPATCH();
            }
            case +OpCode::SET_PROPERTY_LONG: {
                Value inst = pop();
                if (!inst.isInstance()) {
                    runtimeError(fmt::format("Only instances/structs have properties, got {}.", inst.typeToStr()), 3);
                }
                object::ObjInstance *instance = inst.asInstance();

                //we don't care if we're overriding or creating a new field
                instance->fields.insert_or_assign(READ_STRING_LONG()->str, peek(0));
                DISPATCH();
            }

            case +OpCode::CREATE_STRUCT: {
                int numOfFields = READ_BYTE();

                //passing null instead of class signals to the VM that this is a struct, and not a instance of a class
                auto *inst = new object::ObjInstance(nullptr);

                //the compiler emits the fields in reverse order, so we can loop through them normally and pop the values on the stack
                for (int i = 0; i < numOfFields; i++) {
                    object::ObjString *name = READ_STRING();
                    inst->fields.insert_or_assign(name->str, pop());
                }
                push(Value(inst));
                DISPATCH();
            }
            case +OpCode::CREATE_STRUCT_LONG: {
                int numOfFields = READ_BYTE();

                //passing null instead of class signals to the VM that this is a struct, and not a instance of a class
                auto *inst = new object::ObjInstance(nullptr);

                //the compiler emits the fields in reverse order, so we can loop through them normally and pop the values on the stack
                for (int i = 0; i < numOfFields; i++) {
                    object::ObjString *name = READ_STRING_LONG();
                    inst->fields.insert_or_assign(name->str, pop());
                }
                push(Value(inst));
                DISPATCH();
            }

            case +OpCode::METHOD: {
                //class that this method binds too
                defineMethod(READ_STRING_LONG()->str);
                DISPATCH();
            }

            case +OpCode::INVOKE: {
                //gets the method and calls it immediately, without converting it to a objBoundMethod
                object::ObjString *method = READ_STRING();
                int argCount = READ_BYTE();
                STORE_FRAME();
                invoke(method->str, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            case +OpCode::INVOKE_LONG: {
                //gets the method and calls it immediately, without converting it to a objBoundMethod
                object::ObjString *method = READ_STRING_LONG();
                int argCount = READ_BYTE();
                STORE_FRAME();
                invoke(method->str, argCount);
                LOAD_FRAME();
                DISPATCH();
            }

            case +OpCode::INHERIT: {
                Value superclass = peek(1);
                if (!superclass.isClass()) {
                    runtimeError(fmt::format("Superclass must be a class, got {}.", superclass.typeToStr()), 3);
                }
                object::ObjClass *subclass = peek(0).asClass();
                //copy down inheritance
                for (auto it: superclass.asClass()->methods) {
                    subclass->methods.insert_or_assign(it.first, it.second);
                }
                DISPATCH();
            }

            case +OpCode::GET_SUPER: {
                //super is ALWAYS followed by a field
                object::ObjString *name = READ_STRING();
                object::ObjClass *superclass = pop().asClass();

                bindMethod(superclass, name->str);
                DISPATCH();
            }
            case +OpCode::GET_SUPER_LONG: {
                //super is ALWAYS followed by a field
                object::ObjString *name = READ_STRING_LONG();
                object::ObjClass *superclass = pop().asClass();

                bindMethod(superclass, name->str);
                DISPATCH();
            }

            case +OpCode::SUPER_INVOKE: {
                //works same as +OpCode::INVOKE, but uses invokeFromClass() to specify the superclass
                object::ObjString *method = READ_STRING();
                int argCount = READ_BYTE();
                object::ObjClass *superclass = pop().asClass();
                STORE_FRAME();
                invokeFromClass(superclass, method->str, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            case +OpCode::SUPER_INVOKE_LONG: {
                //works same as +OpCode::INVOKE, but uses invokeFromClass() to specify the superclass
                object::ObjString *method = READ_STRING_LONG();
                int argCount = READ_BYTE();
                object::ObjClass *superclass = pop().asClass();
                STORE_FRAME();
                invokeFromClass(superclass, method->str, argCount);
                LOAD_FRAME();
                DISPATCH();
            }
            #pragma endregion
        }
    } catch(int errCode) {
        frame->ip = ip;
        const string cyan = "\u001b[38;5;117m";
        const string black = "\u001b[0m";
        const string red = "\u001b[38;5;196m";
        const string yellow = "\u001b[38;5;220m";
        std::cout << red + "runtime error: \n" + black + errorString + "\n";
        //prints callstack
        for (int i = frameCount - 1; i >= 0; i--) {
            CallFrame* frame = &frames[i];
            object::ObjFunc* function = frame->closure->func;
            // Converts ip from a pointer to a index in the array
            uInt64 instruction = (frame->ip - 1) - vm->code.bytecode.data();
            codeLine line = vm->code.getLine(instruction);
            //fileName:line | in <func name>()
            string temp = yellow + line.getFileName(vm->sourceFiles) + black + ":" + cyan + std::to_string(line.line + 1) + " | " + black;
            std::cout << temp << "in ";
            std::cout << (function->name.length() == 0 ? "script" : function->name) << "()\n";
        }
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
