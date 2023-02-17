#pragma once
#include <variant>
#include "../moduleDefs.h"
#include "../Includes/robinHood.h"
#include "../Objects/objects.h"

namespace object {
	class Obj;

	class ObjString;

	class ObjArray;

	class ObjFunc;

	class ObjNativeFunc;

    class ObjBoundNativeFunc;

	class ObjUpval;

	class ObjClosure;

	class ObjClass;

	class ObjBoundMethod;

	class ObjInstance;

	class ObjFile;

	class ObjMutex;

	class ObjFuture;
}

enum class ValueType {
	DOUBLE,
	BOOL,
	NIL,
    INT,
	OBJ, // This is a pointer
};
inline constexpr unsigned operator+ (ValueType const val) { return static_cast<byte>(val); }

namespace valueHelpers {
    string toString(Value x, std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack = nullptr);
    void print(Value x);
    void mark(Value x);
    string typeToStr(Value x);
}

struct Globalvar {
	string name;
	Value val;
	Globalvar(string _name, Value _val) {
		name = _name;
		val = _val;
	}
};

enum class OpCode {
	//Helpers
	POP,
	POPN,//arg: 8-bit num
	//constants
	CONSTANT,//arg: 8-bit constant index
	CONSTANT_LONG,//arg: 16-bit constant index
	NIL,
	TRUE,
	FALSE,
	//unary
	NEGATE,
	NOT,
	BIN_NOT,
	INCREMENT,//arg: bit flags for type of incrementation and optional 8-16bit arg
	//binary
	BITWISE_XOR,
	BITWISE_OR,
	BITWISE_AND,
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	MOD,
	BITSHIFT_LEFT,
	BITSHIFT_RIGHT,

	LOAD_INT,//arg: 8-bit, integer smaller than 256 to load
	//comparisons and equality
	EQUAL,
	NOT_EQUAL,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,


	//Variables
    GET_NATIVE, //arg: 16-bit index
	//all module level variables(including class and function declarations) are treated as global variables
	//compiler has an array of all globals, and access to globals is done through an array
	DEFINE_GLOBAL,//arg: 8-bit  index
	DEFINE_GLOBAL_LONG,//arg: 16-bit index
	GET_GLOBAL,//arg: 8-bit index
	GET_GLOBAL_LONG,//arg: 16-bit index
	SET_GLOBAL,//arg: 8-bit index
	SET_GLOBAL_LONG,//arg: 16-bit index

	GET_LOCAL,//arg: 8-bit stack position
	SET_LOCAL,//arg: 8-bit stack position
	GET_UPVALUE,//arg: 8-bit upval position
	SET_UPVALUE,//arg: 8-bit upval position
	//Arrays
	CREATE_ARRAY,//arg: 8-bit array size
	//get and set is used by both arrays and instances/structs, since struct.field is just syntax sugar for struct["field"] that
	//gets optimized to use GET_PROPERTY
	GET,
	SET,
	//control flow
	JUMP,//arg: 16-bit jump offset
	JUMP_IF_FALSE,//arg: 16-bit jump offset
	JUMP_IF_TRUE,//arg: 16-bit jump offset
	JUMP_IF_FALSE_POP,//arg: 16-bit jump offset
	LOOP_IF_TRUE,//arg: 16-bit jump offset(gets negated)
	LOOP,//arg: 16-bit jump offset(gets negated)
	JUMP_POPN, //arg: 16-bit jump offset, 8-bit num to pop
	SWITCH, //arg: 16-bit number of constants in cases, followed by 8-bit case constants and 16-bit jump offsets
	SWITCH_LONG, //arg: 16-bit number of constants in cases, followed by 16-bit case constants and 16-bit jump offsets

	//Functions
	CALL,//arg: 8-bit argument count
	RETURN,
	CLOSURE,//arg: 8-bit ObjFunction constant index
	CLOSURE_LONG,//arg: 16-bit ObjFunction constant index

	//Multithreading
	LAUNCH_ASYNC,//arg: 8-bit arg count
	AWAIT,

	//OOP
	CLASS,//arg: 16-bit ObjString constant index
	GET_PROPERTY,//arg: 8-bit ObjString constant index
	GET_PROPERTY_LONG,//arg: 16-bit ObjString constant index
	SET_PROPERTY,//arg: 8-bit ObjString constant index
	SET_PROPERTY_LONG,//arg: 16-bit ObjString constant index
	CREATE_STRUCT,//arg: 8-bit number of fields
	CREATE_STRUCT_LONG,//arg: 16-bit number of fields
	METHOD,//arg: 16-bit ObjString constant index
	INVOKE,//arg: 8-bit ObjString constant index, 8-bit argument count
	INVOKE_LONG,//arg: 16-bit ObjString constant index, 8-bit argument count
	INHERIT,
	GET_SUPER,//arg: 8-bit ObjString constant index
	GET_SUPER_LONG,//arg: 16-bit ObjString constant index
	SUPER_INVOKE,//arg: 8-bit ObjString constant index, 8-bit argument count
	SUPER_INVOKE_LONG,//arg: 16-bit ObjString constant index, 8-bit argument count
};
//conversion from enum to 1 byte number
inline constexpr unsigned operator+ (OpCode const val) { return static_cast<byte>(val); }


struct codeLine {
	uInt end;
	uInt line;
	//index of the file in the array of all source files held by the vm
	byte fileIndex;

	codeLine() {
		line = 0;
		end = 0;
		fileIndex = 0;
	}
	codeLine(uInt _line, byte _fileIndex) {
		line = _line;
		end = 0;
		fileIndex = _fileIndex;
	}

	string getFileName(vector<File*>& files) {
		return files[fileIndex]->name;
	}
};

class Chunk {
public:
	vector<codeLine> lines;
	vector<uint8_t> bytecode;
	vector<Value> constants;
	Chunk();
	void writeData(uint8_t opCode, uInt line, byte fileIndex);
	codeLine getLine(uInt offset);
	void disassemble(string name, int startingOffset, int constantsOffset);
	uInt addConstant(Value val);
};

#define FRAMES_MAX 512
#define STACK_MAX (FRAMES_MAX * 256)

struct CallFrame {
	object::ObjClosure* closure;
	byte* ip;
	Value* slots;
	CallFrame() : closure(nullptr), ip(nullptr), slots(nullptr) {};
};
