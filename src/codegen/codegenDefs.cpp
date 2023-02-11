#include "codegenDefs.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Objects/objects.h"
#include "../DebugPrinting/BytecodePrinter.h"
#include "../Includes/fmt/format.h"
#include <iostream>

using namespace object;
using std::get;

Chunk::Chunk() {}

void Chunk::writeData(uint8_t opCode, uInt line, byte fileIndex) {
	bytecode.push_back(opCode);
	if (lines.size() == 0) {
		lines.push_back(codeLine(line, fileIndex));
		return;
	}
	if (lines[lines.size() - 1].line == line) return;
	//if we're on a new line, mark the end of the bytecode for this line
	//when looking up the line of code for a particular OP we check if it's position in 'code' is less than .end of a line
	lines[lines.size() - 1].end = bytecode.size() - 1;
	lines.emplace_back(line, fileIndex);
}

codeLine Chunk::getLine(uInt offset) {
	for (int i = 0; i < lines.size(); i++) {
		codeLine& line = lines[i];
		if (offset < line.end) return line;
	}
	errorHandler::addSystemError(fmt::format("Couldn't show line for bytecode at position: {}", offset));
    return codeLine();
}

void Chunk::disassemble(string name, int startingOffset, int constantsOffset) {
	std::cout << "=======" << name << "=======\n";
	//prints every instruction in chunk
	for (uInt offset = startingOffset; offset < bytecode.size();) {
		offset = disassembleInstruction(this, offset, constantsOffset);
	}
}

//adds the constant to the array and returns it's index, which is used in conjuction with OP_CONSTANT
//first checks if this value already exists, this helps keep the constants array small
//returns index of the constant
uInt Chunk::addConstant(Value val) {
	for (uInt i = 0; i < constants.size(); i++) {
		if (constants[i] == val) return i;
	}
	uInt size = constants.size();
	constants.push_back(val);
	return size;
}

string valueToStr(Value& val, robin_hood::unordered_set<object::Obj*>& stack) {
	switch (val.value.index()) {
	case 0: {
		double num = val.asNumber();
		int prec = IS_INT(num) ? 0 : 5;
		return std::to_string(num).substr(0, std::to_string(num).find(".") + prec);
	}
	case 1:
		return val.asBool() ? "true" : "false";
	case 2: {
        auto ptr = val.asObj();
        if (ptr == nullptr) return "nil";
        if (stack.count(ptr) > 0) return fmt::format("[Circular ref {:#08x}]", reinterpret_cast<uInt64>(ptr));
        stack.insert(ptr);
        string str = ptr->toString(stack);
        stack.erase(ptr);
        return str;
    }
	default:
		std::cout << "Error printing object";
		return "";
	}
}

bool Value::operator== (const Value& other) const {
	if (this->value.index() != other.value.index()) return false;
	switch (this->value.index()) {
	case +ValueType::NUM: return FLOAT_EQ(get<double>(this->value), get<double>(other.value));
	case +ValueType::BOOL: return this->value == other.value;
	case +ValueType::OBJ: {
        switch(get<object::Obj*>(this->value)->type){
            case object::ObjType::STRING:{
                string& str1 = dynamic_cast<ObjString*>(get<object::Obj*>(this->value))->str;
                string& str2 = dynamic_cast<ObjString*>(get<object::Obj*>(other.value))->str;
                return str1 == str2;
            }
            default: return this->value == other.value;
        }
    }
	}
    return false;
}

bool Value::operator!=(const Value& other) const {
	return !(*this == other);
}

void Value::print() {
    robin_hood::unordered_set<object::Obj*> stack;
	std::cout << this->toString(stack);
}

string Value::toString(robin_hood::unordered_set<object::Obj*>& stack){
    return valueToStr(*this, stack);
}

bool Value::isString() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::STRING;
}
bool Value::isFunction() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::FUNC;
}
bool Value::isNativeFn() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::NATIVE;
}
bool Value::isBoundNativeFunc() const {
    return isObj() && get<object::Obj*>(value)->type == ObjType::BOUND_NATIVE;
}
bool Value::isArray() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::ARRAY;
}
bool Value::isClosure() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::CLOSURE;
}
bool Value::isClass() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::CLASS;
}
bool Value::isInstance() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::INSTANCE;
}
bool Value::isBoundMethod() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::BOUND_METHOD;
}
bool Value::isUpvalue() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::UPVALUE;
}
bool Value::isFile() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::FILE;
}
bool Value::isMutex() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::MUTEX;
}
bool Value::isFuture() const {
	return isObj() && get<object::Obj*>(value)->type == ObjType::FUTURE;
}


object::ObjString* Value::asString() {
	return dynamic_cast<ObjString*>(get<object::Obj*>(value));
}
object::ObjFunc* Value::asFunction() {
	return dynamic_cast<ObjFunc*>(get<object::Obj*>(value));
}
object::ObjNativeFunc* Value::asNativeFn() {
	return dynamic_cast<ObjNativeFunc*>(get<object::Obj*>(value));
}
object::ObjBoundNativeFunc* Value::asBoundNativeFunc(){
    return dynamic_cast<ObjBoundNativeFunc*>(get<object::Obj*>(value));
}
object::ObjArray* Value::asArray() {
	return dynamic_cast<ObjArray*>(get<object::Obj*>(value));
}
object::ObjClosure* Value::asClosure() {
	return dynamic_cast<ObjClosure*>(get<object::Obj*>(value));
}
object::ObjClass* Value::asClass() {
	return dynamic_cast<ObjClass*>(get<object::Obj*>(value));
}
object::ObjInstance* Value::asInstance() {
	return dynamic_cast<ObjInstance*>(get<object::Obj*>(value));
}
object::ObjBoundMethod* Value::asBoundMethod() {
	return dynamic_cast<ObjBoundMethod*>(get<object::Obj*>(value));
}
object::ObjUpval* Value::asUpvalue() {
	return dynamic_cast<ObjUpval*>(get<object::Obj*>(value));
}
object::ObjFile* Value::asFile() {
	return dynamic_cast<ObjFile*>(get<object::Obj*>(value));
}
object::ObjMutex* Value::asMutex() {
	return dynamic_cast<ObjMutex*>(get<object::Obj*>(value));
}
object::ObjFuture* Value::asFuture() {
	return dynamic_cast<ObjFuture*>(get<object::Obj*>(value));
}

void Value::mark() {
	if (isObj()) memory::gc.markObj(get<object::Obj*>(value));
}

string Value::typeToStr() {
	switch (value.index()) {
	case 0: return "<number>";
	case 1: return "<bool>";
	case 2:
		object::Obj* temp = get<object::Obj*>(value);
		if (!temp) return "<nil>";
		switch (temp->type) {
		case object::ObjType::ARRAY: return "<array>";
		case object::ObjType::BOUND_METHOD: return "<method>";
		case object::ObjType::CLASS: return "<class " + asClass()->name + ">";
		case object::ObjType::CLOSURE: return "<function>";
		case object::ObjType::FUNC: return "<function>";
		case object::ObjType::INSTANCE: return asInstance()->klass == nullptr ? "<struct>" : "<instance>";
		case object::ObjType::NATIVE: return "<native function>";
		case object::ObjType::STRING: return "<string>";
		case object::ObjType::UPVALUE: return "<upvalue>";
        case ObjType::BOUND_NATIVE: return "<native function>";
        case ObjType::FILE: return "<file>";
        case ObjType::MUTEX: return "<mutex>";
        case ObjType::FUTURE: return "<future>";
        }
	}
	return "error, couldn't determine type of value";
}