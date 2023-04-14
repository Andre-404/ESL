#include "codegenDefs.h"
#include "../ErrorHandling/errorHandler.h"
#include "../DebugPrinting/BytecodePrinter.h"
#include "../Includes/fmt/format.h"
#include "../codegen/valueHelpersInline.cpp"
#include <iostream>
#include <iomanip>

using namespace object;
using namespace valueHelpers;
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
// TODO: Adding constants is O(n)?? Make it O(1) with a hash set
uInt Chunk::addConstant(Value val) {
	for (uInt i = 0; i < constants.size(); i++) {
		if (constants[i] == val) return i;
	}
	uInt size = constants.size();
	constants.push_back(val);
	return size;
}

string valueHelpers::toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack){
    switch(getType(x)){
        case ValueType::NUMBER:
            // TODO: Make custom precision with string streams?
            // TODO: Do some funky stuff with ints
            if(isInt(x)) return std::to_string(static_cast<uInt64>(round(decodeNumber(x))));
            return std::to_string(decodeNumber(x));
        case ValueType::BOOL:
            return (decodeBool(x)) ? "true" : "false";
        case ValueType::NIL:
            return "null";
        case ValueType::OBJ:
            Obj* ptr = decodeObj(x);
            if (!stack) stack = std::make_shared<ankerl::unordered_dense::set<object::Obj*>>();
            if (stack->contains(ptr)) return fmt::format("[Circular ref {:#08x}]", reinterpret_cast<uint64_t>(ptr));
            stack->insert(ptr);
            string str = ptr->toString(stack);
            stack->erase(ptr);
            return str;
    }
    std::cout << "Error printing object.\n";
    return "";
}

void valueHelpers::print(Value x) {
    std::cout << valueHelpers::toString(x);
}

void valueHelpers::mark(Value x){
    if (isObj(x)) memory::gc.markObj(decodeObj(x));
}

string valueHelpers::typeToStr(Value x) {
    switch (getType(x)) {
        case ValueType::NUMBER: return "<number>";
        case ValueType::BOOL: return "<bool>";
        case ValueType::NIL: return "<null>";
        case ValueType::OBJ:
            Obj* ptr = decodeObj(x);
            switch (ptr->type) {
                case ObjType::ARRAY: return "<array>";
                case ObjType::BOUND_METHOD: return "<method>";
                case ObjType::CLASS: return "<class " + asClass(x)->name->str + ">";
                case ObjType::CLOSURE: return "<function>";
                case ObjType::FUNC: return "<function>";
                case ObjType::INSTANCE: return asInstance(x)->klass == nullptr ? "<struct>" : "<instance>";
                case ObjType::NATIVE: return "<native function>";
                case ObjType::STRING: return "<string>";
                case ObjType::UPVALUE: return "<upvalue>";
                case ObjType::HASH_MAP: return "<hash map>";
                case ObjType::FILE: return "<file>";
                case ObjType::MUTEX: return "<mutex>";
                case ObjType::FUTURE: return "<future>";
                case ObjType::RANGE: return "<range>";
            }
    }
    return "error, couldn't determine type of value";
}