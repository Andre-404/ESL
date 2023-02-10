#include "BytecodePrinter.h"
#include "../Includes/fmt/format.h"
#include "../Objects/objects.h"
#include <iostream>


static int simpleInstruction(string name, int offset) {
	std::cout << name << "\n";
	return offset + 1;
}

static int byteInstruction(string name, Chunk* chunk, int offset) {
	uint8_t slot = chunk->bytecode[offset + 1];
	std::cout << fmt::format("{:16} {:4d}", name, slot) << std::endl;
	return offset + 2;
}

static int shortInstruction(string name, Chunk* chunk, int offset){
    uint8_t slot = (chunk->bytecode[offset + 1] << 8) | chunk->bytecode[offset + 2];
    std::cout << fmt::format("{:16} {:4d}", name, slot) << std::endl;
    return offset + 3;
}

static int constantInstruction(string name, Chunk* chunk, int offset, bool isLong, int constantsOffset) {
	uInt constant = 0;
	if (!isLong) constant = chunk->bytecode[offset + 1];
	else constant = ((chunk->bytecode[offset + 1] << 8) | chunk->bytecode[offset + 2]);
	std::cout << fmt::format("{:16} {:4d} ", name, constantsOffset + constant);
	chunk->constants[constantsOffset + constant].print();
	std::cout<<"\n";
	return offset + (isLong ? 3 : 2);
}

static int globalInstruction(string name, Chunk* chunk, int offset, bool isLong) {
	uInt index = 0;
	if (!isLong) index = chunk->bytecode[offset + 1];
	else index = ((chunk->bytecode[offset + 1] << 8) | chunk->bytecode[offset + 2]);
	std::cout << fmt::format("{:16} {:4d} \n", name, index);
	return offset + (isLong ? 3 : 2);
}

static int jumpInstruction(string name, int sign, Chunk* chunk, int offset) {
	uint16_t jump = (uint16_t)(chunk->bytecode[offset + 1] << 8);
	jump |= chunk->bytecode[offset + 2];
	std::cout << fmt::format("{:16} {:4d} -> {:4d}", name, offset, offset + 3 + sign * jump) << std::endl;
	return offset + 3;
}

static int invokeInstruction(string name, Chunk* chunk, int offset, int constantsOffset) {
	uint8_t constant = chunk->bytecode[offset + 1];
	uint8_t argCount = chunk->bytecode[offset + 2];
	std::cout << fmt::format("{:16} ({} args) {:4d} ", name, argCount, constantsOffset + constant);
	chunk->constants[constantsOffset + constant].print();
	std::cout << "\n";
	return offset + 3;
}

static int longInvokeInstruction(string name, Chunk* chunk, int offset, int constantsOffset) {
	uint8_t constant = (chunk->bytecode[offset + 1] | chunk->bytecode[offset + 2] | (chunk->bytecode[offset + 3] << 16));
	uint8_t argCount = chunk->bytecode[offset + 4];
	std::cout << fmt::format("{:16} ({} args) {:4d}", name, argCount, constantsOffset + constant);
	chunk->constants[constantsOffset + constant].print();
	std::cout << "'\n";
	return offset + 5;
}

int disassembleInstruction(Chunk* chunk, int offset, int constantsOffset) {
	std::cout << fmt::format("{:0>4d} ", offset);

	if (offset > 0 && chunk->getLine(offset).line == chunk->getLine(offset - 1).line) {
		std::cout << "   | ";
	}
	else {
		std::cout << fmt::format("{:4d} ", chunk->getLine(offset).line);
	}

	uint8_t instruction = chunk->bytecode[offset];
	switch (instruction) {
	case +OpCode::POP:
		return simpleInstruction("OP POP", offset);
	case +OpCode::POPN:
		return byteInstruction("OP POPN", chunk, offset);
	case +OpCode::CONSTANT:
		return constantInstruction("OP_CONSTANT", chunk, offset, false, constantsOffset);
	case +OpCode::CONSTANT_LONG:
		return constantInstruction("OP CONSTANT LONG", chunk, offset, true, constantsOffset);
	case +OpCode::NIL:
		return simpleInstruction("OP NIL", offset);
	case +OpCode::TRUE:
		return simpleInstruction("OP TRUE", offset);
	case +OpCode::FALSE:
		return simpleInstruction("OP FALSE", offset);
	case +OpCode::NEGATE:
		return simpleInstruction("OP NEGATE", offset);
	case +OpCode::NOT:
		return simpleInstruction("OP NOT", offset);
	case +OpCode::BIN_NOT:
		return simpleInstruction("OP BIN NOT", offset);
	case +OpCode::INCREMENT: {
		offset++;
		byte args = chunk->bytecode[offset++];
		string sign = (args & 0b00000001) == 0 ? "--" : "++";
		string fix = (args & 0b00000010) == 0 ? "postfix" : "prefix";
		byte type = (args >> 2);
		switch (type) {
		case 0: {
			std::cout << fmt::format("OP INCREMENT {} {} local: {}", sign, fix, chunk->bytecode[offset++]) << std::endl; break;
		}
		case 1: {
			std::cout << fmt::format("OP INCREMENT {} {} upvalue: {}", sign, fix, chunk->bytecode[offset++]) << std::endl; break;
		}
		case 2: {
			uInt constant = constantsOffset + chunk->bytecode[offset++];
			std::cout << fmt::format("OP INCREMENT {} {} global: {} \n", sign, fix, constant);
			break;
		}
		case 3: {
			uInt constant = chunk->bytecode[offset++];
			constant |= chunk->bytecode[offset++];
			std::cout << fmt::format("OP INCREMENT {} {} global 16-bit: {} \n", sign, fix, constantsOffset +constant);
			break;
		}
		case 4: {
			uInt constant = constantsOffset + chunk->bytecode[offset++];
			std::cout << fmt::format("OP INCREMENT {} {} dot access: {} ", sign, fix, constant);
			chunk->constants[constant].print();
			std::cout << std::endl;
			break;
		}
		case 5: {
			uInt constant = constantsOffset + chunk->bytecode[offset++];
			constant |= chunk->bytecode[offset++];
			std::cout << fmt::format("OP INCREMENT {} {} dot access 16-bit: {} ", sign, fix, constant);
			chunk->constants[constant].print();
			std::cout << std::endl;
			break;
		}
		case 6: {
			std::cout << fmt::format("OP INCREMENT {} {} field access", sign, fix) << std::endl; break;
		}
		}
		return offset;
	}
	case +OpCode::BITWISE_XOR:
		return simpleInstruction("OP BITWISE XOR", offset);
	case +OpCode::BITWISE_OR:
		return simpleInstruction("OP BITWISE OR", offset);
	case +OpCode::BITWISE_AND:
		return simpleInstruction("OP BITWISE AND", offset);
	case +OpCode::ADD:
		return simpleInstruction("OP ADD", offset);
	case +OpCode::SUBTRACT:
		return simpleInstruction("OP SUBTRACT", offset);
	case +OpCode::MULTIPLY:
		return simpleInstruction("OP MULTIPLY", offset);
	case +OpCode::DIVIDE:
		return simpleInstruction("OP DIVIDE", offset);
	case +OpCode::MOD:
		return simpleInstruction("OP MOD", offset);
	case +OpCode::BITSHIFT_LEFT:
		return simpleInstruction("OP BITSHIFT_LEFT", offset);
	case +OpCode::BITSHIFT_RIGHT:
		return simpleInstruction("OP BITSHIFT_RIGHT", offset);
	case +OpCode::LOAD_INT:
		return byteInstruction("OP LOAD INT", chunk, offset);
	case +OpCode::EQUAL:
		return simpleInstruction("OP EQUAL", offset);
	case +OpCode::NOT_EQUAL:
		return simpleInstruction("OP NOT EQUAL", offset);
	case +OpCode::GREATER:
		return simpleInstruction("OP GREATER", offset);
	case +OpCode::GREATER_EQUAL:
		return simpleInstruction("OP GREATER EQUAL", offset);
	case +OpCode::LESS:
		return simpleInstruction("OP LESS", offset);
	case +OpCode::LESS_EQUAL:
		return simpleInstruction("OP LESS EQUAL", offset);
	case +OpCode::PRINT:
		return simpleInstruction("OP PRINT", offset);
    case +OpCode::GET_NATIVE:
         return shortInstruction("OP GET NATIVE", chunk, offset);
	case +OpCode::DEFINE_GLOBAL:
		return globalInstruction("OP DEFINE GLOBAL", chunk, offset, false);
	case +OpCode::DEFINE_GLOBAL_LONG:
		return globalInstruction("OP DEFINE GLOBAL LONG", chunk, offset, true);
	case +OpCode::GET_GLOBAL:
		return globalInstruction("OP GET GLOBAL", chunk, offset, false);
	case +OpCode::GET_GLOBAL_LONG:
		return globalInstruction("OP GET GLOBAL LONG", chunk, offset, true);
	case +OpCode::SET_GLOBAL:
		return globalInstruction("OP SET GLOBAL", chunk, offset, false);
	case +OpCode::SET_GLOBAL_LONG:
		return globalInstruction("OP SET GLOBAL LONG", chunk, offset, true);
	case +OpCode::GET_LOCAL:
		return byteInstruction("OP GET LOCAL", chunk, offset);
	case +OpCode::SET_LOCAL:
		return byteInstruction("OP SET LOCAL", chunk, offset);
	case +OpCode::GET_UPVALUE:
		return byteInstruction("OP GET UPVALUE", chunk, offset);
	case +OpCode::SET_UPVALUE:
		return byteInstruction("OP SET UPVALUE", chunk, offset);
	case +OpCode::CREATE_ARRAY:
		return byteInstruction("OP CREATE ARRAY", chunk, offset);
	case +OpCode::GET:
		return simpleInstruction("OP GET", offset);
	case +OpCode::SET:
		return byteInstruction("OP SET", chunk, offset);
	case +OpCode::JUMP:
		return jumpInstruction("OP JUMP", 1, chunk, offset);
	case +OpCode::JUMP_IF_FALSE:
		return jumpInstruction("OP JUMP IF FALSE", 1, chunk, offset);
	case +OpCode::JUMP_IF_TRUE:
		return jumpInstruction("OP JUMP IF TRUE", 1, chunk, offset);
	case +OpCode::JUMP_IF_FALSE_POP:
		return jumpInstruction("OP JUMP IF FALSE POP", 1, chunk, offset);
	case +OpCode::LOOP_IF_TRUE:
		return jumpInstruction("OP LOOP IF TRUE", -1, chunk, offset);
	case +OpCode::LOOP:
		return jumpInstruction("OP LOOP", -1, chunk, offset);
	case +OpCode::JUMP_POPN: {
		uint16_t toPop = chunk->bytecode[offset + 1];
		uint16_t jump = (uint16_t)(chunk->bytecode[offset + 2] << 8);
		jump |= chunk->bytecode[offset + 3];
		std::cout << fmt::format("{:16} {:4d} -> {} POP {}", "OP JUMP POPN", offset, offset + 4 + jump, toPop) << std::endl;
		return offset + 4;
	}
	case +OpCode::SWITCH: {
		offset++;
		uInt16 numOfConstants = static_cast<uInt16>(chunk->bytecode[offset++] << 8);
		numOfConstants |= chunk->bytecode[offset++];
		std::cout << fmt::format("{:16} {:4d} ", "OP SWITCH", numOfConstants) << std::endl;
		uInt jumps = offset + numOfConstants;

		for (int i = 0; i < numOfConstants; i++) {
			uInt constant = constantsOffset + chunk->bytecode[offset++];
			std::cout << fmt::format("{:0>4d}    | {:16} {:4d} ", offset - 1, "CASE CONSTANT", constant);
			chunk->constants[constant].print();
			uInt16 caseJmp = (uInt16)(chunk->bytecode[jumps + i * 2] << 8) | chunk->bytecode[(jumps + i * 2) + 1];
			std::cout << fmt::format(" {} -> {:4d}", jumps + i * 2, jumps + i * 2 + 2 + caseJmp) << std::endl;
		}
		uInt16 defaultJmp = static_cast<uInt16>(chunk->bytecode[jumps + numOfConstants * 2] << 8) | chunk->bytecode[(jumps + numOfConstants * 2) + 1];
		std::cout << fmt::format("{:0>4d}    | {:16} {} -> {:4d} ", jumps + numOfConstants * 2, "DEFAULT CASE", jumps + numOfConstants * 2, jumps + numOfConstants * 2 + 2+ defaultJmp) << std::endl;
		return jumps + (numOfConstants + 1) * 2;
	}
	case +OpCode::SWITCH_LONG: {
		offset++;
		uInt16 numOfConstants = (uInt16)(chunk->bytecode[offset++] << 8);
		numOfConstants |= chunk->bytecode[offset++];
		std::cout << fmt::format("{:16} {:4d} ", "OP SWITCH LONG", numOfConstants) << std::endl;;
		uInt jumps = offset + numOfConstants*2;

		for (int i = 0; i < numOfConstants; i++) {
			uInt constant = constantsOffset + (static_cast<uInt16>(chunk->bytecode[offset] << 8) | chunk->bytecode[offset + 1]);
			std::cout << fmt::format("{:0>4d}    | {:16} {:4d} ", offset, "CASE CONSTANT", constant);
			chunk->constants[constant].print();
			uInt16 caseJmp = (uInt16)(chunk->bytecode[jumps + i * 2] << 8) | chunk->bytecode[(jumps + i * 2) + 1];
			std::cout << fmt::format(" {} -> {:4d}", jumps + i * 2, jumps + i * 2 + 2 + caseJmp) << std::endl;
			offset += 2;
		}
		uInt16 defaultJmp = static_cast<uInt16>(chunk->bytecode[jumps + numOfConstants * 2] << 8) | chunk->bytecode[(jumps + numOfConstants * 2) + 1];
		std::cout << fmt::format("{:0>4d}    | {:16} -> {:4d} ", jumps + numOfConstants * 2, "DEFAULT CASE", jumps + numOfConstants * 2 + 2 + defaultJmp) << std::endl;
		return jumps + (numOfConstants + 1) * 2;
	}
	case +OpCode::CALL:
		return byteInstruction("OP CALL", chunk, offset);
	case +OpCode::RETURN:
		return simpleInstruction("OP RETURN", offset);
	case +OpCode::CLOSURE: {
		offset++;
		uInt constant = constantsOffset + chunk->bytecode[offset++];
		std::cout << fmt::format("{:16} {:4d} ", "OP CLOSURE", constant);
		chunk->constants[constant].print();
		std::cout << std::endl;

		object::ObjFunc* function = chunk->constants[constant].asFunction();
		for (int j = 0; j < function->upvalueCount; j++) {
			int isLocal = chunk->bytecode[offset++];
			int index = chunk->bytecode[offset++];
			std::cout << fmt::format("{:0>4d}    |                     {} index: {}\n", offset - 2, isLocal ? "local" : "upvalue", index) << std::endl;
		}
		return offset;
	}
	case +OpCode::CLOSURE_LONG: {
		offset++;
		uInt constant = constantsOffset + ((chunk->bytecode[offset] << 8) | chunk->bytecode[offset + 1]);
		offset += 2;
		std::cout << fmt::format("{:16} {:4d} ", "OP CLOSURE LONG", constant);
		chunk->constants[constant].print();
		std::cout << std::endl;

		object::ObjFunc* function = chunk->constants[constant].asFunction();
		for (int j = 0; j < function->upvalueCount; j++) {
			int isLocal = chunk->bytecode[offset++];
			int index = chunk->bytecode[offset++];
			std::cout << fmt::format("{:0>4d}    |                     {} index: {}\n", offset - 2, isLocal ? "local" : "upvalue", index) << std::endl;
		}
		return offset;
	}
	case +OpCode::LAUNCH_ASYNC:
		return byteInstruction("OP LAUNCH ASYNC", chunk, offset);
	case +OpCode::AWAIT:
		return simpleInstruction("OP AWAIT", offset);
	case +OpCode::CLASS:
		return constantInstruction("OP CLASS", chunk, offset, true, constantsOffset);
	case +OpCode::GET_PROPERTY:
		return constantInstruction("OP GET PROPERTY", chunk, offset, false, constantsOffset);
	case +OpCode::GET_PROPERTY_LONG:
		return constantInstruction("OP GET PROPERTY LONG", chunk, offset, true, constantsOffset);
	case +OpCode::SET_PROPERTY:
		return constantInstruction("OP SET PROPERTY", chunk, offset, false, constantsOffset);
	case +OpCode::SET_PROPERTY_LONG:
		return constantInstruction("OP SET PROPERTY LONG", chunk, offset, true, constantsOffset);
	case +OpCode::CREATE_STRUCT: {
		offset++;
		uint8_t fieldNum = chunk->bytecode[offset++];
		std::cout << fmt::format("{:16} {:4d}", "OP CREATE STRUCT", fieldNum) << std::endl;
		for (int i = 0; i < fieldNum; i++) {
			uInt constant = constantsOffset + chunk->bytecode[offset++];
			std::cout << fmt::format("{:0>4d}    | {:16} {:4d}", offset - 1, "FIELD CONSTANT", constant) << std::endl;
		}
		return offset;
	}
	case +OpCode::CREATE_STRUCT_LONG: {
		offset++;
		uint8_t fieldNum = chunk->bytecode[offset++];
		std::cout << fmt::format("{:16} {:4d}", "OP CREATE STRUCT LONG", fieldNum) << std::endl;
		for (int i = 0; i < fieldNum; i++) {
			uInt constant = constantsOffset + ((chunk->bytecode[offset] << 8) | chunk->bytecode[offset + 1]);;
			std::cout << fmt::format("{:0>4d}    | {:16} {:4d}", offset, "FIELD CONSTANT", constant) << std::endl;
			offset += 2;
		}
		return offset;
	}
	case +OpCode::METHOD:
		return constantInstruction("OP METHOD", chunk, offset, true, constantsOffset);
	case +OpCode::INVOKE:
		return invokeInstruction("OP INVOKE", chunk, offset, constantsOffset);
	case +OpCode::INVOKE_LONG:
		return longInvokeInstruction("OP INVOKE LONG", chunk, offset, constantsOffset);
	case +OpCode::INHERIT:
		return simpleInstruction("OP INHERIT", offset);
	case +OpCode::GET_SUPER:
		return constantInstruction("OP GET SUPER", chunk, offset, false, constantsOffset);
	case +OpCode::GET_SUPER_LONG:
		return constantInstruction("OP GET SUPER LONG", chunk, offset, true, constantsOffset);
	case +OpCode::SUPER_INVOKE:
		return invokeInstruction("OP SUPER INVOKE", chunk, offset, constantsOffset);
	case +OpCode::SUPER_INVOKE_LONG:
		return longInvokeInstruction("OP SUPER INVOKE LONG", chunk, offset, constantsOffset);
	default:
		std::cout << "Unknown opcode " << (int)instruction << "\n";
		return offset + 1;
	}
}