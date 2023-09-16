#include "valueHelpers.h"
#include "../ErrorHandling/errorHandler.h"
#include "../Includes/fmt/format.h"
#include "../Codegen/valueHelpersInline.cpp"
#include <iostream>
#include <iomanip>

using namespace object;
using namespace valueHelpers;

string valueHelpers::toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack){
    switch(getType(x)){
        case ValueType::NUMBER:
            if(isInt(x)) return std::to_string(static_cast<int64_t>(round(decodeNumber(x))));
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
    if (isObj(x)) memory::gc->markObj(decodeObj(x));
}

string valueHelpers::typeToStr(Value x) {
    switch (getType(x)) {
        case ValueType::NUMBER: return "<number>";
        case ValueType::BOOL: return "<bool>";
        case ValueType::NIL: return "<null>";
        case ValueType::OBJ:
            Obj* ptr = decodeObj(x);
            switch (ptr->type) {
                case +ObjType::ARRAY: return "<array>";
                case +ObjType::CLASS: return "<class " + asClass(x)->name->str + ">";
                case +ObjType::CLOSURE: return "<function>";
                case +ObjType::INSTANCE: return "<instance>";
                case +ObjType::STRING: return "<string>";
                case +ObjType::FREEVAR: return "<upvalue>";
                case +ObjType::HASH_MAP: return "<hash map>";
                case +ObjType::FILE: return "<file>";
                case +ObjType::MUTEX: return "<mutex>";
                case +ObjType::FUTURE: return "<future>";
                case +ObjType::RANGE: return "<range>";
            }
    }
    return "error, couldn't determine type of value";
}