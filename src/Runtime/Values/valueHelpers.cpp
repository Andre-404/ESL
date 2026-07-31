#include "valueHelpers.h"
#include "../../Includes/fmt/core.h"
#include "valueHelpersInline.h"
#include <iostream>

using namespace object;
using namespace valueHelpers;

string valueHelpers::toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::rt_obj*>> stack){
    switch(getType(x)){
        case ValueType::NUMBER:
            if(isInt(x)) return std::to_string(static_cast<int64_t>(round(decodeNumber(x))));
            return std::to_string(decodeNumber(x));
        case ValueType::BOOL:
            return (decodeBool(x)) ? "true" : "false";
        case ValueType::NIL:
            return "null";
        case ValueType::OBJ:
            auto ptr = decodeObj(x);
            if (!stack) stack = std::make_shared<ankerl::unordered_dense::set<object::rt_obj*>>();
            if (stack->contains(ptr)) return fmt::format("[Circular ref {:#08x}]", reinterpret_cast<uint64_t>(ptr));
            stack->insert(ptr);
            string str = ptr->to_str(stack);
            stack->erase(ptr);
            return str;
    }
    std::cout << "Error printing object.\n";
    return "";
}

void valueHelpers::print(Value x) {
    std::cout << valueHelpers::toString(x);
}

string valueHelpers::typeToStr(Value x) {
    switch (getType(x)) {
        case ValueType::NUMBER: return "<number>";
        case ValueType::BOOL: return "<bool>";
        case ValueType::NIL: return "<null>";
        case ValueType::OBJ:
            auto ptr = decodeObj(x);
            switch (ptr->type()) {
                case rt_type::ARRAY: return "<array>";
                case rt_type::CLOSURE: return "<function>";
                case rt_type::INSTANCE: return "<instance: " + string(asInstance(x)->get_class()->name) + ">";
                case rt_type::STRING: return "<string>";
                case rt_type::HASH_MAP: return "<hash map>";
                case rt_type::FILE: return "<file>";
                case rt_type::MUTEX: return "<mutex>";
            }
    }
    return "error, couldn't determine type of value";
}