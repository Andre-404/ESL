#include "valueHelpers.h"
#include "../../Includes/fmt/core.h"
#include "valueHelpersInline.h"
#include <iostream>

using namespace object;
using namespace valueHelpers;

string valueHelpers::toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::rt_obj*>> stack){
    switch(getType(x)){
        case ValueType::NUMBER:
            if(isInt(x)) {
                // int64_t can't hold integral doubles from 2^63 up, fmt prints those (slower)
                // + 0.0 makes -0 print as 0
                double r = round(decodeNumber(x)) + 0.0;
                return std::abs(r) < 9223372036854775808.0 ? std::to_string(int64_t(r)) : fmt::format("{:.0f}", r);
            }
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
                case rt_type::FILE_HANDLE: return "<file>";
                case rt_type::MUTEX: return "<mutex>";
            }
    }
    return "error, couldn't determine type of value";
}