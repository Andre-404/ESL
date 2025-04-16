#pragma once
#include "../../Includes/unorderedDense.h"
#include "../Objects/objects.h"

namespace object {
	class Obj;

	class ObjString;

	class ObjArray;

	class ObjFreevar;

	class ObjClosure;

	class ObjClass;

	class ObjInstance;

    class ObjHashMap;

	class ObjFile;

	class ObjMutex;
}

enum class ValueType {
	NUMBER,
	BOOL,
	NIL,
	OBJ, // This is a pointer
};
inline constexpr unsigned operator+ (ValueType const val) { return static_cast<byte>(val); }

// Masks for important segments of a float value
constexpr uint64_t mask_signature =     0xffff000000000000ull;
// Objects are 16 byte aligned
constexpr uint64_t mask_payload_obj =   0x0000fffffffffff0ull;
constexpr uint64_t mask_payload_type =  0x000000000000000full;
// 51st bit is set to 1 to avoid Intels “QNaN Floating-Point Indefinite”
constexpr uint64_t mask_qnan =          0x7ffc000000000000ull;

// Types
constexpr uint64_t mask_type_bool = 0x0000000000000000ull;
constexpr uint64_t mask_type_true = 0x0000000000000001ull;
constexpr uint64_t mask_type_null = 0x0001000000000000ull;
constexpr uint64_t mask_type_obj  = 0x0002000000000000ull;

// Signatures
constexpr uint64_t mask_signature_bool = mask_qnan | mask_type_bool;
constexpr uint64_t mask_signature_false = mask_signature_bool;
constexpr uint64_t mask_signature_true = mask_signature_bool | mask_type_true;
constexpr uint64_t mask_signature_null = mask_qnan | mask_type_null;
constexpr uint64_t mask_signature_obj = mask_qnan | mask_type_obj;

namespace valueHelpers {
    string toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::Obj *>> stack = nullptr);

    void print(Value x);

    string typeToStr(Value x);
}
