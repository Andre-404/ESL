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
#define MASK_SIGNATURE   0xffff000000000000
#define MASK_PAYLOAD_OBJ 0x0000ffffffffffff
// 51st bit is set to 1 to avoid Intels “QNaN Floating-Point Indefinite”
#define MASK_QNAN        0x7ffc000000000000

// Types
#define MASK_TYPE_FALSE 0x0000000000000000
#define MASK_TYPE_TRUE  0x0001000000000000
#define MASK_TYPE_NIL   0x0002000000000000
#define MASK_TYPE_OBJ   0x0003000000000000
#define MASK_TYPE_ERR   0x8000000000000000


// Signatures
#define MASK_SIGNATURE_FALSE (MASK_QNAN | MASK_TYPE_FALSE)
#define MASK_SIGNATURE_TRUE (MASK_QNAN | MASK_TYPE_TRUE)
#define MASK_SIGNATURE_NIL (MASK_QNAN | MASK_TYPE_NIL)
#define MASK_SIGNATURE_OBJ (MASK_QNAN | MASK_TYPE_OBJ)
#define MASK_SIGNATURE_ERR (MASK_QNAN | MASK_TYPE_ERR)

namespace valueHelpers {
    string toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::Obj *>> stack = nullptr);

    void print(Value x);

    string typeToStr(Value x);
}
