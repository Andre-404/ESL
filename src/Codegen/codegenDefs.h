#pragma once
#include <variant>
#include "../moduleDefs.h"
#include "../Includes/unorderedDense.h"
#include "../Objects/objects.h"

namespace object {
	class Obj;

	class ObjString;

	class ObjArray;

	class ObjFunc;

	class ObjFreevar;

	class ObjClosure;

	class ObjClass;

	class ObjBoundMethod;

	class ObjInstance;

    class ObjHashMap;

	class ObjFile;

	class ObjMutex;

	class ObjFuture;
}

enum class ValueType {
	NUMBER,
	BOOL,
	NIL,
	OBJ, // This is a pointer
};
inline constexpr unsigned operator+ (ValueType const val) { return static_cast<byte>(val); }

namespace valueHelpers {
    string toString(Value x, std::shared_ptr<ankerl::unordered_dense::set<object::Obj *>> stack = nullptr);

    void print(Value x);

    void mark(Value x);

    string typeToStr(Value x);
}
