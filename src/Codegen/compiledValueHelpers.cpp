#include "../Objects/objects.h"
#include "codegenDefs.h"

using namespace object;

namespace compiledValueHelpers{

    struct BoxedValue {
        ValueType type;
        union {
            double num;
            bool bl;
            Obj *ptr;
        } as;

        BoxedValue(double dbl) {
            type = ValueType::NUMBER;
            as.num = dbl;
        }

        BoxedValue(bool bl) {
            type = ValueType::BOOL;
            as.bl = bl;
        }

        BoxedValue(Obj *ptr) {
            type = ValueType::OBJ;
            as.ptr = ptr;
        }

        BoxedValue() {
            type = ValueType::NIL;
            as.ptr = nullptr;
        }
    };


    inline BoxedValue encodeNumber(double x) { return BoxedValue(x); }
    inline BoxedValue encodeBool(bool x) { return BoxedValue(x); }
    inline BoxedValue encodeObj(object::Obj *x) { return BoxedValue(x); }
    inline BoxedValue encodeNil() { return BoxedValue(); }
    inline double decodeNumber(BoxedValue x) { return x.as.num; }
    inline int32_t decodeInt(BoxedValue x) { return std::round(decodeNumber(x)); }
    inline bool decodeBool(BoxedValue x) { return x.as.bl; }
    inline object::Obj *decodeObj(BoxedValue x) { return x.as.ptr; }

    inline bool isNumber(BoxedValue x) {
        return x.type == ValueType::NUMBER;
    }
    inline bool isBool(BoxedValue x) {
        return x.type == ValueType::BOOL;
    }
    inline bool isNil(BoxedValue x) { return x.type == ValueType::NIL; }
    inline bool isObj(BoxedValue x) { return x.type == ValueType::OBJ; }
    inline bool isInt(BoxedValue x) { return isNumber(x) && FLOAT_EQ(decodeNumber(x), std::round(decodeNumber(x))); }
    inline bool isString(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::STRING; }
    inline bool isFunction(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::FUNC; }
    inline bool isNativeFn(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::NATIVE; }
    inline bool isArray(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::ARRAY; }
    inline bool isClosure(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::CLOSURE; }
    inline bool isClass(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::CLASS; }
    inline bool isInstance(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::INSTANCE; }
    inline bool isHashMap(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::HASH_MAP; }
    inline bool isBoundMethod(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::BOUND_METHOD; }
    inline bool isUpvalue(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::UPVALUE; }
    inline bool isFile(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::FILE; }
    inline bool isMutex(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::MUTEX; }
    inline bool isFuture(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::FUTURE; }
    inline bool isRange(BoxedValue x) { return isObj(x) && decodeObj(x)->type == ObjType::RANGE; }
    inline bool isFalsey(BoxedValue x) { return (isBool(x) && !decodeBool(x)) || isNil(x); }

    // Uses reinterpret_cast because it assumes the object being passed is of the requested type
    inline object::ObjString *asString(BoxedValue x) { return reinterpret_cast<ObjString *>(decodeObj(x)); }
    inline object::ObjFunc *asFunction(BoxedValue x) { return reinterpret_cast<ObjFunc *>(decodeObj(x)); }
    inline object::ObjNativeFunc *asNativeFn(BoxedValue x) { return reinterpret_cast<ObjNativeFunc *>(decodeObj(x)); }
    inline object::ObjArray *asArray(BoxedValue x) { return reinterpret_cast<ObjArray *>(decodeObj(x)); }
    inline object::ObjClosure *asClosure(BoxedValue x) { return reinterpret_cast<ObjClosure *>(decodeObj(x)); }
    inline object::ObjClass *asClass(BoxedValue x) { return reinterpret_cast<ObjClass *>(decodeObj(x)); }
    inline object::ObjInstance *asInstance(BoxedValue x) { return reinterpret_cast<ObjInstance *>(decodeObj(x)); }
    inline object::ObjHashMap *asHashMap(BoxedValue x) { return reinterpret_cast<ObjHashMap *>(decodeObj(x)); }
    inline object::ObjBoundMethod *asBoundMethod(BoxedValue x) { return reinterpret_cast<ObjBoundMethod *>(decodeObj(x)); }
    inline object::ObjUpval *asUpvalue(BoxedValue x) { return reinterpret_cast<ObjUpval *>(decodeObj(x)); }
    inline object::ObjFile *asFile(BoxedValue x) { return reinterpret_cast<ObjFile *>(decodeObj(x)); }
    inline object::ObjMutex *asMutex(BoxedValue x) { return reinterpret_cast<ObjMutex *>(decodeObj(x)); }
    inline object::ObjFuture *asFuture(BoxedValue x) { return reinterpret_cast<ObjFuture *>(decodeObj(x)); }
    inline object::ObjRange *asRange(BoxedValue x) { return reinterpret_cast<ObjRange *>(decodeObj(x)); }

    inline bool equals(BoxedValue x, BoxedValue y) {
        if (x.type != y.type) return false;
        if (x.type == ValueType::NUMBER) {
            return FLOAT_EQ(decodeNumber(x), decodeNumber(y));
        }
        return x.as.ptr == y.as.ptr;
    }

}