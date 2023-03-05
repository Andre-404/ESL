#include "../Objects/objects.h"
#include "codegenDefs.h"

using namespace object;

// Masks for important segments of a float value
#define MASK_SIGN        0x8000000000000000
#define MASK_EXPONENT    0x7ff0000000000000
#define MASK_QUIET       0x0008000000000000
#define MASK_TYPE        0x0007000000000000
#define MASK_SIGNATURE   0xffff000000000000
#define MASK_FULL        0xffffffffffffffff
#define MASK_NAN         0x7ff0000000000000
#define MASK_PAYLOAD_OBJ 0x0000ffffffffffff

// Types
#define MASK_TYPE_NAN   0x0000000000000000
#define MASK_TYPE_FALSE 0x0001000000000000
#define MASK_TYPE_TRUE  0x0002000000000000
#define MASK_TYPE_NIL   0x0003000000000000
#define MASK_TYPE_OBJ   MASK_SIGN


// Signatures
#define MASK_SIGNATURE_NAN MASK_NAN
#define MASK_SIGNATURE_FALSE (MASK_NAN | MASK_TYPE_FALSE)
#define MASK_SIGNATURE_TRUE (MASK_NAN | MASK_TYPE_TRUE)
#define MASK_SIGNATURE_NIL (MASK_NAN | MASK_TYPE_NIL)
#define MASK_SIGNATURE_OBJ (MASK_SIGN | MASK_NAN | MASK_TYPE_OBJ)

// Things with values (NaN boxing)
static ValueType getType(Value x){
    if (((~x) & MASK_EXPONENT) != 0) return ValueType::NUMBER;
    switch (x & MASK_SIGNATURE){
        case MASK_SIGNATURE_NAN: return ValueType::NUMBER;
        case MASK_SIGNATURE_FALSE:
        case MASK_SIGNATURE_TRUE: return ValueType::BOOL;
        case MASK_SIGNATURE_OBJ: return ValueType::OBJ;
    }
    return ValueType::NIL;
}

inline Value encodeNumber(double x){ return *reinterpret_cast<Value*>(&x); }
inline Value encodeBool(bool x){ return (x) ? MASK_SIGNATURE_TRUE : MASK_SIGNATURE_FALSE; }
inline Value encodeObj(object::Obj* x){ return MASK_SIGNATURE_OBJ | reinterpret_cast<Value>(x); }
inline Value encodeNil(){ return MASK_SIGNATURE_NIL; }

inline double decodeNumber(Value x){ return *reinterpret_cast<double*>(&x); }
inline int32_t decodeInt(Value x){ return std::round(decodeNumber(x)); }
inline bool decodeBool(Value x){ return x & MASK_TYPE_TRUE; }
inline object::Obj* decodeObj(Value x){ return reinterpret_cast<object::Obj*>(x & MASK_PAYLOAD_OBJ); }

inline bool isNumber(Value x){ return ((~x) & MASK_EXPONENT) != 0  || (x & MASK_SIGNATURE) == MASK_SIGNATURE_NAN; }
inline bool isBool(Value x){ return ((x & MASK_SIGNATURE) == MASK_SIGNATURE_TRUE || (x & MASK_SIGNATURE) == MASK_SIGNATURE_FALSE); }
inline bool isNil(Value x){ return (x & MASK_SIGNATURE) == MASK_SIGNATURE_NIL; }
inline bool isObj(Value x){ return (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ; }

inline bool isInt(Value x) { return isNumber(x) && FLOAT_EQ(decodeNumber(x), std::round(decodeNumber(x))); }

inline bool isString(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::STRING; }
inline bool isFunction(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::FUNC; }
inline bool isNativeFn(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::NATIVE; }
inline bool isArray(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::ARRAY; }
inline bool isClosure(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::CLOSURE; }
inline bool isClass(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::CLASS; }
inline bool isInstance(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::INSTANCE; }
inline bool isHashMap(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::HASH_MAP; }
inline bool isBoundMethod(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::BOUND_METHOD; }
inline bool isUpvalue(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::UPVALUE; }
inline bool isFile(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::FILE; }
inline bool isMutex(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::MUTEX; }
inline bool isFuture(Value x) { return isObj(x) && decodeObj(x)->type == ObjType::FUTURE; }

inline bool isFalsey(Value x) { return (isBool(x) && !decodeBool(x)) || isNil(x); }

// Uses reinterpret_cast because it assumes the object being passed is of the requested type
inline object::ObjString* asString(Value x) { return reinterpret_cast<ObjString*>(decodeObj(x)); }
inline object::ObjFunc* asFunction(Value x) { return reinterpret_cast<ObjFunc*>(decodeObj(x)); }
inline object::ObjNativeFunc* asNativeFn(Value x) { return reinterpret_cast<ObjNativeFunc*>(decodeObj(x)); }
inline object::ObjArray* asArray(Value x) { return reinterpret_cast<ObjArray*>(decodeObj(x)); }
inline object::ObjClosure* asClosure(Value x) { return reinterpret_cast<ObjClosure*>(decodeObj(x)); }
inline object::ObjClass* asClass(Value x) { return reinterpret_cast<ObjClass*>(decodeObj(x)); }
inline object::ObjInstance* asInstance(Value x) { return reinterpret_cast<ObjInstance*>(decodeObj(x)); }
inline object::ObjHashMap* asHashMap(Value x) { return reinterpret_cast<ObjHashMap*>(decodeObj(x)); }
inline object::ObjBoundMethod* asBoundMethod(Value x) { return reinterpret_cast<ObjBoundMethod*>(decodeObj(x)); }
inline object::ObjUpval* asUpvalue(Value x) { return reinterpret_cast<ObjUpval*>(decodeObj(x)); }
inline object::ObjFile* asFile(Value x) { return reinterpret_cast<ObjFile*>(decodeObj(x)); }
inline object::ObjMutex* asMutex(Value x) { return reinterpret_cast<ObjMutex*>(decodeObj(x)); }
inline object::ObjFuture* asFuture(Value x) { return reinterpret_cast<ObjFuture*>(decodeObj(x)); }

inline bool equals(Value x, Value y){
    ValueType type = getType(x);
    if (type != getType(y)) return false;
    if (type == ValueType::NUMBER){
        return FLOAT_EQ(decodeNumber(x), decodeNumber(y));
    }
    return x == y;
}