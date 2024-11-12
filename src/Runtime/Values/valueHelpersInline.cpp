#pragma once
#include "../Objects/objects.h"
#include "valueHelpers.h"

using namespace object;

// Things with values (NaN boxing)
static ValueType getType(Value x){
    if (((x) & MASK_QNAN) != MASK_QNAN) return ValueType::NUMBER;
    switch (x & MASK_SIGNATURE){
        case MASK_SIGNATURE_FALSE:
        case MASK_SIGNATURE_TRUE: return ValueType::BOOL;
        case MASK_SIGNATURE_OBJ: return ValueType::OBJ;
    }
    return ValueType::NIL;
}

inline Value encodeNumber(double x){ return *reinterpret_cast<Value*>(&x); }
inline Value encodeBool(bool x){ return MASK_QNAN | (MASK_SIGNATURE_TRUE*x); }
inline Value encodeObj(object::Obj* x){ return MASK_SIGNATURE_OBJ | reinterpret_cast<Value>(x) | x->type; }
inline Value encodeNil(){ return MASK_SIGNATURE_NIL; }

inline double decodeNumber(Value x){ return *reinterpret_cast<double*>(&x); }
inline int64_t decodeInt(Value x){ return std::round(decodeNumber(x)); }
inline bool decodeBool(Value x){ return x == MASK_SIGNATURE_TRUE; }
inline object::Obj* decodeObj(Value x){ return reinterpret_cast<object::Obj*>(x & MASK_PAYLOAD_OBJ); }

inline bool isNumber(Value x){ return (x & MASK_QNAN) != MASK_QNAN; }
inline bool isBool(Value x){ return (x == MASK_SIGNATURE_TRUE || x == MASK_SIGNATURE_FALSE); }
inline bool isNil(Value x){ return x == MASK_SIGNATURE_NIL; }
inline bool isObj(Value x){ return (x & MASK_SIGNATURE) == MASK_SIGNATURE_OBJ; }

inline bool isInt(Value x) { return isNumber(x) && FLOAT_EQ(decodeNumber(x), std::round(decodeNumber(x))); }

inline bool isString(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::STRING; }
inline bool isArray(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::ARRAY; }
inline bool isClosure(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::CLOSURE; }
inline bool isClass(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::CLASS; }
inline bool isInstance(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::INSTANCE; }
inline bool isHashMap(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::HASH_MAP; }
inline bool isUpvalue(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::FREEVAR; }
inline bool isFile(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::FILE; }
inline bool isMutex(Value x) { return isObj(x) && (x&MASK_PAYLOAD_TYPE) == +ObjType::MUTEX; }

inline bool isFalsey(Value x) { return (isBool(x) && !decodeBool(x)) || isNil(x); }

// Uses reinterpret_cast because it assumes the object being passed is of the requested type
inline object::ObjString* asString(Value x) { return reinterpret_cast<ObjString*>(decodeObj(x)); }
inline object::ObjArray* asArray(Value x) { return reinterpret_cast<ObjArray*>(decodeObj(x)); }
inline object::ObjClosure* asClosure(Value x) { return reinterpret_cast<ObjClosure*>(decodeObj(x)); }
inline object::ObjClass* asClass(Value x) { return reinterpret_cast<ObjClass*>(decodeObj(x)); }
inline object::ObjInstance* asInstance(Value x) { return reinterpret_cast<ObjInstance*>(decodeObj(x)); }
inline object::ObjHashMap* asHashMap(Value x) { return reinterpret_cast<ObjHashMap*>(decodeObj(x)); }
inline object::ObjFreevar* asUpvalue(Value x) { return reinterpret_cast<ObjFreevar*>(decodeObj(x)); }
inline object::ObjFile* asFile(Value x) { return reinterpret_cast<ObjFile*>(decodeObj(x)); }
inline object::ObjMutex* asMutex(Value x) { return reinterpret_cast<ObjMutex*>(decodeObj(x)); }

inline bool equals(Value x, Value y){
    ValueType type = getType(x);
    if (type != getType(y)) return false;
    if (type == ValueType::NUMBER){
        return FLOAT_EQ(decodeNumber(x), decodeNumber(y));
    }
    return x == y;
}