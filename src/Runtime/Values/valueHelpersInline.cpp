#pragma once
#include "../Objects/objects.h"
#include "valueHelpers.h"

using namespace object;

// Things with values (NaN boxing)
static ValueType getType(Value x){
    if (((x) & mask_qnan) != mask_qnan) return ValueType::NUMBER;
    switch (x & mask_signature){
        case mask_signature_false:
        case mask_signature_true: return ValueType::BOOL;
        case mask_signature_obj: return ValueType::OBJ;
    }
    return ValueType::NIL;
}

inline Value encodeNumber(double x){ return *reinterpret_cast<Value*>(&x); }
inline Value encodeBool(bool x){ return mask_signature_bool | x; }
inline Value encodeObj(object::Obj* x){ return mask_signature_obj | reinterpret_cast<Value>(x) | x->type; }
inline Value encodeNil(){ return mask_signature_null; }

inline double decodeNumber(Value x){ return *reinterpret_cast<double*>(&x); }
inline int64_t decodeInt(Value x){ return std::round(decodeNumber(x)); }
inline bool decodeBool(Value x){ return x == mask_signature_true; }
inline object::Obj* decodeObj(Value x){ return reinterpret_cast<object::Obj*>(x & mask_payload_obj); }

inline bool isNumber(Value x){ return (x & mask_qnan) != mask_qnan; }
inline bool isBool(Value x){ return (x & mask_signature) == mask_signature_bool; }
inline bool isNil(Value x){ return x == mask_signature_null; }
inline bool isObj(Value x){ return (x & mask_signature) == mask_signature_obj; }

inline bool isInt(Value x) { return isNumber(x) && FLOAT_EQ(decodeNumber(x), std::round(decodeNumber(x))); }

inline bool isString(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::STRING; }
inline bool isArray(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::ARRAY; }
inline bool isClosure(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::CLOSURE; }
inline bool isClass(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::CLASS; }
inline bool isInstance(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::INSTANCE; }
inline bool isHashMap(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::HASH_MAP; }
inline bool isUpvalue(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::FREEVAR; }
inline bool isFile(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::FILE; }
inline bool isMutex(Value x) { return isObj(x) && (x&mask_payload_type) == +ObjType::MUTEX; }

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