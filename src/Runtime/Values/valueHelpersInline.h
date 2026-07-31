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
inline Value encodeObj(object::rt_obj* x){ return mask_signature_obj | reinterpret_cast<Value>(x) | +x->type(); }
inline Value encodeNil(){ return mask_signature_null; }

inline double decodeNumber(Value x){ return *reinterpret_cast<double*>(&x); }
inline int64_t decodeInt(Value x){ return std::round(decodeNumber(x)); }
inline bool decodeBool(Value x){ return x == mask_signature_true; }
inline object::rt_obj* decodeObj(Value x){ return reinterpret_cast<object::rt_obj*>(x & mask_payload_obj); }

inline bool isNumber(Value x){ return (x & mask_qnan) != mask_qnan; }
inline bool isBool(Value x){ return (x & mask_signature) == mask_signature_bool; }
inline bool isNil(Value x){ return x == mask_signature_null; }
inline bool isObj(Value x){ return (x & mask_signature) == mask_signature_obj; }

inline bool isInt(Value x) { return isNumber(x) && FLOAT_EQ(decodeNumber(x), std::round(decodeNumber(x))); }

inline bool isString(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::STRING; }
inline bool isArray(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::ARRAY; }
inline bool isClosure(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::CLOSURE; }
inline bool isInstance(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::INSTANCE; }
inline bool isHashMap(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::HASH_MAP; }
inline bool isFile(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::FILE; }
inline bool isMutex(Value x) { return isObj(x) && (x&mask_payload_type) == +rt_type::MUTEX; }

inline bool isFalsey(Value x) { return (isBool(x) && !decodeBool(x)) || isNil(x); }

// Uses reinterpret_cast because it assumes the object being passed is of the requested type
inline rt_string* asString(Value x) { return reinterpret_cast<rt_string*>(decodeObj(x)); }
inline rt_arr* asArray(Value x) { return reinterpret_cast<rt_arr*>(decodeObj(x)); }
inline rt_closure* asClosure(Value x) { return reinterpret_cast<rt_closure*>(decodeObj(x)); }
inline rt_inst* asInstance(Value x) { return reinterpret_cast<rt_inst*>(decodeObj(x)); }
inline rt_hashmap* asHashMap(Value x) { return reinterpret_cast<rt_hashmap*>(decodeObj(x)); }

inline bool equals(Value x, Value y){
    ValueType type = getType(x);
    if (type != getType(y)) return false;
    if (type == ValueType::NUMBER){
        return FLOAT_EQ(decodeNumber(x), decodeNumber(y));
    }
    return x == y;
}