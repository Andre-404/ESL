#pragma once
#include "../common.h"
#include <memory>

namespace types{
    enum class TypeFlag{
        // Simple types, represented with only a flag
        NIL,
        BOOL,
        NUMBER,
        STRING,
        MUTEX,
        FILE,
        // Base type
        ANY,
        // Complex types, represented by their own struct
        ARRAY,
        FUNCTION,
        HASHMAP,
        INSTANCE,
        CLASS,
        UNKNOWN // Special type that's ignored when doing a type union
    };

    class Type{
    public:
        TypeFlag type;
        Type(){
            type = TypeFlag::ANY;
        }
        Type(TypeFlag _ty){
            type = _ty;
        }
    };
    using tyPtr = std::shared_ptr<Type>;

    class ArrayType : public Type{
    public:
        tyPtr itemType;
        ArrayType(const tyPtr _itemType){
            itemType = _itemType;
            type = TypeFlag::ARRAY;
        }
    };

    class FunctionType : public Type{
    public:
        int argCount;
        tyPtr retType; //Possible return types
        vector<tyPtr> paramTypes;
        bool isClosure;

        FunctionType(const int _argCount, const tyPtr _retType, const bool _isClosure){
            argCount = _argCount;
            retType = _retType;
            isClosure = _isClosure;
            type = TypeFlag::FUNCTION;
        }
    };

    class HashMapType : public Type{
    public:
        tyPtr itemType;
        HashMapType(const tyPtr _itemType){
            itemType = _itemType;
            type = TypeFlag::HASHMAP;
        }
    };

    class ClassType : public Type{
    public:
        // Privates are prefixed with "priv."
        std::unordered_map<string, std::pair<tyPtr, uInt64>> fields;
        std::unordered_map<string, std::pair<tyPtr, uInt64>> methods;
        string name;

        // Fields and methods get filled up from the outside
        ClassType(){
            type = TypeFlag::CLASS;
        }

        void inherit(const std::shared_ptr<ClassType> parent){
            methods = parent->methods;
            fields = parent->fields;
        }
    };

    class InstanceType : public Type{
    public:
        std::shared_ptr<ClassType> klass;
        InstanceType(const std::shared_ptr<ClassType> _klass){
            klass = _klass;
            type = TypeFlag::INSTANCE;
        }
    };

    tyPtr getBasicType(TypeFlag type);
    inline bool typeFlagMatch(const tyPtr ty, const TypeFlag type) {
        return ty->type == type;
    }

    inline bool types_equal(const tyPtr &left, const tyPtr &right) {
        if (left->type != right->type) return false;
        switch (left->type) {
            case TypeFlag::ANY: return false;
            case TypeFlag::ARRAY: return types_equal(((ArrayType*)left.get())->itemType, ((ArrayType*)right.get())->itemType);
            case TypeFlag::FUNCTION: return left == right;
            case TypeFlag::HASHMAP:
                return types_equal(((HashMapType*)left.get())->itemType, ((HashMapType*)right.get())->itemType);
            case TypeFlag::INSTANCE:
                return types_equal(((InstanceType*)left.get())->klass, ((InstanceType*)right.get())->klass);
            case TypeFlag::CLASS: return left == right;
            default: return true;
        }
    }

}
