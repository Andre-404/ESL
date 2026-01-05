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

    tyPtr getBasicType(const TypeFlag type);

}
