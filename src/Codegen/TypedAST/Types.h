#pragma once
#include "../../common.h"
#include <memory>
#include <unordered_set>

namespace types{
    enum class TypeFlag{
        // Simple types, represented with only a flag
        NIL,
        BOOL,
        NUMBER,
        STRING,
        MUTEX,
        RANGE,
        FILE,
        // Base type
        ANY,
        // Complex types, represented by their own struct
        ARRAY,
        FUNCTION,
        HASHMAP,
        INSTANCE,
        CLASS,
        FUTURE,
        // Union of many types
        UNION,
        // Temporaries that get collapsed into a vector of types
        CALL_RETURN_DEFER,
        FUT_AWAIT_DEFER,
        INSTANCE_GET_DEFER
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
    using tyVarIdx = int;
    using tyPtr = std::shared_ptr<Type>;

    // Union of multiple types, used to represent types of variables, return values from functions, etc.
    class TypeUnion : public Type{
    public:
        std::unordered_set<tyVarIdx> types;
        TypeUnion(){
            type = TypeFlag::UNION;
        }
    };

    // Defers obtaining the actual type(fn return, future return, instance field type) until type collapsing
    // After collapsing store all possible types into collapsedReturnTypes
    class CallReturnDeferred : public Type{
    public:
        tyVarIdx calleeType;
        vector<tyVarIdx> argTypes;

        CallReturnDeferred(tyVarIdx _calleeType, vector<tyVarIdx> _args){
            calleeType = _calleeType;
            argTypes = _args;
            type = TypeFlag::CALL_RETURN_DEFER;
        }
    };

    class FutureAwaitDeferred : public Type{
    public:
        tyVarIdx exprType;

        FutureAwaitDeferred(tyVarIdx _exprType){
            exprType = _exprType;
            type = TypeFlag::FUT_AWAIT_DEFER;
        }
    };

    class InstanceGetDeferred : public Type{
    public:
        tyVarIdx instance;
        string field;

        InstanceGetDeferred(tyVarIdx _inst, string _field){
            instance = _inst;
            field = _field;
            type = TypeFlag::INSTANCE_GET_DEFER;
        }
    };

    class ArrayType : public Type{
    public:
        tyVarIdx itemType;
        ArrayType(tyVarIdx _itemType){
            itemType = _itemType;
            type = TypeFlag::ARRAY;
        }
    };

    class FunctionType : public Type{
    public:
        int argCount;
        tyVarIdx retType; //Possible return types
        vector<tyVarIdx> paramTypes;
        bool isClosure;

        FunctionType(int _argCount, tyVarIdx _retType, bool _isClosure){
            argCount = _argCount;
            retType = _retType;
            isClosure = _isClosure;
            type = TypeFlag::FUNCTION;
        }
    };

    class HashMapType : public Type{
    public:
        tyVarIdx itemType;
        HashMapType(tyVarIdx _itemType){
            itemType = _itemType;
            type = TypeFlag::HASHMAP;
        }
    };

    class ClassType;

    class InstanceType : public Type{
    public:
        tyVarIdx klass;
        InstanceType(tyVarIdx _klass){
            klass = _klass;
            type = TypeFlag::INSTANCE;
        }
    };

    class ClassType : public Type{
    public:
        // Privates are prefixed with "priv."
        std::unordered_map<string, tyVarIdx> fields;
        std::unordered_map<string, tyVarIdx> methods;

        // Fields and methods get filled up from the outside
        ClassType(){
            type = TypeFlag::CLASS;
        }

        void inherit(std::shared_ptr<ClassType> parent){
            methods = parent->methods;
            fields = parent->fields;
        }
    };

    class FutureType : public Type{
    public:
        tyVarIdx calleeType;
        vector<tyVarIdx> argTypes;

        FutureType(tyVarIdx _calleeType, vector<tyVarIdx> _args){
            calleeType = _calleeType;
            argTypes = _args;
            type = TypeFlag::FUTURE;
        }
    };

    tyPtr getBasicType(TypeFlag type);

    // Adds rhs to lhs if rhs is not already in lhs
    void typeInflow(std::shared_ptr<TypeUnion> lhs, tyVarIdx rhs);
}
