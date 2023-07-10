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
    using tyPtr = std::shared_ptr<Type>;

    // Union of multiple types, used to represent types of variables, return values from functions, etc.
    class TypeUnion : public Type{
    public:
        std::unordered_set<tyPtr> types;
        bool collapsed;
        TypeUnion(){
            collapsed = false;
            type = TypeFlag::UNION;
        }
    };

    // Defers obtaining the actual type(fn return, future return, instance field type) until type collapsing
    // After collapsing store all possible types into collapsedTypes
    class CallReturnDeferred : public Type{
    public:
        tyPtr calleeType;
        bool collapsed;
        std::unordered_set<tyPtr> collapsedTypes;

        CallReturnDeferred(tyPtr _calleeType){
            calleeType = _calleeType;
            collapsed = false;
            type = TypeFlag::CALL_RETURN_DEFER;
        }
    };

    class FutureAwaitDeferred : public Type{
    public:
        tyPtr exprType;
        bool collapsed;
        std::unordered_set<tyPtr> collapsedTypes;

        FutureAwaitDeferred(tyPtr _exprType){
            exprType = _exprType;
            collapsed = false;
            type = TypeFlag::FUT_AWAIT_DEFER;
        }
    };

    class InstanceGetDeferred : public Type{
    public:
        tyPtr instance;
        string field;
        bool collapsed;
        std::unordered_set<tyPtr> collapsedTypes;

        InstanceGetDeferred(tyPtr _inst, string _field){
            instance = _inst;
            field = _field;
            collapsed = false;
            type = TypeFlag::INSTANCE_GET_DEFER;
        }
    };

    class ArrayType : public Type{
    public:
        tyPtr itemType;
        ArrayType(tyPtr _itemType){
            itemType = _itemType;
            type = TypeFlag::ARRAY;
        }
    };

    class FunctionType : public Type{
    public:
        int argCount;
        std::shared_ptr<TypeUnion> retType; //Possible return types
        bool isClosure;

        FunctionType(int _argCount, std::shared_ptr<TypeUnion> _retType, bool _isClosure){
            argCount = _argCount;
            retType = _retType;
            isClosure = _isClosure;
            type = TypeFlag::FUNCTION;
        }
    };

    class HashMapType : public Type{
    public:
        tyPtr itemType;
        HashMapType(tyPtr _itemType){
            itemType = _itemType;
            type = TypeFlag::HASHMAP;
        }
    };

    class ClassType;

    class InstanceType : public Type{
    public:
        std::shared_ptr<ClassType> klass;
        InstanceType(std::shared_ptr<ClassType> _klass){
            klass = _klass;
            type = TypeFlag::INSTANCE;
        }
    };

    class ClassType : public Type{
    public:
        // Privates are prefixed with "priv."
        // In the pair first value is type of field/method, second is index of that field/method after linearization
        // For fields index is into array in ObjInstance, and methods index is index into methods array of ObjClass
        std::unordered_map<string, std::pair<tyPtr, int>> fields;
        std::unordered_map<string, std::pair<std::shared_ptr<FunctionType>, int>> methods;

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
        std::shared_ptr<CallReturnDeferred> heldTypes;//Possible held types

        FutureType(std::shared_ptr<CallReturnDeferred> _heldTypes){
            heldTypes = _heldTypes;
            type = TypeFlag::FUTURE;
        }
    };

    tyPtr getBasicType(TypeFlag type);

    // Adds rhs to lhs if rhs is not already in lhs
    void typeInflow(std::shared_ptr<TypeUnion> lhs, tyPtr rhs);

    void inheritClass(std::shared_ptr<ClassType> child, std::shared_ptr<ClassType> parent);

    vector<tyPtr> collapse(tyPtr toCollapse);
}
