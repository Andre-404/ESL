#pragma once
#include "../common.h"
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
        FUTURE
    };

    enum class TypeConstraintFlag{
        ADD_TY,
        GET_RETURN_TY,
        GET_AWAIT_TY,
        INST_GET_FIELD_TY,
        COMPUTE_ADD_TYS,
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

    class TypeConstraint{
    public:
        TypeConstraintFlag type;
    };

    class AddTyConstraint : public TypeConstraint{
    public:
        tyVarIdx toAdd;
        AddTyConstraint(const tyVarIdx _toAdd){
            toAdd = _toAdd;
            type = TypeConstraintFlag::ADD_TY;
        }
    };

    class CallResTyConstraint : public TypeConstraint{
    public:
        tyVarIdx calleeType;

        CallResTyConstraint(const tyVarIdx _calleeType){
            calleeType = _calleeType;
            type = TypeConstraintFlag::GET_RETURN_TY;
        }
    };

    class AwaitTyConstraint : public TypeConstraint{
    public:
        tyVarIdx potentialFuture;

        AwaitTyConstraint(const tyVarIdx _potentialFuture){
            potentialFuture = _potentialFuture;
            type = TypeConstraintFlag::GET_AWAIT_TY;
        }
    };

    class InstGetFieldTyConstraint : public TypeConstraint{
    public:
        tyVarIdx potentialInst;
        string field;

        InstGetFieldTyConstraint(const tyVarIdx _potentialInst, const string _field){
            potentialInst = _potentialInst;
            field = _field;
            type = TypeConstraintFlag::INST_GET_FIELD_TY;
        }
    };

    class ComputeAddTysConstraint : public TypeConstraint{
    public:
        tyVarIdx lhs;
        tyVarIdx rhs;

        ComputeAddTysConstraint(const tyVarIdx _lhs, const tyVarIdx _rhs){
            lhs = _lhs;
            rhs = _rhs;
            type = TypeConstraintFlag::COMPUTE_ADD_TYS;
        }
    };

    class ArrayType : public Type{
    public:
        tyVarIdx itemType;
        ArrayType(const tyVarIdx _itemType){
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

        FunctionType(const int _argCount, const tyVarIdx _retType, const bool _isClosure){
            argCount = _argCount;
            retType = _retType;
            isClosure = _isClosure;
            type = TypeFlag::FUNCTION;
        }
    };

    class HashMapType : public Type{
    public:
        tyVarIdx itemType;
        HashMapType(const tyVarIdx _itemType){
            itemType = _itemType;
            type = TypeFlag::HASHMAP;
        }
    };

    class ClassType;

    class InstanceType : public Type{
    public:
        std::shared_ptr<ClassType> klass;
        InstanceType(const std::shared_ptr<ClassType> _klass){
            klass = _klass;
            type = TypeFlag::INSTANCE;
        }
    };

    class ClassType : public Type{
    public:
        // Privates are prefixed with "priv."
        std::unordered_map<string, std::pair<tyVarIdx, uInt64>> fields;
        std::unordered_map<string, std::pair<tyVarIdx, uInt64>> methods;
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

    class FutureType : public Type{
    public:
        tyVarIdx calleeType;

        FutureType(const tyVarIdx _calleeType){
            calleeType = _calleeType;
            type = TypeFlag::FUTURE;
        }
    };

    tyPtr getBasicType(const TypeFlag type);

}
