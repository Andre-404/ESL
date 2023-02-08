#include "nativeFunctions.h"
#include "thread.h"
#include <iostream>

#define NATIVE_FUNC(name, arity, func) vector.push_back(new object::ObjNativeFunc(func, arity, name))

vector<object::ObjNativeFunc*> runtime::createNativeFuncs(){
    vector<object::ObjNativeFunc*> vector;
    NATIVE_FUNC("nativePrint", 1, [](Thread* t, int argCount) {
        t->pop().print();
        std::cout << "\n";
        return true;
    });
    NATIVE_FUNC("add1", 1, [](Thread* t, int argCount) {
        auto temp = Value(t->pop().asNumber() + 1);
        t->pop();
        t->push(temp);
        return false;
    });
    return vector;
}
robin_hood::unordered_map<string, uInt> runtime::createNativeNameTable(vector<object::ObjNativeFunc *>& natives){
    robin_hood::unordered_map<string, uInt> map;
    for(int i = 0; i < natives.size(); i++){
        map.insert_or_assign(natives[i]->name, i);
    }
    return map;
}
#define BOUND_NATIVE(name, arity, func) classes.back().methods.insert_or_assign(name, new object::ObjNativeFunc(func, arity, name))
vector<runtime::BuiltinClass> runtime::createBuiltinClasses(){
    vector<runtime::BuiltinClass> classes;
    classes.emplace_back("number");
    BOUND_NATIVE("add1", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().asNumber() + 1));
        return false;
    });
    classes.emplace_back("string");
    classes.emplace_back("array");
    classes.emplace_back("struct");
    classes.emplace_back("file");
    classes.emplace_back("mutex");
    classes.emplace_back("instance");


    return classes;
}

#undef NATIVE_FUNC