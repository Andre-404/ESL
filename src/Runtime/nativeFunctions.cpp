#include "nativeFunctions.h"
#include "thread.h"
#include "vm.h"
#include "../Includes/fmt/format.h"
#include <iostream>
#include <chrono>
#include <algorithm>
#include <filesystem>

robin_hood::unordered_map<string, uInt> runtime::createNativeNameTable(vector<object::ObjNativeFunc *>& natives){
    robin_hood::unordered_map<string, uInt> map;
    for(int i = 0; i < natives.size(); i++){
        map.insert_or_assign(natives[i]->name, i);
    }
    return map;
}

#define NATIVE_FUNC(name, arity, func) vector.push_back(new object::ObjNativeFunc(func, arity, name))
#define TYPE_ERROR(expectedType, argNum, value) \
    t->runtimeError(fmt::format("Expected {} for argument {}, got '{}'", expectedType, argNum, value.typeToStr()), 3)

#define INT_ERROR(argNum) \
    t->runtimeError(fmt::format("Expected integer for argument {}, got decimal", argNum), 3)

static void isNumAndInt(runtime::Thread* t, Value val, uInt argNum){
    if(!val.isNumber()) TYPE_ERROR("number", argNum, val);
    if(!IS_INT(val.asNumber())) INT_ERROR(argNum);
}


vector<object::ObjNativeFunc*> runtime::createNativeFuncs(){
    vector<object::ObjNativeFunc*> vector;
    NATIVE_FUNC("print", -1, [](Thread* t, int argCount) {
        for(int i = argCount - 1; i >= 0; i--){
            t->peek(i).print();
        }
        std::cout << "\n";
        t->popn(argCount);
        t->push(Value::nil());
        return true;
    });
    NATIVE_FUNC("input", 0, [](Thread* t, int argCount) {
        string str;
        std::getline(std::cin, str);
        t->push(Value(new object::ObjString(str)));
        return true;
    });

    NATIVE_FUNC("ms_since_epoch", 0, [](Thread* t, int argCount) {
        double duration = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(Value(duration));
        return true;
    });
    NATIVE_FUNC("micros_since_epoch", 0, [](Thread* t, int argCount) {
        double duration = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(Value(duration));
        return true;
    });
    // Random number generator
    NATIVE_FUNC("random_num", 0, [](Thread* t, int argCount) {
        double randomNumber = std::uniform_int_distribution<long long>(-INT64_MAX, INT64_MAX)(t->vm->rng);
        t->push(Value(randomNumber));
        return true;
    });
    NATIVE_FUNC("random_range", 2, [](Thread* t, int argCount) {
        Value num2 = t->pop();
        Value num1 = t->pop();
        if(!num1.isNumber()) TYPE_ERROR("number", 0, num1);
        if(!num2.isNumber()) TYPE_ERROR("number", 1, num2);
        if(num1.asNumber() > num2.asNumber())
            t->runtimeError(fmt::format("Argument 0 is {}, must be less than argument 1 which is {}.", num1.asNumber(), num2.asNumber()), 3);
        double randomNumber = std::uniform_int_distribution<long long>(num1.asNumber(), num2.asNumber())(t->vm->rng);
        t->push(Value(randomNumber));
        return true;
    });
    NATIVE_FUNC("random_set_seed", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        if (FLOAT_EQ(num.asNumber(), -1)) t->vm->rng.seed(std::chrono::steady_clock::now().time_since_epoch().count());
        else t->vm->rng.seed(num.asNumber());
        t->push(Value::nil());
        return true;
    });

    // Numbers
    NATIVE_FUNC("is_int", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(IS_INT(num.asNumber())));
        return true;
    });
    // Math
    NATIVE_FUNC("floor", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(floor(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("ceil", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(ceil(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("round", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(round(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("sqr", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(pow(num.asNumber(), 2)));
        return true;
    });
    NATIVE_FUNC("sqrt", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(sqrt(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("pow", 2, [](Thread* t, int argCount) {
        Value exponent = t->pop();
        Value base = t->pop();
        if(!base.isNumber()) TYPE_ERROR("number", 0, base);
        if(!exponent.isNumber()) TYPE_ERROR("number", 0, exponent);

        t->push(Value(pow(base.asNumber(), exponent.asNumber())));
        return true;
    });
    NATIVE_FUNC("log2", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(log2(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("log10", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(log10(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("log", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(log(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("logn", 2, [](Thread* t, int argCount) {
        Value num = t->pop();
        Value base = t->pop();
        if(!base.isNumber()) TYPE_ERROR("number", 0, base);
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(log(num.asNumber()) / log(base.asNumber())));
        return true;
    });
    NATIVE_FUNC("sin", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(sin(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("dsin", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(sin((num.asNumber()*180)/std::numbers::pi_v<double>)));
        return true;
    });
    NATIVE_FUNC("cos", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(cos(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("dcos", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(cos((num.asNumber()*180)/std::numbers::pi_v<double>)));
        return true;
    });
    NATIVE_FUNC("tan", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(tan(num.asNumber())));
        return true;
    });
    NATIVE_FUNC("dtan", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!num.isNumber()) TYPE_ERROR("number", 0, num);

        t->push(Value(tan((num.asNumber()*180)/std::numbers::pi_v<double>)));
        return true;
    });

    NATIVE_FUNC("min", -1, [](Thread* t, int argCount) {
        if(argCount < 2) t->runtimeError(fmt::format("Function 'min' requires at least 2 arguments, got {}", argCount), 2);
        double minVal;
        for(int i = 0; i < argCount; i++){
            // -1 because arguments start at peek(argCount - 1)
            Value num = t->peek(argCount - i - 1);
            if(!num.isNumber()) TYPE_ERROR("number", i, num);
            if(i == 0) minVal = num.asNumber();
            else minVal = std::min(minVal, num.asNumber());
        }
        t->popn(argCount);

        t->push(Value(minVal));
        return true;
    });
    NATIVE_FUNC("max", -1, [](Thread* t, int argCount) {
        if(argCount < 2) t->runtimeError(fmt::format("Function 'max' requires at least 2 arguments, got {}", argCount), 2);
        double maxVal;
        for(int i = 0; i < argCount; i++){
            // -1 because arguments start at peek(argCount - 1)
            Value num = t->peek(argCount - i - 1);
            if(!num.isNumber()) TYPE_ERROR("number", i, num);
            if(i == 0) maxVal = num.asNumber();
            else maxVal = std::max(maxVal, num.asNumber());
        }
        t->popn(argCount);

        t->push(Value(maxVal));
        return true;
    });
    NATIVE_FUNC("mean", -1, [](Thread* t, int argCount) {
        if(argCount < 2) t->runtimeError(fmt::format("Function 'mean' requires at least 2 arguments, got {}", argCount), 2);
        double sum = 0;
        for(int i = 0; i < argCount; i++){
            // -1 because arguments start at peek(argCount - 1)
            Value num = t->peek(argCount - i - 1);
            if(!num.isNumber()) TYPE_ERROR("number", i, num);
            sum += num.asNumber();
        }
        t->popn(argCount);

        t->push(Value(sum / static_cast<double>(argCount)));
        return true;
    });
    // Create objects
    NATIVE_FUNC("create_array", -1, [](Thread* t, int argCount) {
        if(argCount == 0 || argCount > 2 ) t->runtimeError(fmt::format("Function 'create_array' expects 1 or 2 arguments, got {}", argCount), 2);
        Value fillVal;
        Value arraySize;
        if(argCount == 2){
            fillVal = t->pop();
            arraySize = t->pop();
        }else arraySize = t->pop();

        isNumAndInt(t, arraySize, 0);
        double arrSize = arraySize.asNumber();

        auto arr = new object::ObjArray(arrSize);
        if(!fillVal.isNil()) std::fill(arr->values.begin(), arr->values.end(), fillVal);
        t->push(Value(arr));
        return true;
    });
    NATIVE_FUNC("mutex", 0, [](Thread* t, int argCount) {
        t->push(Value(new object::ObjMutex()));
        return true;
    });

    // Files
    NATIVE_FUNC("open_file_read", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!path.isString()) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(path.asString()->str, 0);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(Value(file));
        return true;
    });
    NATIVE_FUNC("open_file_write", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!path.isString()) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(path.asString()->str, 1);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(Value(file));
        return true;
    });
    NATIVE_FUNC("file_exists", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!path.isString()) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = path.asString()->str;

        t->push(Value(std::filesystem::exists(p)));
        return true;
    });
    NATIVE_FUNC("file_delete", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!path.isString()) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = path.asString()->str;
        if(!std::filesystem::exists(p)) t->runtimeError(fmt::format("File/directory in path '{}' doesn't exist.", p.string()), 7);

        try{
            std::filesystem::remove(p);
        }catch(std::filesystem::filesystem_error& err){
            t->runtimeError(fmt::format("OS level error: {}", err.what()), 8);
        }

        t->push(Value::nil());
        return true;
    });
    NATIVE_FUNC("file_rename", 2, [](Thread* t, int argCount) {
        Value newName = t->pop();
        if(!newName.isString()) TYPE_ERROR("string", 1, newName);
        Value path = t->pop();
        if(!path.isString()) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = path.asString()->str;
        if(!std::filesystem::exists(p)) t->runtimeError(fmt::format("File/directory in path '{}' doesn't exist.", p.string()), 7);

        try{
            std::filesystem::rename(p, newName.asString()->str);
        }catch(std::filesystem::filesystem_error& err){
            t->runtimeError(fmt::format("OS level error: {}", err.what()), 8);
        }

        t->push(Value::nil());
        return true;
    });

    return vector;
}
void strRangeCheck(runtime::Thread* t, uInt argNum, Value indexVal, uInt64 end) {
    isNumAndInt(t, indexVal, argNum);
    double index = indexVal.asNumber();
    if (index >= 0 && index <= end) return;
    t->runtimeError(fmt::format("String length is {}, argument {} is {}", end, argNum, static_cast<uInt64>(index)), 9);
};

// These are required because the main thread might want to start a GC run while this thread is in the process of acquiring a mutex
// If this blocks, main thread needs to know that it can safely run GC since this thread is blocked
static void incThreadWait(runtime::Thread* t){
    if(t == t->vm->mainThread) return;
    // If this is a child thread and the GC must run, notify the main thread that this one is paused
    // Main thread sends the notification when to awaken
    {
        std::scoped_lock lk(t->vm->pauseMtx);
        t->vm->threadsPaused.fetch_add(1);
    }
    // Only the main thread waits for mainThreadCv
    t->vm->mainThreadCv.notify_one();
}
static void decThreadWait(runtime::Thread* t){
    if(t == t->vm->mainThread) return;
    // If this is a child thread and the GC must run, notify the main thread that this one is paused
    // Main thread sends the notification when to awaken
    {
        std::scoped_lock lk(t->vm->pauseMtx);
        t->vm->threadsPaused.fetch_sub(1);
    }
    // Only the main thread waits for mainThreadCv
    t->vm->mainThreadCv.notify_one();
}

#define BOUND_NATIVE(name, arity, func) classes.back().methods.insert_or_assign(name, BuiltinMethod(func, arity))
vector<runtime::BuiltinClass> runtime::createBuiltinClasses(){
    vector<runtime::BuiltinClass> classes;
    // Common
    classes.emplace_back();
    BOUND_NATIVE("is_number", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isNumber()));
        return false;
    });
    BOUND_NATIVE("is_bool", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isBool()));
        return false;
    });
    BOUND_NATIVE("is_string", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isString()));
        return false;
    });
    BOUND_NATIVE("is_array", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isArray()));
        return false;
    });
    BOUND_NATIVE("is_function", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(Value(val.isClosure() || val.isBoundMethod() || val.isNativeFn() || val.isBoundNativeFunc()));
        return false;
    });
    BOUND_NATIVE("is_class", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isClass()));
        return false;
    });
    BOUND_NATIVE("is_instance", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(Value(val.isInstance() && val.asInstance()->klass));
        return false;
    });
    BOUND_NATIVE("is_struct", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(Value(val.isInstance() && !val.asInstance()->klass));
        return false;
    });
    BOUND_NATIVE("is_file", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isFile()));
        return false;
    });
    BOUND_NATIVE("is_mutex", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isMutex()));
        return false;
    });
    BOUND_NATIVE("is_future", 0, [](Thread*t, int argCount){
        t->push(Value(t->pop().isFuture()));
        return false;
    });

    BOUND_NATIVE("to_string", 0, [](Thread*t, int argCount){
        robin_hood::unordered_set<object::Obj*> stack;
        string str = t->pop().toString(stack);
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    // String
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("length", 0, [](Thread*t, int argCount){
        t->push(Value(static_cast<double>(t->pop().asString()->str.length())));
        return false;
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int argCount){
        Value toAppend = t->pop();
        Value str = t->pop();
        if(!toAppend.isString()) TYPE_ERROR("string", 0, toAppend);
        string newStr = str.asString()->str + toAppend.asString()->str;
        t->push(Value(new object::ObjString(newStr)));
        return false;
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int argCount){
        Value toAppend = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        if(!toAppend.isString()) TYPE_ERROR("string", 0, toAppend);
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        str.insert(pos.asNumber(), toAppend.asString()->str);
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.erase(pos.asNumber(), len.asNumber());
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("replace", 3, [](Thread*t, int argCount){
        Value toReplace = t->pop();
        if(!toReplace.isString()) TYPE_ERROR("string", 2, toReplace);
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.replace(pos.asNumber(), len.asNumber(), toReplace.asString()->str);
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("substr", 2, [](Thread*t, int argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.substr(pos.asNumber(), len.asNumber());
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("char_at", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        string c(1, str[pos.asNumber()]);
        t->push(Value(new object::ObjString(c)));
        return false;
    });
    BOUND_NATIVE("byte_at", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(Value(static_cast<double>(str[pos.asNumber()])));
        return false;
    });
    BOUND_NATIVE("pos", 1, [](Thread*t, int argCount){
        Value substr = t->pop();
        if(!substr.isString()) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = callee.asString()->str;
        auto pos = str.find(substr.asString()->str);
        auto p = static_cast<double>(pos);
        if(pos == str.npos) p = -1;
        t->push(Value(p));
        return false;
    });
    BOUND_NATIVE("last_pos", 1, [](Thread*t, int argCount){
        Value substr = t->pop();
        if(!substr.isString()) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = callee.asString()->str;
        auto pos = str.rfind(substr.asString()->str);
        auto p = static_cast<double>(pos);
        if(pos == str.npos) p = -1;
        t->push(Value(p));
        return false;
    });
    BOUND_NATIVE("is_upper", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(Value(std::isupper(str[pos.asNumber()]) != 0));
        return false;
    });
    BOUND_NATIVE("is_lower", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = callee.asString()->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(Value(std::isupper(str[pos.asNumber()]) == 0));
        return false;
    });
    BOUND_NATIVE("to_upper", 0, [](Thread*t, int argCount){
        Value callee = t->pop();
        string str = callee.asString()->str;
        for(char& i : str){
            i = toupper(i);
        }
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("to_lower", 0, [](Thread*t, int argCount){
        Value callee = t->pop();
        string str = callee.asString()->str;
        for(char& i : str){
            i = tolower(i);
        }
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("to_number", 0, [](Thread*t, int argCount){
        t->push(Value(static_cast<double>(std::stoi(t->pop().asString()->str))));
        return false;
    });
    BOUND_NATIVE("split", 1, [](Thread*t, int argCount){
        Value delimiter = t->pop();
        if(!delimiter.isString()) TYPE_ERROR("string", 0, delimiter);
        string baseString = t->pop().asString()->str;
        string delStr = delimiter.asString()->str;
        auto arr = new object::ObjArray();
        uInt64 pos = baseString.npos;
        while((pos = baseString.find(delStr)) != baseString.npos){
            string temp = baseString.substr(0, pos);
            arr->values.push_back(Value(new object::ObjString(temp)));
            baseString.erase(0, pos + delStr.length());
        }
        arr->values.push_back(Value(new object::ObjString(baseString)));
        t->push(Value(arr));
        return false;
    });
    // Array
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("push", 1, [](Thread*t, int argCount){
        Value val = t->pop();
        t->peek(0).asArray()->values.push_back(val);
        return false;
    });
    BOUND_NATIVE("pop", 0, [](Thread*t, int argCount){
        auto arr = t->pop().asArray();
        Value val = arr->values.back();
        arr->values.pop_back();
        t->push(val);
        return false;
    });
    BOUND_NATIVE("copy", 0, [](Thread*t, int argCount){
        auto arr = t->pop().asArray();
        auto newArr = new object::ObjArray();
        newArr->values = arr->values;
        t->push(Value(newArr));
        return false;
    });
    BOUND_NATIVE("resize", -1, [](Thread*t, int argCount){
        Value fill = Value::nil();
        Value newSize;
        if(argCount == 2){
            fill = t->pop();
            newSize = t->pop();
        }else if(argCount == 1) newSize = t->pop();
        else t->runtimeError(fmt::format("Expected 1 or 2 arguments, got {}.", argCount), 2);
        isNumAndInt(t, newSize, 0);
        if(newSize.asNumber() < 0) t->runtimeError("Expected positive integer for argument 0, got negative.", 3);
        uInt64 s = newSize.asNumber();
        t->peek(0).asArray()->values.resize(s, fill);
        return false;
    });
    BOUND_NATIVE("length", 0, [](Thread*t, int argCount){
        auto arr = t->pop().asArray();
        t->push(Value(static_cast<double>(arr->values.size())));
        return false;
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int argCount){
        Value val = t->pop();
        Value index = t->pop();
        auto arr = t->peek(0).asArray();
        isNumAndInt(t, index, 0);
        double ind = index.asNumber();
        if(ind < 0 || ind > arr->values.size())
            t->runtimeError(fmt::format("Index {} outside of range [0, {}]", ind, arr->values.size()), 3);

        arr->values.insert(arr->values.begin() + ind, val);
        return false;
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int argCount){
        Value length = t->pop();
        Value index = t->pop();
        auto arr = t->peek(0).asArray();
        isNumAndInt(t, index, 0);
        isNumAndInt(t, length, 1);
        double ind = index.asNumber();
        double len = length.asNumber();
        if(ind < 0 || ind > arr->values.size())
            t->runtimeError(fmt::format("Index {} outside of range [0, {}]", ind, arr->values.size()), 3);
        if(len < 0) t->runtimeError("Expected positive integer for argument 1, got negative.", 3);

        auto end = (ind + len > arr->values.size()) ? arr->values.end() : arr->values.begin() + ind + len;
        arr->values.erase(arr->values.begin() + ind, end);
        return false;
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int argCount){
        Value other = t->pop();
        if(!other.isArray()) TYPE_ERROR("array", 0, other);
        if(t->peek(0) == other) t->runtimeError("Cannot concat array to itself.", 3);
        auto& arr1 = t->peek(0).asArray()->values;
        auto& arr2 = other.asArray()->values;
        arr1.insert(arr1.end(), arr2.begin(), arr2.end());
        return false;
    });
    BOUND_NATIVE("reverse", 0, [](Thread*t, int argCount){
        auto arr = t->peek(0).asArray();
        std::reverse(arr->values.begin(), arr->values.end());
        return false;
    });
    BOUND_NATIVE("equals", 1, [](Thread*t, int argCount){
        Value other = t->pop();
        if(!other.isArray()) TYPE_ERROR("array", 0, other);
        auto& arr1 = t->pop().asArray()->values;
        auto& arr2 = other.asArray()->values;
        if(arr1.size() != arr2.size()){
            t->push(Value(false));
            return false;
        }
        for(uInt i = 0; i < arr1.size(); i++){
            if(arr1[i] != arr2[i]) {
                t->push(Value(false));
                return false;
            }
        }
        t->push(Value(true));
        return false;
    });

    // File
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("open_read", 0, [](Thread*t, int argCount){
        auto file = t->peek(0).asFile();
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 0;
        return false;
    });
    BOUND_NATIVE("open_write", 0, [](Thread*t, int argCount){
        auto file = t->peek(0).asFile();
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 1;
        return false;
    });
    BOUND_NATIVE("close", 0, [](Thread*t, int argCount){
        std::fstream& stream = t->peek(0).asFile()->stream;
        if(!stream.is_open()) t->runtimeError("Trying to close a file that isn't opened", 8);
        stream.close();
        return false;
    });
    BOUND_NATIVE("path", 0, [](Thread*t, int argCount){
        t->push(Value(new object::ObjString(t->pop().asFile()->path)));
        return false;
    });
    BOUND_NATIVE("readln", 0, [](Thread*t, int argCount){
        auto f = t->pop().asFile();
        if(f->openType != 0) t->runtimeError("File open for reading, not writing.", 8);
        std::fstream& stream = f->stream;
        string str;
        std::getline(stream, str);
        t->push(Value(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("write", 1, [](Thread*t, int argCount){
        Value str = t->pop();
        if(!str.isString()) TYPE_ERROR("string", 0, str);
        auto f = t->peek(0).asFile();
        if(f->openType != 1) t->runtimeError("File open for writing, not reading.", 8);
        std::fstream& stream =f->stream;
        stream << str.asString()->str;
        return false;
    });

    // Mutex
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("exclusive_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        mutex.asMutex()->mtx.lock();
        decThreadWait(t);
        t->push(Value::nil());
        return false;
    });
    BOUND_NATIVE("try_exclusive_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // Try_lock doesn't block
        bool res = mutex.asMutex()->mtx.try_lock();
        t->push(Value(res));
        return false;
    });
    BOUND_NATIVE("shared_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        mutex.asMutex()->mtx.lock_shared();
        decThreadWait(t);
        t->push(Value::nil());
        return false;
    });
    BOUND_NATIVE("try_shared_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // Try_lock_shared doesn't block
        mutex.asMutex()->mtx.try_lock_shared();;
        t->push(Value::nil());
        return false;
    });
    BOUND_NATIVE("unlock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        mutex.asMutex()->mtx.unlock();
        t->push(Value::nil());
        return false;
    });
    // Future
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("cancel", 0, [](Thread*t, int argCount){
        auto fut = t->pop().asFuture();
        fut->thread->cancelToken.store(true);
        t->push(Value::nil());
        return false;
    });
    BOUND_NATIVE("is_done", 0, [](Thread*t, int argCount){
        auto fut = t->pop().asFuture();
        auto done = fut->fut.wait_until(std::chrono::system_clock::time_point::min());
        t->push(Value(!(done == std::future_status::timeout)));
        return false;
    });

    return classes;
}
#undef BOUND_NATIVE
#undef NATIVE_FUNC