#include "nativeFunctions.h"
#include "thread.h"
#include "vm.h"
#include "../Includes/fmt/format.h"
#include "../codegen/valueHelpersInline.cpp"
#include <iostream>
#include <chrono>
#include <algorithm>
#include <filesystem>

using namespace valueHelpers;

robin_hood::unordered_map<string, uInt> runtime::createNativeNameTable(vector<object::ObjNativeFunc *>& natives){
    robin_hood::unordered_map<string, uInt> map;
    for(int i = 0; i < natives.size(); i++){
        map.insert_or_assign(natives[i]->name, i);
    }
    return map;
}

#define NATIVE_FUNC(name, arity, func) vector.push_back(new object::ObjNativeFunc(func, arity, name))
#define TYPE_ERROR(expectedType, argNum, value) \
    t->runtimeError(fmt::format("Expected {} for argument {}, got '{}'", expectedType, argNum, typeToStr(value)), 3)

#define INT_ERROR(argNum) \
    t->runtimeError(fmt::format("Expected integer for argument {}, got decimal", argNum), 3)

static void isNumAndInt(runtime::Thread* t, Value val, uInt argNum){
    if(!isNumber(val)) TYPE_ERROR("number", argNum, val);
    if(!isInt(val)) INT_ERROR(argNum);
}


vector<object::ObjNativeFunc*> runtime::createNativeFuncs(){
    vector<object::ObjNativeFunc*> vector;
    NATIVE_FUNC("print", -1, [](Thread* t, int argCount) {
        for(int i = argCount - 1; i >= 0; i--){
            print(t->peek(i));
        }
        std::cout << "\n";
        t->popn(argCount);
        t->push(encodeNil());
        return true;
    });
    NATIVE_FUNC("input", 0, [](Thread* t, int argCount) {
        string str;
        std::getline(std::cin, str);
        t->push(encodeObj(new object::ObjString(str)));
        return true;
    });

    NATIVE_FUNC("ms_since_epoch", 0, [](Thread* t, int argCount) {
        double duration = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(encodeNumber(duration));
        return true;
    });
    NATIVE_FUNC("micros_since_epoch", 0, [](Thread* t, int argCount) {
        double duration = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(encodeNumber(duration));
        return true;
    });
    // Random number generator
    NATIVE_FUNC("random_num", 0, [](Thread* t, int argCount) {
        // TODO: Make it return ints??
        double randomNumber = std::uniform_int_distribution<long long>(-INT64_MAX, INT64_MAX)(t->vm->rng);
        t->push(encodeNumber(randomNumber));
        return true;
    });
    NATIVE_FUNC("random_range", 2, [](Thread* t, int argCount) {
        Value num2 = t->pop();
        Value num1 = t->pop();
        if(!isNumber(num1)) TYPE_ERROR("number", 0, num1);
        if(!isNumber(num2)) TYPE_ERROR("number", 1, num2);
        if(decodeNumber(num1) > decodeNumber(num2))
            t->runtimeError(fmt::format("Argument 0 is {}, must be less than argument 1 which is {}.",
                                        decodeNumber(num1),
                                        decodeNumber(num2)), 3);
        double randomNumber = std::uniform_int_distribution<long long>(decodeNumber(num1), decodeNumber(num2))(t->vm->rng);
        t->push(encodeNumber(randomNumber));
        return true;
    });
    NATIVE_FUNC("random_set_seed", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        if (FLOAT_EQ(decodeNumber(num), -1)) t->vm->rng.seed(std::chrono::steady_clock::now().time_since_epoch().count());
        else t->vm->rng.seed(decodeNumber(num));
        t->push(encodeNil());
        return true;
    });

    // Numbers
    NATIVE_FUNC("is_int", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(encodeBool(isInt(num)));
        return true;
    });
    // Math
    NATIVE_FUNC("floor", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(floor(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("ceil", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(ceil(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("round", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(round(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("sqrt", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(encodeNumber(sqrt(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("pow", 2, [](Thread* t, int argCount) {
        Value exponent = t->pop();
        Value base = t->pop();
        if(!isNumber(base)) TYPE_ERROR("number", 0, base);
        if(!isNumber(exponent)) TYPE_ERROR("number", 0, exponent);

        t->push(encodeNumber(pow(decodeNumber(base), decodeNumber(exponent))));
        return true;
    });
    NATIVE_FUNC("log2", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log2(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("log10", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log10(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("log", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("logn", 2, [](Thread* t, int argCount) {
        Value num = t->pop();
        Value base = t->pop();
        if(!isNumber(base)) TYPE_ERROR("number", 0, base);
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log(decodeNumber(num)) / log(decodeNumber(base))));
        return true;
    });
    NATIVE_FUNC("sin", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(sin(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("dsin", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(sin((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
        return true;
    });
    NATIVE_FUNC("cos", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(cos(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("dcos", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(cos((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
        return true;
    });
    NATIVE_FUNC("tan", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(tan(decodeNumber(num))));
        return true;
    });
    NATIVE_FUNC("dtan", 1, [](Thread* t, int argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(tan((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
        return true;
    });

    NATIVE_FUNC("min", -1, [](Thread* t, int argCount) {
        if(argCount < 2) t->runtimeError(fmt::format("Function 'min' requires at least 2 arguments, got {}", argCount), 2);
        double minVal;
        for(int i = 0; i < argCount; i++){
            // -1 because arguments start at peek(argCount - 1)
            Value num = t->peek(argCount - i - 1);
            if(!isNumber(num)) TYPE_ERROR("number", i, num);
            if(i == 0) minVal = decodeNumber(num);
            else minVal = std::min(minVal, decodeNumber(num));
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
            if(!isNumber(num)) TYPE_ERROR("number", i, num);
            if(i == 0) maxVal = decodeNumber(num);
            else maxVal = std::max(maxVal, decodeNumber(num));
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
            if(!isNumber(num)) TYPE_ERROR("number", i, num);
            sum += decodeNumber(num);
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
        double arrSize = decodeNumber(arraySize);

        auto arr = new object::ObjArray(arrSize);
        if(!isNil(fillVal)) std::fill(arr->values.begin(), arr->values.end(), fillVal);
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
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(asString(path)->str, 0);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(encodeObj(file));
        return true;
    });
    NATIVE_FUNC("open_file_write", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(asString(path)->str, 1);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(Value(file));
        return true;
    });
    NATIVE_FUNC("file_exists", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = asString(path)->str;

        t->push(Value(std::filesystem::exists(p)));
        return true;
    });
    NATIVE_FUNC("file_delete", 1, [](Thread* t, int argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = asString(path)->str;
        if(!std::filesystem::exists(p)) t->runtimeError(fmt::format("File/directory in path '{}' doesn't exist.", p.string()), 7);

        try{
            std::filesystem::remove(p);
        }catch(std::filesystem::filesystem_error& err){
            t->runtimeError(fmt::format("OS level error: {}", err.what()), 8);
        }

        t->push(encodeNil());
        return true;
    });
    NATIVE_FUNC("file_rename", 2, [](Thread* t, int argCount) {
        Value newName = t->pop();
        if(!isString(newName)) TYPE_ERROR("string", 1, newName);
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = asString(path)->str;
        if(!std::filesystem::exists(p)) t->runtimeError(fmt::format("File/directory in path '{}' doesn't exist.", p.string()), 7);

        try{
            std::filesystem::rename(p, asString(newName)->str);
        }catch(std::filesystem::filesystem_error& err){
            t->runtimeError(fmt::format("OS level error: {}", err.what()), 8);
        }

        t->push(encodeNil());
        return true;
    });

    return vector;
}
void strRangeCheck(runtime::Thread* t, uInt argNum, Value indexVal, uInt64 end) {
    isNumAndInt(t, indexVal, argNum);
    double index = decodeNumber(indexVal);
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
        t->push(encodeBool(isNumber(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_bool", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isBool(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_string", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isString(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_array", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isArray(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_function", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(encodeBool(isClosure(val) || isBoundMethod(val) || isNativeFn(val) || isBoundNativeFunc(val)));
        return false;
    });
    BOUND_NATIVE("is_class", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isClass(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_instance", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(encodeBool(isInstance(val) && asInstance(val)->klass));
        return false;
    });
    BOUND_NATIVE("is_struct", 0, [](Thread*t, int argCount){
        Value val = t->pop();
        t->push(encodeBool(isInstance(val) && !asInstance(val)->klass));
        return false;
    });
    BOUND_NATIVE("is_file", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isFile(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_mutex", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isMutex(t->pop())));
        return false;
    });
    BOUND_NATIVE("is_future", 0, [](Thread*t, int argCount){
        t->push(encodeBool(isFuture(t->pop())));
        return false;
    });

    BOUND_NATIVE("to_string", 0, [](Thread*t, int argCount){
        robin_hood::unordered_set<object::Obj*> stack;
        string str = toString(t->pop());
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    // String
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("length", 0, [](Thread*t, int argCount){
        t->push(encodeNumber(asString(t->pop())->str.length()));
        return false;
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int argCount){
        Value toAppend = t->pop();
        Value str = t->pop();
        if(!isString(toAppend)) TYPE_ERROR("string", 0, toAppend);
        string newStr = asString(str)->str + asString(toAppend)->str;
        t->push(encodeObj(new object::ObjString(newStr)));
        return false;
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int argCount){
        Value toAppend = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        if(!isString(toAppend)) TYPE_ERROR("string", 0, toAppend);
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        str.insert(decodeNumber(pos), asString(toAppend)->str);
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.erase(decodeNumber(pos), decodeNumber(len));
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("replace", 3, [](Thread*t, int argCount){
        Value toReplace = t->pop();
        if(!isString(toReplace)) TYPE_ERROR("string", 2, toReplace);
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.replace(decodeNumber(pos), decodeNumber(len), asString(toReplace)->str);
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("substr", 2, [](Thread*t, int argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.substr(decodeNumber(pos), decodeNumber(len));
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("char_at", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        string c(1, str[decodeNumber(pos)]);
        t->push(encodeObj(new object::ObjString(c)));
        return false;
    });
    BOUND_NATIVE("byte_at", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeNumber(str[decodeNumber(pos)]));
        return false;
    });
    BOUND_NATIVE("pos", 1, [](Thread*t, int argCount){
        Value substr = t->pop();
        if(!isString(substr)) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = asString(callee)->str;
        auto pos = str.find(asString(substr)->str);
        auto p = static_cast<double>(pos);
        if(pos == str.npos) p = -1;
        t->push(encodeNumber(p));
        return false;
    });
    BOUND_NATIVE("last_pos", 1, [](Thread*t, int argCount){
        Value substr = t->pop();
        if(!isString(substr)) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = asString(callee)->str;
        auto pos = str.rfind(asString(substr)->str);
        int32_t p = pos;
        if(pos == str.npos) p = -1;
        t->push(encodeNumber(p));
        return false;
    });
    BOUND_NATIVE("is_upper", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeBool(std::isupper(str[decodeNumber(pos)]) != 0));
        return false;
    });
    BOUND_NATIVE("is_lower", 1, [](Thread*t, int argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeBool(std::isupper(str[decodeNumber(pos)]) == 0));
        return false;
    });
    BOUND_NATIVE("to_upper", 0, [](Thread*t, int argCount){
        Value callee = t->pop();
        string str = asString(callee)->str;
        for(char& i : str){
            i = toupper(i);
        }
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("to_lower", 0, [](Thread*t, int argCount){
        Value callee = t->pop();
        string str = asString(callee)->str;
        for(char& i : str){
            i = tolower(i);
        }
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("to_number", 0, [](Thread*t, int argCount){
        t->push(encodeNumber(std::stoi(asString(t->pop())->str)));
        return false;
    });
    BOUND_NATIVE("split", 1, [](Thread*t, int argCount){
        Value delimiter = t->pop();
        if(!isString(delimiter)) TYPE_ERROR("string", 0, delimiter);
        string baseString = asString(t->pop())->str;
        string delStr = asString(delimiter)->str;
        auto arr = new object::ObjArray();
        uInt64 pos = baseString.npos;
        while((pos = baseString.find(delStr)) != baseString.npos){
            string temp = baseString.substr(0, pos);
            arr->values.push_back(encodeObj(new object::ObjString(temp)));
            baseString.erase(0, pos + delStr.length());
        }
        arr->values.push_back(encodeObj(new object::ObjString(baseString)));
        t->push(encodeObj(arr));
        return false;
    });
    // Array
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("push", 1, [](Thread*t, int argCount){
        Value val = t->pop();
        asArray(t->peek(0))->values.emplace_back(val);
        return false;
    });
    BOUND_NATIVE("pop", 0, [](Thread*t, int argCount){
        auto arr = asArray(t->pop());
        Value val = arr->values.back();
        arr->values.pop_back();
        t->push(val);
        return false;
    });
    BOUND_NATIVE("copy", 0, [](Thread*t, int argCount){
        auto arr = asArray(t->pop());
        auto newArr = new object::ObjArray();
        newArr->values = arr->values;
        t->push(encodeObj(newArr));
        return false;
    });
    BOUND_NATIVE("resize", -1, [](Thread*t, int argCount){
        Value fill = encodeNil();
        Value newSize;
        if(argCount == 2){
            fill = t->pop();
            newSize = t->pop();
        }else if(argCount == 1) newSize = t->pop();
        else t->runtimeError(fmt::format("Expected 1 or 2 arguments, got {}.", argCount), 2);
        isNumAndInt(t, newSize, 0);
        if(decodeNumber(newSize) < 0) t->runtimeError("Expected positive integer for argument 0, got negative.", 3);
        uInt64 s = decodeNumber(newSize);
        asArray(t->peek(0))->values.resize(s, fill);
        return false;
    });
    BOUND_NATIVE("length", 0, [](Thread*t, int argCount){
        auto arr = asArray(t->pop());
        t->push(encodeNumber(arr->values.size()));
        return false;
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int argCount){
        Value val = t->pop();
        Value index = t->pop();
        auto arr = asArray(t->peek(0));
        isNumAndInt(t, index, 0);
        int32_t ind = decodeInt(index);
        if(ind < 0 || ind > arr->values.size())
            t->runtimeError(fmt::format("Index {} outside of range [0, {}]", ind, arr->values.size()), 3);

        arr->values.insert(arr->values.begin() + ind, val);
        return false;
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int argCount){
        Value length = t->pop();
        Value index = t->pop();
        auto arr = asArray(t->peek(0));
        isNumAndInt(t, index, 0);
        isNumAndInt(t, length, 1);
        int32_t ind = decodeInt(index);
        int32_t len = decodeInt(length);
        if(ind < 0 || ind > arr->values.size())
            t->runtimeError(fmt::format("Index {} outside of range [0, {}]", ind, arr->values.size()), 3);
        if(len < 0) t->runtimeError("Expected positive integer for argument 1, got negative.", 3);

        auto end = (ind + len > arr->values.size()) ? arr->values.end() : arr->values.begin() + ind + len;
        arr->values.erase(arr->values.begin() + ind, end);
        return false;
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int argCount){
        Value other = t->pop();
        if(!isArray(other)) TYPE_ERROR("array", 0, other);
        if(t->peek(0) == other) t->runtimeError("Cannot concat array to itself.", 3);
        auto& arr1 = asArray(t->peek(0))->values;
        auto& arr2 = asArray(other)->values;
        arr1.insert(arr1.end(), arr2.begin(), arr2.end());
        return false;
    });
    BOUND_NATIVE("reverse", 0, [](Thread*t, int argCount){
        auto arr = asArray(t->peek(0));
        std::reverse(arr->values.begin(), arr->values.end());
        return false;
    });
    BOUND_NATIVE("equals", 1, [](Thread*t, int argCount){
        Value other = t->pop();
        if(!isArray(other)) TYPE_ERROR("array", 0, other);
        auto& arr1 = asArray(t->pop())->values;
        auto& arr2 = asArray(other)->values;
        if(arr1.size() != arr2.size()){
            t->push(encodeBool(false));
            return false;
        }
        for(uInt i = 0; i < arr1.size(); i++){
            if(!equals(arr1[i], arr2[i])) {
                t->push(encodeBool(false));
                return false;
            }
        }
        t->push(encodeBool(true));
        return false;
    });

    // File
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("open_read", 0, [](Thread*t, int argCount){
        auto file = asFile(t->peek(0));
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 0;
        return false;
    });
    BOUND_NATIVE("open_write", 0, [](Thread*t, int argCount){
        auto file = asFile(t->peek(0));
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 1;
        return false;
    });
    BOUND_NATIVE("is_open_read", 0, [](Thread*t, int argCount){
        auto file = asFile(t->pop());
        std::fstream& stream = file->stream;
        t->push(Value(stream.is_open() && file->openType == 0));
        return false;
    });
    BOUND_NATIVE("is_open_write", 0, [](Thread*t, int argCount){
        auto file = asFile(t->pop());
        std::fstream& stream = file->stream;
        t->push(Value(stream.is_open() && file->openType == 1));
        return false;
    });
    BOUND_NATIVE("close", 0, [](Thread*t, int argCount){
        std::fstream& stream = asFile(t->peek(0))->stream;
        if(!stream.is_open()) t->runtimeError("Trying to close a file that isn't opened", 8);
        stream.close();
        return false;
    });
    BOUND_NATIVE("path", 0, [](Thread*t, int argCount){
        t->push(encodeObj(new object::ObjString(asFile(t->pop())->path)));
        return false;
    });
    BOUND_NATIVE("readln", 0, [](Thread*t, int argCount){
        auto f = asFile(t->pop());
        if(f->openType != 0) t->runtimeError("File open for reading, not writing.", 8);
        std::fstream& stream = f->stream;
        string str;
        std::getline(stream, str);
        t->push(encodeObj(new object::ObjString(str)));
        return false;
    });
    BOUND_NATIVE("write", 1, [](Thread*t, int argCount){
        Value str = t->pop();
        if(!isString(str)) TYPE_ERROR("string", 0, str);
        auto f = asFile(t->peek(0));
        if(f->openType != 1) t->runtimeError("File open for writing, not reading.", 8);
        std::fstream& stream =f->stream;
        stream << asString(str)->str;
        return false;
    });

    // Mutex
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("exclusive_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        asMutex(mutex)->mtx.lock();
        decThreadWait(t);
        t->push(encodeNil());
        return false;
    });
    BOUND_NATIVE("try_exclusive_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // Try_lock doesn't block
        bool res = asMutex(mutex)->mtx.try_lock();
        t->push(encodeBool(res));
        return false;
    });
    BOUND_NATIVE("shared_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        asMutex(mutex)->mtx.lock_shared();
        decThreadWait(t);
        t->push(encodeNil());
        return false;
    });
    BOUND_NATIVE("try_shared_lock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // Try_lock_shared doesn't block
        asMutex(mutex)->mtx.try_lock_shared();;
        t->push(encodeNil());
        return false;
    });
    BOUND_NATIVE("unlock", 0, [](Thread*t, int argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        asMutex(mutex)->mtx.unlock();
        t->push(encodeNil());
        return false;
    });
    // Future
    classes.emplace_back(&classes[0]);
    BOUND_NATIVE("cancel", 0, [](Thread*t, int argCount){
        auto fut = asFuture(t->pop());
        fut->thread->cancelToken.store(true, std::memory_order_relaxed);
        fut->thread->pauseToken.store(true, std::memory_order_relaxed);
        t->push(encodeNil());
        return false;
    });
    BOUND_NATIVE("is_done", 0, [](Thread*t, int argCount){
        auto fut = asFuture(t->pop());
        auto done = fut->fut.wait_until(std::chrono::system_clock::time_point::min());
        t->push(encodeBool(!(done == std::future_status::timeout)));
        return false;
    });

    return classes;
}
#undef BOUND_NATIVE
#undef NATIVE_FUNC