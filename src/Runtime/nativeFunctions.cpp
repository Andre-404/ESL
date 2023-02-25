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

ankerl::unordered_dense::map<string, uInt> runtime::createNativeNameTable(vector<object::ObjNativeFunc *>& natives){
    ankerl::unordered_dense::map<string, uInt> map;
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
    NATIVE_FUNC("print", -1, [](Thread* t, int8_t argCount) {
        for(int i = argCount - 1; i >= 0; i--){
            print(t->peek(i));
        }
        std::cout << "\n";
        t->popn(argCount);
        t->push(encodeNil());
    });
    NATIVE_FUNC("input", 0, [](Thread* t, int8_t argCount) {
        string str;
        std::getline(std::cin, str);
        t->push(encodeObj(object::ObjString::createStr(str)));
    });

    NATIVE_FUNC("ms_since_epoch", 0, [](Thread* t, int8_t argCount) {
        double duration = duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(encodeNumber(duration));
    });
    NATIVE_FUNC("micros_since_epoch", 0, [](Thread* t, int8_t argCount) {
        double duration = duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
        t->push(encodeNumber(duration));
    });
    // Random number generator
    NATIVE_FUNC("random_num", 0, [](Thread* t, int8_t argCount) {
        // TODO: Make it return ints??
        double randomNumber = std::uniform_int_distribution<long long>(-INT64_MAX, INT64_MAX)(t->vm->rng);
        t->push(encodeNumber(randomNumber));
    });
    NATIVE_FUNC("random_range", 2, [](Thread* t, int8_t argCount) {
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
    });
    NATIVE_FUNC("random_set_seed", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        if (FLOAT_EQ(decodeNumber(num), -1)) t->vm->rng.seed(std::chrono::steady_clock::now().time_since_epoch().count());
        else t->vm->rng.seed(decodeNumber(num));
        t->push(encodeNil());
    });

    // Numbers
    NATIVE_FUNC("is_int", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(encodeBool(isInt(num)));
    });
    // Math
    NATIVE_FUNC("floor", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(floor(decodeNumber(num))));
    });
    NATIVE_FUNC("ceil", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(ceil(decodeNumber(num))));
    });
    NATIVE_FUNC("round", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        // TODO: Make it return ints
        t->push(encodeNumber(round(decodeNumber(num))));
    });
    NATIVE_FUNC("sqrt", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(encodeNumber(sqrt(decodeNumber(num))));
    });
    NATIVE_FUNC("pow", 2, [](Thread* t, int8_t argCount) {
        Value exponent = t->pop();
        Value base = t->pop();
        if(!isNumber(base)) TYPE_ERROR("number", 0, base);
        if(!isNumber(exponent)) TYPE_ERROR("number", 0, exponent);

        t->push(encodeNumber(pow(decodeNumber(base), decodeNumber(exponent))));
    });
    NATIVE_FUNC("log2", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log2(decodeNumber(num))));
    });
    NATIVE_FUNC("log10", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log10(decodeNumber(num))));
    });
    NATIVE_FUNC("log", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log(decodeNumber(num))));
    });
    NATIVE_FUNC("logn", 2, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        Value base = t->pop();
        if(!isNumber(base)) TYPE_ERROR("number", 0, base);
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(log(decodeNumber(num)) / log(decodeNumber(base))));
    });
    NATIVE_FUNC("sin", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(sin(decodeNumber(num))));
    });
    NATIVE_FUNC("dsin", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(sin((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
    });
    NATIVE_FUNC("cos", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(cos(decodeNumber(num))));
    });
    NATIVE_FUNC("dcos", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(cos((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
    });
    NATIVE_FUNC("tan", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(tan(decodeNumber(num))));
    });
    NATIVE_FUNC("dtan", 1, [](Thread* t, int8_t argCount) {
        Value num = t->pop();
        if(!isNumber(num)) TYPE_ERROR("number", 0, num);

        t->push(Value(tan((decodeNumber(num) * 180) / std::numbers::pi_v<double>)));
    });

    NATIVE_FUNC("min", -1, [](Thread* t, int8_t argCount) {
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
    });
    NATIVE_FUNC("max", -1, [](Thread* t, int8_t argCount) {
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
    });
    NATIVE_FUNC("mean", -1, [](Thread* t, int8_t argCount) {
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
    });
    // Create objects
    NATIVE_FUNC("create_array", -1, [](Thread* t, int8_t argCount) {
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
    });
    NATIVE_FUNC("mutex", 0, [](Thread* t, int8_t argCount) {
        t->push(Value(new object::ObjMutex()));
    });

    // Files
    NATIVE_FUNC("open_file_read", 1, [](Thread* t, int8_t argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(asString(path)->str, 0);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(encodeObj(file));
    });
    NATIVE_FUNC("open_file_write", 1, [](Thread* t, int8_t argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        auto file = new object::ObjFile(asString(path)->str, 1);
        if(!file->stream.good()) t->runtimeError(fmt::format("File in path {} doesn't exist.", file->path), 7);
        file->stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        t->push(Value(file));
    });
    NATIVE_FUNC("file_exists", 1, [](Thread* t, int8_t argCount) {
        Value path = t->pop();
        if(!isString(path)) TYPE_ERROR("string", 0, path);
        std::filesystem::path p = asString(path)->str;

        t->push(Value(std::filesystem::exists(p)));
    });
    NATIVE_FUNC("file_delete", 1, [](Thread* t, int8_t argCount) {
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
    });
    NATIVE_FUNC("file_rename", 2, [](Thread* t, int8_t argCount) {
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


#define BOUND_NATIVE(name, arity, func) classes.back()->methods.insert_or_assign(object::ObjString::createStr(name), new object::ObjNativeFunc(func, arity, name))
#define ADD_CLASS(name) \
do{                     \
    auto klass = new object::ObjClass(name); \
    klass->methods = baseClass->methods;     \
    classes.push_back(klass);                \
}while(false)


#define INLINE_PEEK(depth) t->stackTop[-1 - depth]
#define INLINE_POP() (*(--t->stackTop))
// When pushing/popping from arrays and such, have to take care of memory
#define MEM_ADD(size) memory::gc.heapSize += size


vector<object::ObjClass*> runtime::createBuiltinClasses(object::ObjClass* baseClass){
    vector<object::ObjClass*> classes;
    /*
    // Common
    ADD_CLASS("common");
    BOUND_NATIVE("is_number", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isNumber(t->pop())));
    });
    BOUND_NATIVE("is_bool", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isBool(t->pop())));

    });
    BOUND_NATIVE("is_string", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isString(t->pop())));

    });
    BOUND_NATIVE("is_array", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isArray(t->pop())));

    });
    BOUND_NATIVE("is_function", 0, [](Thread*t, int8_t argCount){
        Value val = t->pop();
        t->push(encodeBool(isClosure(val) || isBoundMethod(val) || isNativeFn(val)));

    });
    BOUND_NATIVE("is_class", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isClass(t->pop())));

    });
    BOUND_NATIVE("is_instance", 0, [](Thread*t, int8_t argCount){
        Value val = t->pop();
        t->push(encodeBool(isInstance(val) && asInstance(val)->klass));

    });
    BOUND_NATIVE("is_struct", 0, [](Thread*t, int8_t argCount){
        Value val = t->pop();
        t->push(encodeBool(isInstance(val) && !asInstance(val)->klass));

    });
    BOUND_NATIVE("is_file", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isFile(t->pop())));

    });
    BOUND_NATIVE("is_mutex", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isMutex(t->pop())));

    });
    BOUND_NATIVE("is_future", 0, [](Thread*t, int8_t argCount){
        t->push(encodeBool(isFuture(t->pop())));

    });

    BOUND_NATIVE("to_string", 0, [](Thread*t, int8_t argCount){
        ankerl::unordered_dense::set<object::Obj*> stack;
        string str = toString(t->pop());
        t->push(encodeObj(object::ObjString::createStr(str)));
    });*/
    // String
    ADD_CLASS("string");
    BOUND_NATIVE("length", 0, [](Thread*t, int8_t argCount){
        t->push(encodeNumber(asString(t->pop())->str.length()));
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int8_t argCount){
        Value toAppend = t->pop();
        Value str = t->pop();
        if(!isString(toAppend)) TYPE_ERROR("string", 0, toAppend);
        string newStr = asString(str)->str + asString(toAppend)->str;
        t->push(encodeObj(object::ObjString::createStr(newStr)));
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int8_t argCount){
        Value toAppend = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        if(!isString(toAppend)) TYPE_ERROR("string", 0, toAppend);
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        str.insert(decodeNumber(pos), asString(toAppend)->str);
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int8_t argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.erase(decodeNumber(pos), decodeNumber(len));
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("replace", 3, [](Thread*t, int8_t argCount){
        Value toReplace = t->pop();
        if(!isString(toReplace)) TYPE_ERROR("string", 2, toReplace);
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.replace(decodeNumber(pos), decodeNumber(len), asString(toReplace)->str);
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("substr", 2, [](Thread*t, int8_t argCount){
        Value len = t->pop();
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        isNumAndInt(t, len, 1);
        str.substr(decodeNumber(pos), decodeNumber(len));
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("char_at", 1, [](Thread*t, int8_t argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        string c(1, str[decodeNumber(pos)]);
        t->push(encodeObj(object::ObjString::createStr(c)));
    });
    BOUND_NATIVE("byte_at", 1, [](Thread*t, int8_t argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeNumber(str[decodeNumber(pos)]));
    });
    BOUND_NATIVE("pos", 1, [](Thread*t, int8_t argCount){
        Value substr = t->pop();
        if(!isString(substr)) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = asString(callee)->str;
        auto pos = str.find(asString(substr)->str);
        auto p = static_cast<double>(pos);
        if(pos == str.npos) p = -1;
        t->push(encodeNumber(p));
    });
    BOUND_NATIVE("last_pos", 1, [](Thread*t, int8_t argCount){
        Value substr = t->pop();
        if(!isString(substr)) TYPE_ERROR("string", 0, substr);
        Value callee = t->pop();
        string str = asString(callee)->str;
        auto pos = str.rfind(asString(substr)->str);
        int32_t p = pos;
        if(pos == str.npos) p = -1;
        t->push(encodeNumber(p));
    });
    BOUND_NATIVE("is_upper", 1, [](Thread*t, int8_t argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeBool(std::isupper(str[decodeNumber(pos)]) != 0));
    });
    BOUND_NATIVE("is_lower", 1, [](Thread*t, int8_t argCount){
        Value pos = t->pop();
        Value callee = t->pop();
        string str = asString(callee)->str;
        strRangeCheck(t, 0, pos, str.length());
        t->push(encodeBool(std::isupper(str[decodeNumber(pos)]) == 0));
    });
    BOUND_NATIVE("to_upper", 0, [](Thread*t, int8_t argCount){
        Value callee = t->pop();
        string str = asString(callee)->str;
        for(char& i : str){
            i = toupper(i);
        }
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("to_lower", 0, [](Thread*t, int8_t argCount){
        Value callee = t->pop();
        string str = asString(callee)->str;
        for(char& i : str){
            i = tolower(i);
        }
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("to_number", 0, [](Thread*t, int8_t argCount){
        t->push(encodeNumber(std::stoi(asString(t->pop())->str)));
    });
    BOUND_NATIVE("split", 1, [](Thread*t, int8_t argCount){
        Value delimiter = t->pop();
        if(!isString(delimiter)) TYPE_ERROR("string", 0, delimiter);
        string baseString = asString(t->pop())->str;
        string delStr = asString(delimiter)->str;
        auto arr = new object::ObjArray();
        uInt64 pos = baseString.npos;
        while((pos = baseString.find(delStr)) != baseString.npos){
            string temp = baseString.substr(0, pos);
            arr->values.push_back(encodeObj(object::ObjString::createStr(temp)));
            MEM_ADD(8);
            baseString.erase(0, pos + delStr.length());
        }
        arr->values.push_back(encodeObj(object::ObjString::createStr(baseString)));
        MEM_ADD(8);
        t->push(encodeObj(arr));
    });
    // Array
    ADD_CLASS("array");
    BOUND_NATIVE("push", 1, [](Thread*t, int8_t argCount){
        MEM_ADD(sizeof(Value));
        asArray(INLINE_PEEK(1))->values.push_back(INLINE_POP());
    });
    BOUND_NATIVE("pop", 0, [](Thread*t, int8_t argCount){
        auto arr = asArray(t->pop());
        Value val = arr->values.back();
        MEM_ADD(-sizeof(Value));
        arr->values.pop_back();
        t->push(val);
    });
    BOUND_NATIVE("copy", 0, [](Thread*t, int8_t argCount){
        auto arr = asArray(t->pop());
        auto newArr = new object::ObjArray();
        newArr->values = arr->values;
        MEM_ADD(sizeof(Value)*newArr->values.size());
        t->push(encodeObj(newArr));
    });
    BOUND_NATIVE("resize", -1, [](Thread*t, int8_t argCount){
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
        auto& arr = asArray(t->peek(0))->values;
        MEM_ADD(sizeof(Value)*(s - arr.size()));
        arr.resize(s, fill);
    });
    BOUND_NATIVE("length", 0, [](Thread*t, int8_t argCount){
        auto arr = asArray(t->pop());
        t->push(encodeNumber(arr->values.size()));
    });
    BOUND_NATIVE("insert", 2, [](Thread*t, int8_t argCount){
        Value val = t->pop();
        Value index = t->pop();
        auto arr = asArray(t->peek(0));
        isNumAndInt(t, index, 0);
        int32_t ind = decodeInt(index);
        if(ind < 0 || ind > arr->values.size())
            t->runtimeError(fmt::format("Index {} outside of range [0, {}]", ind, arr->values.size()), 3);

        arr->values.insert(arr->values.begin() + ind, val);
        MEM_ADD(sizeof(Value));
    });
    BOUND_NATIVE("erase", 2, [](Thread*t, int8_t argCount){
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
        MEM_ADD(-sizeof(Value));
    });
    BOUND_NATIVE("concat", 1, [](Thread*t, int8_t argCount){
        Value other = t->pop();
        if(!isArray(other)) TYPE_ERROR("array", 0, other);
        if(t->peek(0) == other) t->runtimeError("Cannot concat array to itself.", 3);
        auto& arr1 = asArray(t->peek(0))->values;
        auto& arr2 = asArray(other)->values;
        arr1.insert(arr1.end(), arr2.begin(), arr2.end());
        MEM_ADD(sizeof(Value) * arr2.size());
    });
    BOUND_NATIVE("reverse", 0, [](Thread*t, int8_t argCount){
        auto arr = asArray(t->peek(0));
        std::reverse(arr->values.begin(), arr->values.end());
    });
    BOUND_NATIVE("equals", 1, [](Thread*t, int8_t argCount){
        Value other = t->pop();
        if(!isArray(other)) TYPE_ERROR("array", 0, other);
        auto& arr1 = asArray(t->pop())->values;
        auto& arr2 = asArray(other)->values;
        if(arr1.size() != arr2.size()){
            t->push(encodeBool(false));
            return;
        }
        for(uInt i = 0; i < arr1.size(); i++){
            if(!equals(arr1[i], arr2[i])) {
                t->push(encodeBool(false));
                return;
            }
        }
        t->push(encodeBool(true));
    });

    // File
    ADD_CLASS("file");
    BOUND_NATIVE("open_read", 0, [](Thread*t, int8_t argCount){
        auto file = asFile(t->peek(0));
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 0;
    });
    BOUND_NATIVE("open_write", 0, [](Thread*t, int8_t argCount){
        auto file = asFile(t->peek(0));
        std::fstream& stream = file->stream;
        if(stream.is_open()) t->runtimeError("Trying to open a file that is already opened", 8);
        stream.open(file->path);
        file->openType = 1;
    });
    BOUND_NATIVE("is_open_read", 0, [](Thread*t, int8_t argCount){
        auto file = asFile(t->pop());
        std::fstream& stream = file->stream;
        t->push(Value(stream.is_open() && file->openType == 0));
    });
    BOUND_NATIVE("is_open_write", 0, [](Thread*t, int8_t argCount){
        auto file = asFile(t->pop());
        std::fstream& stream = file->stream;
        t->push(Value(stream.is_open() && file->openType == 1));
    });
    BOUND_NATIVE("close", 0, [](Thread*t, int8_t argCount){
        std::fstream& stream = asFile(t->peek(0))->stream;
        if(!stream.is_open()) t->runtimeError("Trying to close a file that isn't opened", 8);
        stream.close();
    });
    BOUND_NATIVE("path", 0, [](Thread*t, int8_t argCount){
        t->push(encodeObj(object::ObjString::createStr(asFile(t->pop())->path)));
    });
    BOUND_NATIVE("readln", 0, [](Thread*t, int8_t argCount){
        auto f = asFile(t->pop());
        if(f->openType != 0) t->runtimeError("File open for reading, not writing.", 8);
        std::fstream& stream = f->stream;
        string str;
        std::getline(stream, str);
        t->push(encodeObj(object::ObjString::createStr(str)));
    });
    BOUND_NATIVE("write", 1, [](Thread*t, int8_t argCount){
        Value str = t->pop();
        if(!isString(str)) TYPE_ERROR("string", 0, str);
        auto f = asFile(t->peek(0));
        if(f->openType != 1) t->runtimeError("File open for writing, not reading.", 8);
        std::fstream& stream =f->stream;
        stream << asString(str)->str;
    });

    // Mutex
    ADD_CLASS("mutex");
    BOUND_NATIVE("exclusive_lock", 0, [](Thread*t, int8_t argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        asMutex(mutex)->mtx.lock();
        decThreadWait(t);
        t->push(encodeNil());
    });
    BOUND_NATIVE("try_exclusive_lock", 0, [](Thread*t, int8_t argCount){
        Value mutex = t->pop();
        // Try_lock doesn't block
        bool res = asMutex(mutex)->mtx.try_lock();
        t->push(encodeBool(res));
    });
    BOUND_NATIVE("shared_lock", 0, [](Thread*t, int8_t argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        incThreadWait(t);
        asMutex(mutex)->mtx.lock_shared();
        decThreadWait(t);
        t->push(encodeNil());
    });
    BOUND_NATIVE("try_shared_lock", 0, [](Thread*t, int8_t argCount){
        Value mutex = t->pop();
        // Try_lock_shared doesn't block
        asMutex(mutex)->mtx.try_lock_shared();;
        t->push(encodeNil());
    });
    BOUND_NATIVE("unlock", 0, [](Thread*t, int8_t argCount){
        Value mutex = t->pop();
        // If this thread is waiting for a mutex, it can be considered paused and a GC can run
        asMutex(mutex)->mtx.unlock();
        t->push(encodeNil());
    });
    // Future
    ADD_CLASS("future");
    BOUND_NATIVE("cancel", 0, [](Thread*t, int8_t argCount){
        auto fut = asFuture(t->pop());
        fut->thread->cancelToken.store(true, std::memory_order_relaxed);
        fut->thread->pauseToken.store(true, std::memory_order_relaxed);
        t->push(encodeNil());
    });
    BOUND_NATIVE("is_done", 0, [](Thread*t, int8_t argCount){
        auto fut = asFuture(t->pop());
        auto done = fut->fut.wait_until(std::chrono::system_clock::time_point::min());
        t->push(encodeBool(!(done == std::future_status::timeout)));
    });
    return classes;
}
#undef BOUND_NATIVE
#undef NATIVE_FUNC