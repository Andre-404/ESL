#include "objects.h"
#include "../MemoryManagment/garbageCollector.h"
#include "../Includes/fmt/format.h"
#include "../Codegen/valueHelpersInline.cpp"

using namespace object;
using namespace memory;
using namespace valueHelpers;

#pragma region Obj
size_t Obj::getSize(){
    switch(type){
        case +ObjType::STRING: return sizeof(ObjString);
        case +ObjType::FUNC: return sizeof(ObjFunc);
        case +ObjType::ARRAY: return sizeof(ObjArray);
        case +ObjType::CLOSURE: return sizeof(ObjClosure);
        case +ObjType::FREEVAR: return sizeof(ObjFreevar);
        case +ObjType::CLASS: return sizeof(ObjClass);
        case +ObjType::INSTANCE: return sizeof(ObjInstance);
        case +ObjType::BOUND_METHOD: return sizeof(ObjBoundMethod);
        case +ObjType::HASH_MAP: return sizeof(ObjHashMap);
        case +ObjType::FILE: return sizeof(ObjFile);
        case +ObjType::MUTEX: return sizeof(ObjMutex);
        case +ObjType::FUTURE: return sizeof(ObjFuture);
        case +ObjType::RANGE: return sizeof(ObjRange);
    }
}

string Obj::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack){
    switch(type){
        case +ObjType::STRING: return reinterpret_cast<ObjString*>(this)->str;
       // case +ObjType::FUNC: return "<" + reinterpret_cast<ObjFunc*>(this)->name + ">";;
        case +ObjType::ARRAY:{
            ObjArray* arr = reinterpret_cast<ObjArray*>(this);
            string str = "[";
            for(Value& val : arr->values){
                str.append(" " + valueHelpers::toString(val, stack)).append(",");
            }
            str.erase(str.size() - 1).append(" ]");
            return str;
        }
        case +ObjType::CLOSURE: return "<" + string(reinterpret_cast<ObjClosure*>(this)->name) + ">";;
        case +ObjType::FREEVAR: return "<upvalue>";
        case +ObjType::CLASS: return "<class " + reinterpret_cast<ObjClass*>(this)->name->str + ">";
        case +ObjType::INSTANCE: return "<" + reinterpret_cast<ObjInstance*>(this)->klass->name->str + " instance>";
        case +ObjType::BOUND_METHOD: return "<bound method>";
        case +ObjType::HASH_MAP:{
            ObjHashMap* map = reinterpret_cast<ObjHashMap*>(this);
            string str = "{";
            for(auto it : map->fields){
                str.append(" \"").append(it.first->str).append("\" : ");
                str.append(valueHelpers::toString(it.second, stack)).append(",");
            }
            str.erase(str.size() - 1).append(" }");
            return str;
        }
        case +ObjType::FILE: return "<file>";
        case +ObjType::MUTEX: return "<mutex>";;
        case +ObjType::FUTURE: return "<future>";
        case +ObjType::RANGE: {
            ObjRange* range = reinterpret_cast<ObjRange*>(this);
            return fmt::format("{}..{}{}", range->start, range->isEndInclusive ? "=" : "", range->end);
        }
    }
    return "cannot stringfy object";
}
#pragma endregion

#pragma region ObjString
ObjString::ObjString(const string& _str) {
	str = _str;
    marked = false;
	type = +ObjType::STRING;
}

bool ObjString::compare(const ObjString* other) {
	return (str.compare(other->str) == 0);
}

bool ObjString::compare(const string other) {
	return (str.compare(other) == 0);
}

ObjString* ObjString::concat(const ObjString* other) {
	string temp = str + other->str;
	return new ObjString(temp);
}

string convertBackSlashToEscape(const std::string& input)
{
    string output;
    auto isEscapeChar = [](char c){
        return c == 'n' || c == 'r' || c == 't' || c == 'a' || c == 'b' || c == 'f' || c == 'v';
    };
    for (int i = 0; i < input.length(); i++) {
        if (input[i] == '\\' && i < input.length() - 1 && isEscapeChar(input[i+1])) {
            // replace \\n with \n, \\r with \r, and \\t with \t
            switch (input[i+1]) {
                case 'n': output += '\n'; break;
                case 'r': output += '\r'; break;
                case 't': output += '\t'; break;
                case 'a': output += '\a'; break;
                case 'b': output += '\b'; break;
                case 'f': output += '\f'; break;
                case 'v': output += '\v'; break;
            }
            i++; // skip the next character
        } else {
            // copy the current character
            output += input[i];
        }
    }
    return output;
}

ObjString* ObjString::createStr(string str){
    str = convertBackSlashToEscape(str);
    auto it = memory::gc->interned.find(str);
    if(it != memory::gc->interned.end()) return it->second;
    auto newStr = new ObjString(str);
    memory::gc->heapSize += str.size();
    memory::gc->interned[str] = newStr;
    return newStr;
}
#pragma endregion

#pragma region ObjFunction
ObjFunc::ObjFunc(const Function _func, const int _arity, const char* _name)
    : func(_func), arity(_arity), name(_name) {
	type = +ObjType::FUNC;
    marked = false;
}
#pragma endregion

#pragma region ObjClosure
ObjClosure::ObjClosure(const Function _func, const int _arity, const char* _name, const int _upvalCount)
    : func(_func), arity(_arity), name(_name), upvalCount(_upvalCount){
    upvals = new object::ObjFreevar*[upvalCount];
    marked = false;
	type = +ObjType::CLOSURE;
}

ObjClosure::~ObjClosure(){
    delete upvals;
}
#pragma endregion

#pragma region ObjUpval
ObjFreevar::ObjFreevar(const Value& _val) {
	val = _val;
    marked = false;
	type = +ObjType::FREEVAR;
}
#pragma endregion

#pragma region ObjArray
ObjArray::ObjArray() {
	type = +ObjType::ARRAY;
	numOfHeapPtr = 0;
    marked = false;
}
ObjArray::ObjArray(const size_t size) {
	values.resize(size);
	type = +ObjType::ARRAY;
	numOfHeapPtr = 0;
    marked = false;
}
#pragma endregion

#pragma region ObjClass
ObjClass::ObjClass(string _name, object::ObjClass* _superclass) {
	name = ObjString::createStr(_name);
    marked = false;
    superclass = _superclass;
	type = +ObjType::CLASS;
}
#pragma endregion

#pragma region ObjInstance
ObjInstance::ObjInstance(ObjClass* _klass) {
	klass = _klass;
	fields = klass->fieldsInit;
    marked = false;
	type = +ObjType::INSTANCE;
}
#pragma endregion

#pragma region ObjHashMap
ObjHashMap::ObjHashMap() {
	marked = false;
	type = +ObjType::HASH_MAP;
}
#pragma endregion

#pragma region ObjBoundMethod
ObjBoundMethod::ObjBoundMethod(const Value _receiver, ObjFunc* _method) : receiver(_receiver) {
	method = _method;
    marked = false;
	type = +ObjType::BOUND_METHOD;
}
#pragma endregion

#pragma region ObjFile
ObjFile::ObjFile(const string& _path, int _openType) : path(_path) {
    marked = false;
    openType = _openType;
	stream.open(path, std::ios::in | std::ios::out);
	type = +ObjType::FILE;
}
ObjFile::~ObjFile() {
	stream.close();
}
#pragma endregion

#pragma region ObjMutex
ObjMutex::ObjMutex() {
    marked = false;
	type = +ObjType::MUTEX;
}
#pragma endregion

#pragma region ObjFuture
// Function that executes on a newly started thread, handles initialization of thread data and cleanup after execution is finished
void threadWrapper(ObjFuture* fut, ObjClosure* closure, int argc, Value* args) {
    uintptr_t* stackStart = getStackPointer();
    memory::gc->addStackStart(std::this_thread::get_id(), stackStart);

    Value encodedFunc = encodeObj(closure);
    Value val = encodeNil();
    // Very, very, VERY ugly way to do this, but I can't think of a better way
    switch(argc){
        case 0: val = reinterpret_cast<Value(*)(Value)>(closure->func)(encodedFunc); break;
        case 1: val = reinterpret_cast<Value(*)(Value, Value)>(closure->func)(encodedFunc, args[0]); break;
        case 2: val = reinterpret_cast<Value(*)(Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1]); break;
        case 3: val = reinterpret_cast<Value(*)(Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2]); break;
        case 4: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3]); break;
        case 5: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4]); break;
        case 6: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5]); break;
        case 7: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6]); break;
        case 8: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]); break;
        case 9: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]); break;
        case 10: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]); break;
        case 11: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]); break;
        case 12: val = reinterpret_cast<Value(*)(Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value)>(closure->func)(encodedFunc, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]); break;
    }
    // If the future is deallocated, then the user doesn't care about the return value of this function
    if(memory::gc->isValidPtr(fut)){
        fut->val = val;
        fut->done = true;
    }
    // Calling this AFTER setting val, since a gc cycle could potentially be in progress and waiting on this thread to suspend
    // In such a case the gc cycle kicks of immediately after calling removeStackStart
    memory::gc->removeStackStart(std::this_thread::get_id());
}

ObjFuture::ObjFuture(ObjClosure* closure, int argc, Value* args) {
    marked = false;
    done = false;
	val = encodeNil();
	type = +ObjType::FUTURE;
    thread = std::jthread(&threadWrapper, this, closure, argc, args);
}
ObjFuture::~ObjFuture() {
    // If this future is deleted and the thread is still active detach the thread and have it run its course
    // joinable check is because the user might have joined this thread using await
    if(thread.joinable()) thread.detach();
}

#pragma endregion

#pragma region ObjRange
ObjRange::ObjRange(const double _start, const double _end, const bool _isEndInclusive)
    : start(_start), end(_end), isEndInclusive(_isEndInclusive){
    marked = false;
    type = +ObjType::RANGE;
}
#pragma endregion