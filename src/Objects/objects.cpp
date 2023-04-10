#include "objects.h"
#include "../MemoryManagment/garbageCollector.h"
#include "../Runtime/thread.h"
#include "../Includes/fmt/format.h"
#include "../codegen/valueHelpersInline.cpp"

using namespace object;
using namespace memory;
using namespace valueHelpers;

#pragma region ObjString
ObjString::ObjString(string& _str) {
	str = _str;
    marked = false;
	type = ObjType::STRING;
}
uInt64 ObjString::getSize() {
	//+1 for terminator byte
	return sizeof(ObjString);
}
void ObjString::trace() {
	//nothing to mark
}

string ObjString::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return str;
}

bool ObjString::compare(ObjString* other) {
	return (str.compare(other->str) == 0);
}

bool ObjString::compare(string other) {
	return (str.compare(other) == 0);
}

ObjString* ObjString::concat(ObjString* other) {
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
    auto it = memory::gc.interned.find(str);
    if(it != memory::gc.interned.end()) return it->second;
    auto newStr = new ObjString(str);
    memory::gc.heapSize += str.size();
    memory::gc.interned[str] = newStr;
    return newStr;
}
#pragma endregion

#pragma region ObjFunction
ObjFunc::ObjFunc() {
	arity = 0;
	upvalueCount = 0;
	bytecodeOffset = 0;
	constantsOffset = 0;
	type = ObjType::FUNC;
	name = "";
    marked = false;
}

void ObjFunc::trace() {
	// Nothing
}

string ObjFunc::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<" + name + ">";
}

uInt64 ObjFunc::getSize() {
	return sizeof(ObjFunc);
}
#pragma endregion

#pragma region ObjNativeFunc
ObjNativeFunc::ObjNativeFunc(NativeFn _func, int8_t _arity, const char* _name) {
	func = _func;
	arity = _arity;
    name = _name;
    marked = false;
	type = ObjType::NATIVE;
}

void ObjNativeFunc::trace() {
	//nothing
}

string ObjNativeFunc::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return fmt::format("<native function {}>", name);
}

uInt64 ObjNativeFunc::getSize() {
	return sizeof(ObjNativeFunc);
}
#pragma endregion

#pragma region ObjClosure
ObjClosure::ObjClosure(ObjFunc* _func) {
	func = _func;
	upvals = vector<ObjUpval*>(func->upvalueCount);
    marked = false;
	type = ObjType::CLOSURE;
}

void ObjClosure::trace() {
	for (ObjUpval* upval : upvals) {
		gc.markObj(upval);
	}
	gc.markObj(func);
}

string ObjClosure::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return func->toString(stack);
}

uInt64 ObjClosure::getSize() {
	return sizeof(ObjClosure);
}
#pragma endregion

#pragma region ObjUpval
ObjUpval::ObjUpval(Value& _val) {
	val = _val;
    marked = false;
	type = ObjType::UPVALUE;
}

void ObjUpval::trace() {
	mark(val);
}

string ObjUpval::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<upvalue>";
}

uInt64 ObjUpval::getSize() {
	return sizeof(ObjUpval);
}
#pragma endregion

#pragma region ObjArray
ObjArray::ObjArray() {
	type = ObjType::ARRAY;
	numOfHeapPtr = 0;
    marked = false;
}
ObjArray::ObjArray(size_t size) {
	values.resize(size);
	type = ObjType::ARRAY;
	numOfHeapPtr = 0;
}

//small optimization: if numOfHeapPtrs is 0 then we don't even scan the array for objects
//and if there are objects we only scan until we find all objects
void ObjArray::trace() {
	int temp = 0;
	int i = 0;
	uInt64 arrSize = values.size();
	while (i < arrSize && temp < numOfHeapPtr) {
		mark(values[i]);
		if(isObj(values[i])) temp++;
	}
}

string ObjArray::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	string str = "[";
    for(Value& val : values){
        str.append(" " + valueHelpers::toString(val, stack)).append(",");
    }
    str.erase(str.size() - 1).append(" ]");
    return str;
}

uInt64 ObjArray::getSize() {
	return sizeof(ObjArray) + sizeof(Value)*values.size();
}
#pragma endregion

#pragma region ObjClass
ObjClass::ObjClass(string _name, object::ObjClass* _superclass) {
	name = ObjString::createStr(_name);
    marked = false;
    superclass = _superclass;
	type = ObjType::CLASS;
}

void ObjClass::trace() {
	for (auto & m : methods) {
        gc.markObj(m.second);
        m.first->marked = true;
	}
    for (auto & f : fieldsInit) {
        f.first->marked = true;
    }
    name->marked = true;
    if(superclass) gc.markObj(superclass);
}

string ObjClass::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<class " + name->str + ">";
}

uInt64 ObjClass::getSize() {
	return sizeof(ObjClass);
}
#pragma endregion

#pragma region ObjInstance
ObjInstance::ObjInstance(ObjClass* _klass) {
	klass = _klass;
	fields = klass->fieldsInit;
    marked = false;
	type = ObjType::INSTANCE;
}

void ObjInstance::trace() {
	for (auto & field : fields) {
        field.first->marked = true;
		mark(field.second);
	}
	if(klass) gc.markObj(klass);
}

string ObjInstance::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<" + klass->name->str + " instance>";
}

uInt64 ObjInstance::getSize() {
	return sizeof(ObjInstance);
}
#pragma endregion

#pragma region ObjHashMap
ObjHashMap::ObjHashMap() {
	marked = false;
	type = ObjType::HASH_MAP;
}

void ObjHashMap::trace() {
	for (auto & field : fields) {
		field.first->marked = true;
		mark(field.second);
	}
}

string ObjHashMap::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	string str = "{";
	for(auto it : fields){
		str.append(" \"").append(it.first->str).append("\" : ");
		str.append(valueHelpers::toString(it.second, stack)).append(",");
	}
	str.erase(str.size() - 1).append(" }");
	return str;
}

uInt64 ObjHashMap::getSize() {
	return sizeof(ObjHashMap);
}
#pragma endregion

#pragma region ObjBoundMethod
ObjBoundMethod::ObjBoundMethod(Value _receiver, Method _method) {
	receiver = _receiver;
	method = _method;
    marked = false;
	type = ObjType::BOUND_METHOD;
}

void ObjBoundMethod::trace() {
	mark(receiver);
	gc.markObj(method);
}

string ObjBoundMethod::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<bound method>";
}

uInt64 ObjBoundMethod::getSize() {
	return sizeof(ObjBoundMethod);
}
#pragma endregion

#pragma region ObjFile
ObjFile::ObjFile(string& _path, int _openType) {
	path = _path;
    marked = false;
    openType = _openType;
	stream.open(path, std::ios::in | std::ios::out);
	type = ObjType::FILE;
}
ObjFile::~ObjFile() {
	stream.close();
}

void ObjFile::trace() {
	//nothing
}

string ObjFile::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<file>";
}

uInt64 ObjFile::getSize() {
	return sizeof(ObjFile);
}
#pragma endregion

#pragma region ObjMutex
ObjMutex::ObjMutex() {
    marked = false;
	type = ObjType::MUTEX;
}
ObjMutex::~ObjMutex() {

}

void ObjMutex::trace() {
	//nothing
}

string ObjMutex::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<mutex>";
}

uInt64 ObjMutex::getSize() {
	return sizeof(ObjMutex);
}
#pragma endregion

#pragma region ObjFuture
ObjFuture::ObjFuture(runtime::Thread* t) {
	thread = t;
    marked = false;
	val = encodeNil();
	type = ObjType::FUTURE;
}
ObjFuture::~ObjFuture() {

}

void ObjFuture::startParallelExecution() {
	fut = std::async(std::launch::async, &runtime::Thread::executeBytecode, thread);
}

void ObjFuture::trace() {
	//when tracing all threads other than the main one are suspended, so there's no way for anything to write to val
	mark(val);
}

string ObjFuture::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
	return "<future>";
}

uInt64 ObjFuture::getSize() {
	return sizeof(ObjFuture);
}
#pragma endregion

#pragma region ObjRange
ObjRange::ObjRange(double _start, double _end, bool _isEndInclusive) {
    start = _start;
    end = _end;
    isEndInclusive = _isEndInclusive;
    marked = false;
    type = ObjType::RANGE;
}
ObjRange::~ObjRange() {

}

void ObjRange::trace() {

}

string ObjRange::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack) {
    return fmt::format("{}..{}{}", start, isEndInclusive ? "=" : "", end);
}

uInt64 ObjRange::getSize() {
    return sizeof(ObjRange);
}
#pragma endregion