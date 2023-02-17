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

string ObjString::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
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

string ObjFunc::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	return "<" + name + ">";
}

uInt64 ObjFunc::getSize() {
	return sizeof(ObjFunc);
}
#pragma endregion

#pragma region ObjNativeFunc
ObjNativeFunc::ObjNativeFunc(NativeFn _func, int _arity, string _name) {
	func = _func;
	arity = _arity;
    name = _name;
    marked = false;
	type = ObjType::NATIVE;
}

void ObjNativeFunc::trace() {
	//nothing
}

string ObjNativeFunc::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	return fmt::format("<native function {}>", name);
}

uInt64 ObjNativeFunc::getSize() {
	return sizeof(ObjNativeFunc);
}
#pragma endregion

#pragma region ObjBoundNativeFunc
ObjBoundNativeFunc::ObjBoundNativeFunc(NativeFn _func, int _arity, string _name, Value& _receiver) {
    func = _func;
    arity = _arity;
    name = _name;
    receiver = _receiver;
    marked = false;
    type = ObjType::BOUND_NATIVE;
}

void ObjBoundNativeFunc::trace() {
    mark(receiver);
}

string ObjBoundNativeFunc::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
    return fmt::format("<native function {}>", name);
}

uInt64 ObjBoundNativeFunc::getSize() {
    return sizeof(ObjBoundNativeFunc);
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

string ObjClosure::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
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

string ObjUpval::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
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
	values = vector<Value>(size);
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

string ObjArray::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	string str = "[";
    for(Value& val : values){
        str.append(" " + valueHelpers::toString(val, stack)).append(",");
    }
    str.erase(str.size() - 1).append(" ]");
    return str;
}

uInt64 ObjArray::getSize() {
	return sizeof(ObjArray);
}
#pragma endregion

#pragma region ObjClass
ObjClass::ObjClass(string _name) {
	name = std::move(_name);
    marked = false;
	type = ObjType::CLASS;
}

void ObjClass::trace() {
	for (auto & method : methods) {
		mark(method.second);
	}
}

string ObjClass::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	return "<class " + name + ">";
}

uInt64 ObjClass::getSize() {
	return sizeof(ObjClass);
}
#pragma endregion

#pragma region ObjInstance
ObjInstance::ObjInstance(ObjClass* _klass) {
	klass = _klass;
    marked = false;
	type = ObjType::INSTANCE;
}

void ObjInstance::trace() {
	for (auto & field : fields) {
		mark(field.second);
	}
	if(klass) gc.markObj(klass);
}

string ObjInstance::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	if(klass) "<" + klass->name + " instance>";
    string str = "{";
    for(auto it : fields){
        str.append(" \"").append(it.first).append("\" : ");
        str.append(valueHelpers::toString(it.second, stack)).append(",");
    }
    str.erase(str.size() - 1).append(" }");
    return str;
}

uInt64 ObjInstance::getSize() {
	return sizeof(ObjInstance);
}
#pragma endregion

#pragma region ObjBoundMethod
ObjBoundMethod::ObjBoundMethod(Value _receiver, ObjClosure* _method) {
	receiver = _receiver;
	method = _method;
    marked = false;
	type = ObjType::BOUND_METHOD;
}

void ObjBoundMethod::trace() {
	mark(receiver);
	gc.markObj(method);
}

string ObjBoundMethod::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	return method->func->toString(stack);
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

string ObjFile::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
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

string ObjMutex::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
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

string ObjFuture::toString(std::shared_ptr<robin_hood::unordered_set<object::Obj*>> stack) {
	return "<future>";
}

uInt64 ObjFuture::getSize() {
	return sizeof(ObjFuture);
}
#pragma endregion

