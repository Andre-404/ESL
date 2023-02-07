#include "objects.h"
#include "../MemoryManagment/garbageCollector.h"
#include "../Runtime/thread.h"

using namespace object;
using namespace memory;

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

string ObjString::toString() {
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

string ObjFunc::toString() {
	return "<" + name + ">";
}

uInt64 ObjFunc::getSize() {
	return sizeof(ObjFunc);
}
#pragma endregion

#pragma region ObjNativeFn
ObjNativeFunc::ObjNativeFunc(NativeFn _func, byte _arity) {
	func = _func;
	arity = _arity;
    marked = false;
	type = ObjType::NATIVE;
}

void ObjNativeFunc::trace() {
	//nothing
}

string ObjNativeFunc::toString() {
	return "<native function>";
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

string ObjClosure::toString() {
	return func->toString();
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
	val.mark();
}

string ObjUpval::toString() {
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
		values[i].mark();
		if(values[i].isObj()) temp++;
	}
}

string ObjArray::toString() {
	return "<array>";
}

uInt64 ObjArray::getSize() {
	return sizeof(ObjArray);
}
#pragma endregion

#pragma region ObjClass
ObjClass::ObjClass(string _name) {
	name = _name;
    marked = false;
	type = ObjType::CLASS;
}

void ObjClass::trace() {
	for (auto it = methods.begin(); it != methods.end(); it++) {
		it->second.mark();
	}
}

string ObjClass::toString() {
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
	for (auto it = fields.begin(); it != fields.end(); it++) {
		it->second.mark();
	}
	if(klass) gc.markObj(klass);
}

string ObjInstance::toString() {
	if (!klass) return "<struct>";
	return "<" + klass->name + " instance>";
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
	receiver.mark();
	gc.markObj(method);
}

string ObjBoundMethod::toString() {
	return method->func->toString();
}

uInt64 ObjBoundMethod::getSize() {
	return sizeof(ObjBoundMethod);
}
#pragma endregion

#pragma region ObjFile
ObjFile::ObjFile(string& _path) {
	path = _path;
    marked = false;
	stream.open(path);
	type = ObjType::FILE;
}
ObjFile::~ObjFile() {
	stream.close();
}

void ObjFile::trace() {
	//nothing
}

string ObjFile::toString() {
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

string ObjMutex::toString() {
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
	val = Value::nil();
	type = ObjType::FUTURE;
}
ObjFuture::~ObjFuture() {

}

void ObjFuture::startParallelExecution() {
	fut = std::async(std::launch::async, &runtime::Thread::executeBytecode, thread);
}

void ObjFuture::trace() {
	//when tracing all threads other than the main one are suspended, so there's no way for anything to write to val
	val.mark();
}

string ObjFuture::toString() {
	return "<future>";
}

uInt64 ObjFuture::getSize() {
	return sizeof(ObjFuture);
}
#pragma endregion

