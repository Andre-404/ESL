#include "objects.h"
#include "../../Includes/fmt/format.h"
#include "../Values/valueHelpersInline.cpp"
#include "../MemoryManagment/garbageCollector.h"
#include "../../Includes/rapidhash.h"

using namespace object;
using namespace memory;
using namespace valueHelpers;

#pragma region Obj
size_t Obj::getSize(){
    switch(type){
        case +ObjType::STRING: return sizeof(ObjString) + ((ObjString*)this)->size;
        case +ObjType::ARRAY: return sizeof(ObjArray);
        case +ObjType::CLOSURE: return sizeof(ObjClosure);
        case +ObjType::FREEVAR: return sizeof(ObjFreevar);
        case +ObjType::CLASS: return sizeof(ObjClass);
        case +ObjType::INSTANCE: return sizeof(ObjInstance) + ((ObjInstance*)this)->fieldArrLen*sizeof(Value);
        case +ObjType::HASH_MAP: return sizeof(ObjHashMap);
        case +ObjType::FILE: return sizeof(ObjFile);
        case +ObjType::MUTEX: return sizeof(ObjMutex);
        case +ObjType::FUTURE: return sizeof(ObjFuture);
        case +ObjType::RANGE: return sizeof(ObjRange);
        default: std::cout<<"getsize called with nonvalid obj type\n";
    }
}

void object::runObjDestructor(object::Obj* obj){
    // Have to do this because we don't have access to virtual destructors,
    // however some objects allocate STL containers that need cleaning up
    obj->GCInfo[1] = 0; // Reset allocated bit
    switch(obj->type){
        case +object::ObjType::DEALLOCATED: return;
        case +object::ObjType::ARRAY: reinterpret_cast<object::ObjArray*>(obj)->~ObjArray(); break;
        case +object::ObjType::FILE: reinterpret_cast<object::ObjFile*>(obj)->~ObjFile(); break;
        case +object::ObjType::FUTURE: reinterpret_cast<object::ObjFuture*>(obj)->~ObjFuture(); break;
        case +object::ObjType::HASH_MAP: reinterpret_cast<object::ObjHashMap*>(obj)->~ObjHashMap(); break;
        case +object::ObjType::MUTEX: reinterpret_cast<object::ObjMutex*>(obj)->~ObjMutex(); break;
        default: break;
    }
    // Makes sure to not dealloc twice by setting the type
    obj->type = +object::ObjType::DEALLOCATED;
}

string Obj::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack){
    switch(type){
        case +ObjType::STRING: return string(reinterpret_cast<ObjString*>(this)->str);
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
        case +ObjType::FREEVAR: return "<freevar>";
        case +ObjType::CLASS: return "<class " + string(reinterpret_cast<ObjClass*>(this)->name) + ">";
        case +ObjType::INSTANCE: return "<" + string(reinterpret_cast<ObjInstance*>(this)->klass->name) + " instance>";
        case +ObjType::HASH_MAP:{
            ObjHashMap* map = reinterpret_cast<ObjHashMap*>(this);
            string str = "{";
            for(auto it : map->fields){
                str.append(" \"").append(string(it.first->str)).append("\" : ");
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

void* Obj::operator new(const size_t size) {
    return memory::getLocalArena().alloc(size);
}
#pragma endregion

#pragma region ObjString
// Never called
ObjString::ObjString() {
	type = +ObjType::STRING;
}

bool ObjString::compare(ObjString* other) {
	return size == other->size && std::strcmp(str, other->str) == 0;
}

bool ObjString::compare(const string other) {
	return std::strcmp(str, other.c_str()) == 0;
}

ObjString* ObjString::concat(ObjString* other) {
    ObjString* newStr = static_cast<ObjString *>(
            memory::getLocalArena().alloc(sizeof(ObjString) + size + other->size +1));
    newStr->str = ((char*)newStr)+sizeof(ObjString);
    newStr->type = +ObjType::STRING;
    newStr->size = size + other->size;

    std::memcpy(newStr->str, str, size);
    std::memcpy(newStr->str + size, other->str, other->size+1);

    return memory::gc->interned.checkInterned(newStr);
}

ObjString* ObjString::createStr(char* str){
    ObjString* newStr = static_cast<ObjString *>(
            memory::getLocalArena().alloc(sizeof(ObjString) + std::strlen(str) +1));
    newStr->str = ((char*)newStr)+sizeof(ObjString);
    newStr->type = +ObjType::STRING;
    newStr->size = std::strlen(str);
    strcpy(newStr->str, str);
    return memory::gc->interned.checkInterned(newStr);
}

uint64_t stringHash::operator()(object::ObjString* str) const noexcept{
    return rapidhash(str->str, str->size);
}
#pragma endregion

#pragma region ObjClosure
ObjClosure::ObjClosure(const Function _func, const int _arity, const char* _name, const int _freevarCount)
    : func(_func), arity(_arity), name(_name), freevarCount(_freevarCount){
    freevars = freevarCount > 0 ? new object::ObjFreevar*[freevarCount] : nullptr;
	type = +ObjType::CLOSURE;
}

ObjClosure::~ObjClosure(){
    delete freevars;
}
#pragma endregion

#pragma region ObjUpval
ObjFreevar::ObjFreevar(const Value& _val) {
	val = _val;
	type = +ObjType::FREEVAR;
}
#pragma endregion

#pragma region ObjArray
ObjArray::ObjArray() {
	type = +ObjType::ARRAY;
	numOfHeapPtr = 0;
}
ObjArray::ObjArray(const size_t size) {
	values.resize(size);
	type = +ObjType::ARRAY;
	numOfHeapPtr = 0;
}
#pragma endregion

#pragma region ObjClass
ObjClass::ObjClass(string _name) {
	name = nullptr;
	type = +ObjType::CLASS;
}
#pragma endregion

#pragma region ObjInstance
ObjInstance::ObjInstance(ObjClass* _klass, uInt _fieldArrLen) {
	klass = _klass;
    fieldArrLen = _fieldArrLen;
	type = +ObjType::INSTANCE;
}

Value* ObjInstance::getFields(){
    return (Value*)(((char*)this)+sizeof(ObjInstance));
}
#pragma endregion

#pragma region ObjHashMap
ObjHashMap::ObjHashMap() {
	type = +ObjType::HASH_MAP;
}
#pragma endregion

#pragma region ObjFile
ObjFile::ObjFile(const string& _path, int _openType) : path(_path) {
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
    // Calling this AFTER setting val, since a gc cycle could potentially be in progress and waiting on this thread to suspend
    // In such a case the gc cycle kicks of immediately after calling removeStackStart
    memory::gc->removeStackStart(std::this_thread::get_id());
}

ObjFuture::ObjFuture(ObjClosure* closure, int argc, Value* args) {
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
    type = +ObjType::RANGE;
}
#pragma endregion