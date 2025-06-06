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
        case +ObjType::ARRAY_STORAGE_HEADER: return sizeof(ObjArrayStorage) + ((ObjArrayStorage*)this)->capacity*sizeof(Value);
        case +ObjType::CLOSURE: return sizeof(ObjClosure);
        case +ObjType::FREEVAR: return sizeof(ObjFreevar);
        case +ObjType::CLASS: return sizeof(ObjClass);
        case +ObjType::INSTANCE: return sizeof(ObjInstance) + ((ObjInstance*)this)->fieldArrLen*sizeof(Value);
        case +ObjType::HASH_MAP: return sizeof(ObjHashMap);
        case +ObjType::FILE: return sizeof(ObjFile);
        case +ObjType::MUTEX: return sizeof(ObjMutex);
        default: std::cout<<"getsize called with nonvalid obj type\n";
    }
    __builtin_unreachable();
}

void object::runObjDestructor(object::Obj* obj){
    // Have to do this because we don't have access to virtual destructors,
    // however some objects allocate STL containers that need cleaning up
    obj->GCInfo[1] = 0; // Reset allocated bit
    switch(obj->type){
        case +object::ObjType::DEALLOCATED: return;
        case +object::ObjType::ARRAY: reinterpret_cast<object::ObjArray*>(obj)->~ObjArray(); break;
        case +object::ObjType::FILE: reinterpret_cast<object::ObjFile*>(obj)->~ObjFile(); break;
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
            for(int i = 0; i < arr->size; i++){
                str.append(" " + valueHelpers::toString(arr->getData()[i], stack)).append(",");
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
    }
    return "cannot stringfy object";
}

void* Obj::operator new(const size_t size, memory::ThreadArena& allocator) {
    return allocator.alloc(size);
}
#pragma endregion

#pragma region ObjString
bool ObjString::compare(ObjString* other) {
	return size == other->size && std::strcmp(str, other->str) == 0;
}

bool ObjString::compare(const string other) {
	return std::strcmp(str, other.c_str()) == 0;
}

ObjString* ObjString::concat(ObjString* other, ThreadArena& allocator) {
    ObjString* newStr = static_cast<ObjString *>(allocator.alloc(sizeof(ObjString) + size + other->size +1));
    newStr->str = ((char*)newStr)+sizeof(ObjString);
    newStr->type = +ObjType::STRING;
    newStr->size = size + other->size;

    std::memcpy(newStr->str, str, size);
    std::memcpy(newStr->str + size, other->str, other->size+1);

    return memory::gc->interned.checkInterned(newStr);
}

ObjString* ObjString::createStr(char* str, memory::ThreadArena& allocator){
    ObjString* newStr = static_cast<ObjString *>(allocator.alloc(sizeof(ObjString) + std::strlen(str) +1));
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
ObjFreevar** ObjClosure::getFreevarArr(){
    return (ObjFreevar**)(((char*)this)+sizeof(ObjClosure));
}
#pragma endregion

#pragma region ObjArray

Value* ObjArrayStorage::getData(){
    return (Value*)(((char*)this)+sizeof(ObjArrayStorage));
}

ObjArrayStorage* ObjArrayStorage::allocArray(uint32_t desiredSize, memory::ThreadArena& allocator){
    uint64_t capacity = std::bit_ceil(static_cast<uint64_t>(desiredSize));
    if(capacity > (1 << 31)){
        // TODO: error
    }
    ObjArrayStorage* store = static_cast<ObjArrayStorage *>(allocator.alloc(
            sizeof(ObjArrayStorage) + capacity * sizeof(Value)));
    store->type = +ObjType::ARRAY_STORAGE_HEADER;
    store->capacity = capacity;
    return store;
}

ObjArray::ObjArray(memory::ThreadArena& allocator) {
    containsObjects = 0;
    storage = ObjArrayStorage::allocArray(8, allocator);
    size = 0;
	type = +ObjType::ARRAY;
}
ObjArray::ObjArray(const size_t _size, memory::ThreadArena& allocator) {
    containsObjects = 0;
    size = _size;
    storage = ObjArrayStorage::allocArray(size, allocator);
	type = +ObjType::ARRAY;
}

Value* ObjArray::getData(){
    return storage->getData();
}
void ObjArray::push(Value item, memory::ThreadArena& allocator){
    if(size == storage->capacity){
        ObjArrayStorage* newStorage = ObjArrayStorage::allocArray(storage->capacity+1, allocator);
        memcpy(newStorage->getData(), storage->getData(), size*sizeof(Value));
        storage = newStorage;
    }
    getData()[size++] = item;
}
#pragma endregion

ObjFreevar::ObjFreevar(Value val){
    this->val = val;
    type = +ObjType::FREEVAR;
}

#pragma region ObjClass
ObjClass::ObjClass(string _name) {
	name = nullptr;
	type = +ObjType::CLASS;
}
#pragma endregion

#pragma region ObjInstance
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