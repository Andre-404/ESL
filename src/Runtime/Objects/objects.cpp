#include "objects.h"
#include "../../Includes/fmt/format.h"
#include "../Values/valueHelpersInline.cpp"
#include "../../Includes/rapidhash.h"
#include "string-interner.h"

using namespace object;
using namespace valueHelpers;

#pragma region Obj
size_t Obj::getSize(){
    switch(type()){
        case ObjType::STRING: return sizeof(ObjString) + ((ObjString*)this)->size;
        case ObjType::ARRAY: return sizeof(ObjArray);
        case ObjType::ARRAY_STORAGE_HEADER: return sizeof(ObjArrayStorage) + ((ObjArrayStorage*)this)->capacity*sizeof(Value);
        case ObjType::CLOSURE: return sizeof(ObjClosure) + ((ObjClosure*)this)->freevarCount*sizeof(Value);
        case ObjType::CLASS: return sizeof(ObjClass);
        case ObjType::INSTANCE: return sizeof(ObjInstance) + ((ObjInstance*)this)->fieldArrLen*sizeof(Value);
        case ObjType::HASH_MAP: return sizeof(ObjHashMap);
        case ObjType::FILE: return sizeof(ObjFile);
        case ObjType::MUTEX: return sizeof(ObjMutex);
        default: std::cout<<"getsize called with nonvalid obj type\n";
    }
    __builtin_unreachable();
}

string Obj::toString(std::shared_ptr<ankerl::unordered_dense::set<object::Obj*>> stack){
    switch(type()){
        case ObjType::STRING: return string(reinterpret_cast<ObjString*>(this)->get_str());
        case ObjType::ARRAY:{
            auto arr = reinterpret_cast<ObjArray*>(this);
            string str = "[";
            for(int i = 0; i < arr->size; i++){
                str.append(" " + valueHelpers::toString(arr->getData()[i], stack)).append(",");
            }
            str.erase(str.size() - 1).append(" ]");
            return str;
        }
        case ObjType::CLOSURE: return "<" + string(reinterpret_cast<ObjClosure*>(this)->name) + ">";
        case ObjType::CLASS: return "<class " + string(reinterpret_cast<ObjClass*>(this)->name) + ">";
        case ObjType::INSTANCE: return "<" + string(reinterpret_cast<ObjInstance*>(this)->klass->name) + " instance>";
        case ObjType::HASH_MAP:{
            auto map = reinterpret_cast<ObjHashMap*>(this);
            string str = "{";
            for(auto it : map->fields){
                str.append(" \"").append(string(it.first->get_str())).append("\" : ");
                str.append(valueHelpers::toString(it.second, stack)).append(",");
            }
            str.erase(str.size() - 1).append(" }");
            return str;
        }
        case ObjType::FILE: return "<file>";
        case ObjType::MUTEX: return "<mutex>";
        default: break;
    }
    return "cannot stringfy object";
}
#pragma endregion

#pragma region ObjString
bool ObjString::compare(ObjString* other) {
	return size == other->size && std::strcmp(get_str(), other->get_str()) == 0;
}

bool ObjString::compare(const string other) {
	return std::strcmp(get_str(), other.c_str()) == 0;
}

ObjString* ObjString::concat(ObjString* other) {
    auto ptr = gc::allocate(sizeof(ObjString) + size + other->size +1);
    auto newStr = new(ptr) ObjString {};
    newStr->size = size + other->size;

    std::memcpy(newStr->get_str(), get_str(), size);
    std::memcpy(newStr->get_str() + size, other->get_str(), other->size+1);

    return object::string_interner::get().check_interned(newStr);
}

ObjString* ObjString::createStr(char* str){
    auto ptr = gc::allocate(sizeof(ObjString) + std::strlen(str) +1);
    auto newStr = new(ptr) ObjString {};
    newStr->size = std::strlen(str);
    strcpy(newStr->get_str(), str);
    return object::string_interner::get().check_interned(newStr);
}

uint64_t stringHash::operator()(const ObjString* str) const noexcept{
    return rapidhash(str->get_str(), str->size);
}
#pragma endregion

#pragma region ObjClosure
Value* ObjClosure::getFreevarArr(){
    return reinterpret_cast<Value *>(this + 1);
}
#pragma endregion

#pragma region ObjArray

Value* ObjArrayStorage::getData(){
    return (Value*)(((char*)this)+sizeof(ObjArrayStorage));
}

ObjArrayStorage* ObjArrayStorage::allocArray(uint32_t desiredSize){
    auto capacity = std::bit_ceil(static_cast<uint64_t>(desiredSize));
    if(capacity > (1ull << 31)){
        // TODO: error
    }
    auto ptr = gc::allocate(sizeof(ObjArrayStorage) + capacity * sizeof(Value));
    auto store = new(ptr) ObjArrayStorage {};
    store->capacity = capacity;
    return store;
}

ObjArray::ObjArray() : Obj(ObjType::ARRAY, false) {
    containsObjects = 0;
    size = 0;
    storage = ObjArrayStorage::allocArray(8);
    gc::write_b(storage);
}
ObjArray::ObjArray(const size_t _size) : Obj(ObjType::ARRAY, false) {
    containsObjects = 0;
    size = _size;
    storage = ObjArrayStorage::allocArray(size);
    gc::write_b(storage);
}

Value* ObjArray::getData(){
    return storage->getData();
}
void ObjArray::push(Value item){
    if(size == storage->capacity){
        ObjArrayStorage* newStorage = ObjArrayStorage::allocArray(storage->capacity+1);
        memcpy(newStorage->getData(), storage->getData(), size*sizeof(Value));
        storage = newStorage;
        gc::write_b(storage);
    }
    getData()[size++] = item;
    if (isObj(item)) {
        containsObjects = 1;
        gc::write_b(decodeObj(item));
    }
}
#pragma endregion

#pragma region ObjInstance
Value* ObjInstance::getFields(){
    return (Value*)(((char*)this)+sizeof(ObjInstance));
}
#pragma endregion

#pragma region ObjHashMap
ObjHashMap::ObjHashMap() : Obj(ObjType::HASH_MAP, false) {}
#pragma endregion

#pragma region ObjFile
ObjFile::ObjFile(string& _path, int _openType) : Obj(ObjType::FILE, true), path(_path) {
    openType = _openType;
	stream.open(path, std::ios::in | std::ios::out);
}
ObjFile::~ObjFile() {
	stream.close();
}
#pragma endregion