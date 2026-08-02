#include "objects.h"
#include "../Values/valueHelpersInline.h"
#include "../../Includes/rapidhash.h"
#include "string-interner.h"

using namespace object;
using namespace valueHelpers;

#pragma region Obj
size_t rt_obj::get_sz(){
    switch(type()){
        case rt_type::STRING: return sizeof(rt_string) + ((rt_string*)this)->sz();
        case rt_type::ARRAY: return sizeof(rt_arr);
        case rt_type::ARRAY_STORAGE_HEADER: return sizeof(rt_arr_store) + ((rt_arr_store*)this)->get_data().size() * sizeof(Value);
        case rt_type::CLOSURE: return sizeof(rt_closure) + ((rt_closure*)this)->get_env().size()*sizeof(Value);
        case rt_type::INSTANCE: return sizeof(rt_inst) + ((rt_inst*)this)->get_fields().size()*sizeof(Value);
        case rt_type::HASH_MAP: return sizeof(rt_hashmap);
        default: std::cout<<"getsize called with nonvalid obj type\n";
    }
    __builtin_unreachable();
}

string rt_obj::to_str(std::shared_ptr<ankerl::unordered_dense::set<object::rt_obj*>> stack){
    switch(type()){
        case rt_type::STRING: return string(reinterpret_cast<rt_string*>(this)->get_str());
        case rt_type::ARRAY:{
            auto arr = reinterpret_cast<rt_arr*>(this);
            string str = "[";
            for (auto val : arr->get_data())
                str.append(" " + valueHelpers::toString(val, stack)).append(",");
            str.erase(str.size() - 1).append(" ]");
            return str;
        }
        case rt_type::CLOSURE: return "<" + string(reinterpret_cast<rt_closure*>(this)->name()) + ">";
        case rt_type::INSTANCE: return "<" + string(reinterpret_cast<rt_inst*>(this)->get_class()->name) + " instance>";
        /*case rt_type::HASH_MAP:{
            auto map = reinterpret_cast<rt_hashmap*>(this);
            string str = "{";
            for(auto it : map->fields){
                str.append(" \"").append(string(it.first->get_str())).append("\" : ");
                str.append(valueHelpers::toString(it.second, stack)).append(",");
            }
            str.erase(str.size() - 1).append(" }");
            return str;
        }*/
        default: break;
    }
    return "cannot stringfy object";
}
#pragma endregion

#pragma region ObjString
bool rt_string::compare(rt_string* other) {
	return _size == other->_size && std::strcmp(get_str(), other->get_str()) == 0;
}

bool rt_string::compare(const string other) {
	return std::strcmp(get_str(), other.c_str()) == 0;
}

rt_string* rt_string::concat(rt_string* other) {
    auto ptr = gc::esl_make_gc<rt_string>(_size + other->_size + 1, _size + other->_size, get_str());

    std::memcpy(ptr->get_str() + _size, other->get_str(), other->_size+1);

    return object::string_interner::get().check_interned(ptr);
}

rt_string* rt_string::create(char* str){
    auto ptr = gc::esl_make_gc<rt_string>(std::strlen(str) + 1, std::strlen(str), str);
    return object::string_interner::get().check_interned(ptr);
}

uint64_t rt_string::hash::operator()(const rt_string* str) const noexcept{
    return rapidhash(str->get_str(), str->_size);
}
#pragma endregion

#pragma region ObjArray

rt_arr_store::rt_arr_store(uint32_t cap, std::span<Value> init)
    : rt_obj(rt_type::ARRAY_STORAGE_HEADER, false), _contains_obj(0), _capacity(cap) {
    auto s = get_data();
    bool found = false;
    for (size_t i = 0; i < init.size(); i++) {
        s[i] = init[i];
        if (isObj(init[i])) {
            found = true;
            gc::write_b(decodeObj(init[i]));
        }
    }
    memset(&s[init.size()], 0, (cap - init.size())*sizeof(Value));
    if (found) _contains_obj = 1;
}


rt_arr_store* rt_arr_store::alloc(uint32_t desiredSize, std::span<Value> init){
    auto capacity = std::bit_ceil(static_cast<uint64_t>(desiredSize));
    if(capacity > (1ull << 31)){
        // TODO: error
    }
    return gc::esl_make_gc<rt_arr_store>(capacity * sizeof(Value), capacity, init);
}

rt_arr::rt_arr(size_t size) : rt_obj(rt_type::ARRAY, false) {
    _size = size;
    _storage = nullptr;
}

void rt_arr::gc_init() {
    _storage = rt_arr_store::alloc(_size, {});
    gc::write_b(_storage);
}

static void barrier_range(std::span<Value> data, uint32_t from, uint32_t to) {
    for (uint32_t i = from; i < to; i++) {
        if (isObj(data[i])) gc::write_b(decodeObj(data[i]));
    }
}

void rt_arr::reserve(uint32_t min_cap) {
    if (min_cap <= _storage->get_data().size()) return;
    auto _new = rt_arr_store::alloc(min_cap, get_data());
    _storage = _new;
    gc::write_b(_storage);
}

void rt_arr::push(Value item){
    reserve(_size + 1);
    _storage->get_data()[_size++] = item;
    if (isObj(item)) {
        _storage->set_has_obj();
        gc::write_b(decodeObj(item));
    }
}

Value rt_arr::pop(){
    if (_size == 0) return encodeNil();
    auto data = _storage->get_data();
    Value item = data[--_size];
    // The whole storage is traced, not just the live part, so dead slots must be cleared
    data[_size] = 0;
    return item;
}

void rt_arr::resize(uint32_t new_size, Value fill){
    if (new_size == _size) return;
    if (new_size < _size) {
        auto data = _storage->get_data();
        memset(&data[new_size], 0, (_size - new_size) * sizeof(Value));
        _size = new_size;
        return;
    }
    reserve(new_size);
    auto data = _storage->get_data();
    for (uint32_t i = _size; i < new_size; i++) data[i] = fill;
    if (isObj(fill)) {
        _storage->set_has_obj();
        gc::write_b(decodeObj(fill));
    }
    _size = new_size;
}

void rt_arr::insert(uint32_t index, Value item){
    if (index >= _size) { push(item); return; }
    reserve(_size + 1);
    auto data = _storage->get_data();
    memmove(&data[index + 1], &data[index], (_size - index) * sizeof(Value));
    // Barrier after the shift, since every element moved is treated as being inserted again
    if (_storage->has_obj()) barrier_range(data, index + 1, _size + 1);
    data[index] = item;
    _size++;
    if (isObj(item)) {
        _storage->set_has_obj();
        gc::write_b(decodeObj(item));
    }
}

void rt_arr::erase(uint32_t index){
    if (index >= _size) return;
    auto data = _storage->get_data();
    memmove(&data[index], &data[index + 1], (_size - index - 1) * sizeof(Value));
    // This effectivly inserts all of the elements above index so we need a barrier after insertion
    if (_storage->has_obj()) barrier_range(data, index, _size - 1);
    data[--_size] = 0;
}

void rt_arr::clear(){
    auto data = _storage->get_data();
    memset(data.data(), 0, data.size() * sizeof(Value));
    _size = 0;
}
#pragma endregion

#pragma region ObjHashMap
rt_hashmap::rt_hashmap() : rt_obj(rt_type::HASH_MAP, false) {}
#pragma endregion