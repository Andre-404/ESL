#include "objects.h"
#include "../Values/valueHelpersInline.h"
#include "../../Includes/rapidhash.h"
#include "string-interner.h"

using namespace object;
using namespace valueHelpers;

#pragma region Obj
size_t rt_obj::get_sz(){
    switch(type()){
        case rt_type::STRING:
            return sizeof(rt_string) + ((rt_string*)this)->sz() + 1; // +1 for null terminator
        case rt_type::ARRAY:
            return sizeof(rt_arr);
        case rt_type::ARRAY_STORAGE_HEADER:
            return sizeof(rt_arr_store) + ((rt_arr_store*)this)->get_data().size() * sizeof(Value);
        case rt_type::CLOSURE:
            return sizeof(rt_closure) + ((rt_closure*)this)->get_env().size()*sizeof(Value);
        case rt_type::INSTANCE:
            return sizeof(rt_inst) + ((rt_inst*)this)->get_fields().size()*sizeof(Value);
        case rt_type::HASH_MAP:
            return sizeof(rt_hashmap);
        case rt_type::BUFFER:
            return sizeof(rt_buffer) + ((rt_buffer*)this)->size();
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
        case rt_type::HASH_MAP:{
            auto map = reinterpret_cast<rt_hashmap*>(this);
            string str = "{";
            map->for_each([&](Value key, Value val) {
                str.append(" \"").append(asString(key)->get_str()).append("\" : ");
                str.append(valueHelpers::toString(val, stack)).append(",");
            });
            if (map->size() > 0) str.erase(str.size() - 1);
            return str.append(" }");
        }
        default: break;
    }
    return "cannot stringfy object";
}
#pragma endregion

#pragma region ObjString
bool rt_string::compare(rt_string* other) {
	return _size == other->_size && std::memcmp(get_str(), other->get_str(), _size) == 0;
}

bool rt_string::compare(const string& other) {
	return _size == other.size() && std::memcmp(get_str(), other.c_str(), _size) == 0;
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

#pragma region ObjBuffer
rt_buffer* rt_buffer::alloc(uint32_t size, byte fill) {
    return gc::esl_make_gc<rt_buffer>(size, size, fill);
}
#pragma endregion

#pragma region ObjHashMap
using namespace swiss;

static byte h2(uint64_t hash) { return hash & 0x7f; }
static uint64_t h1(uint64_t hash) { return hash >> 7; }
static uint32_t group_mask(uint32_t cap) { return cap / group_sz - 1; }
// Max load factor is 7/8
static uint32_t usable(uint32_t cap) { return cap - cap / 8; }
static uint32_t cap_for(uint32_t entries) {
    uint64_t min = (uint64_t(entries) * 8 + 6) / 7;
    return std::bit_ceil(std::max<uint64_t>(min, group_sz));
}
static uint64_t hash_key(rt_string* key) { return rt_string::hash{}(key); }
static void barrier_item(Value* slots, uint32_t i) {
    gc::write_b(decodeObj(slots[2 * i])); // key
    if (auto val = slots[2 * i + 1]; isObj(val)) gc::write_b(decodeObj(val));
}

// Groups are probed triangularly (0, 1, 3, 6, ...) which visits every group of a power of 2 sized table
// Terminates because the load factor guarantees at least one empty slot
static uint32_t find_insert_slot(const byte* ctrl, uint32_t cap, uint64_t hash) {
    auto mask = group_mask(cap);
    uint32_t g = h1(hash) & mask;
    for (uint32_t step = 1;; g = (g + step++) & mask) {
        if (auto m = match_empty_or_deleted(load_group(ctrl, g))) return g * group_sz + first_slot(m);
    }
}

static void fill_slot(byte* ctrl, Value* slots, uint32_t i, uint64_t hash, rt_string* key, Value val) {
    ctrl[i] = h2(hash);
    slots[2 * i] = encodeObj(key);
    slots[2 * i + 1] = val;
    barrier_item(slots, i);
}

rt_hashmap::rt_hashmap(uint32_t expected)
    : rt_obj(rt_type::HASH_MAP, false), _count(0), _growth_left(0), _capacity(cap_for(expected)),
      _ctrl(nullptr), _slots(nullptr) {}

void rt_hashmap::gc_init() {
    _ctrl = rt_buffer::alloc(_capacity, ctrl_empty);
    gc::write_b(_ctrl);
    auto slots = rt_arr_store::alloc(_capacity * 2, {});
    slots->set_has_obj();
    _slots = slots;
    gc::write_b(_slots);
    _growth_left = usable(_capacity);
}

int64_t rt_hashmap::find_slot(rt_string* key, uint64_t hash) {
    auto c = ctrl();
    auto s = slots();
    auto mask = group_mask(_capacity);
    auto tag = h2(hash);
    uint32_t g = h1(hash) & mask;
    for (uint32_t step = 1;; g = (g + step++) & mask) {
        auto group = load_group(c, g);
        for (auto m = match_h2(group, tag); m; m &= m - 1) {
            auto i = g * group_sz + first_slot(m);
            auto k = asString(s[2 * i]);
            if (k == key || k->compare(key)) return i;
        }
        // An empty slot ends every probe sequence that could have placed key here
        if (match_empty(group)) return -1;
    }
}

Value* rt_hashmap::find(rt_string* key) {
    auto i = find_slot(key, hash_key(key));
    return i < 0 ? nullptr : &slots()[2 * i + 1];
}

void rt_hashmap::insert_or_assign(rt_string* key, Value val) {
    auto hash = hash_key(key);
    if (auto i = find_slot(key, hash); i >= 0) {
        slots()[2 * i + 1] = val;
        if (isObj(val)) gc::write_b(decodeObj(val));
        return;
    }
    auto i = find_insert_slot(ctrl(), _capacity, hash);
    // Tombstone reuse doesn't affect load factor, only rehash when using new empty slot that goes over the limit
    // If we need to rehash but most of the slots are tombstones don't increase capacity
    if (_growth_left == 0 && ctrl()[i] == ctrl_empty) {
        rehash(_count >= usable(_capacity) / 2 ? _capacity * 2 : _capacity);
        i = find_insert_slot(ctrl(), _capacity, hash);
    }
    if (ctrl()[i] == ctrl_empty) _growth_left--;
    fill_slot(ctrl(), slots(), i, hash, key, val);
    _count++;
}

bool rt_hashmap::erase(rt_string* key) {
    auto i = find_slot(key, hash_key(key));
    if (i < 0) return false;
    auto c = ctrl();
    auto s = slots();
    // The whole store is traced so dead slots must be cleared
    s[2 * i] = 0;
    s[2 * i + 1] = 0;
    // If group was empty before this erase then setting the control to empty won't break any probe sequence
    // since such a sequence would have had to terminate in this group
    if (match_empty(load_group(c, i / group_sz))) {
        c[i] = ctrl_empty;
        _growth_left++;
    } else c[i] = ctrl_deleted;
    _count--;
    return true;
}

void rt_hashmap::clear() {
    memset(ctrl(), ctrl_empty, _capacity);
    memset(slots(), 0, _capacity * 2 * sizeof(Value));
    _count = 0;
    _growth_left = usable(_capacity);
}

// Reinserted entries get barriered like new insertions because the new store may have been
// allocated black, in which case the marker never scans it
void rt_hashmap::rehash(uint32_t new_cap) {
    if (new_cap == _capacity) return in_place_rehash();

    auto new_ctrl = rt_buffer::alloc(new_cap, ctrl_empty);
    auto new_slots = rt_arr_store::alloc(new_cap * 2, {});
    new_slots->set_has_obj();
    auto nc = new_ctrl->get_data().data();
    auto ns = new_slots->get_data().data();
    for_each([&](Value key, Value val) {
        auto k = asString(key);
        auto hash = hash_key(k);
        fill_slot(nc, ns, find_insert_slot(nc, new_cap, hash), hash, k, val);
    });
    _ctrl = new_ctrl;
    _slots = new_slots;
    _capacity = new_cap;
    gc::write_b(new_ctrl);
    gc::write_b(new_slots);
    _growth_left = usable(new_cap) - _count;
}

void rt_hashmap::in_place_rehash() {
    for (uint32_t g = 0; g < _capacity / group_sz; g++)
        set_group(ctrl(), g, reset_group(load_group(ctrl(), g)));

    for (uint32_t i = 0; i < _capacity; i++) {
        if (ctrl()[i] != ctrl_deleted) continue;

        auto hash = hash_key(asString(slots()[2 * i]));
        auto new_i = find_insert_slot(ctrl(), _capacity, hash);

        if (new_i / group_sz == i / group_sz) {
            ctrl()[i] = h2(hash);
            continue;
        }

        auto is_empty = ctrl()[new_i] == ctrl_empty;
        std::swap(slots()[2 * i], slots()[2 * new_i]);
        std::swap(slots()[2 * i + 1], slots()[2 * new_i + 1]);

        barrier_item(slots(), new_i);

        ctrl()[new_i] = h2(hash);
        // An empty item already has zeroed slot memory
        if (is_empty) 
            ctrl()[i] = ctrl_empty;
        else {
            // Since the swap is considered as inserting both items again we have to berrier on both paths
            barrier_item(slots(), i);
            i--;
        }
    }

    _growth_left = usable(_capacity) - _count;
}
#pragma endregion