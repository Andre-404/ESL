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
            for (auto& slot : arr->get_data())
                str.append(" " + valueHelpers::toString(gc::read_ref(slot), stack)).append(",");
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
        auto val = gc::read_ref(init[i]);
        s[i] = val;
        if (isObj(val)) {
            found = true;
            gc::write_b(this, decodeObj(val));
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
    auto store = rt_arr_store::alloc(_size, {});
    gc::store_ref(_storage, store);
    gc::write_b(this, store);
}

static void shift_slots(rt_arr_store* store, uint32_t to, uint32_t from, uint32_t cnt) {
    auto data = store->get_data();
    if (!store->has_obj()) {
        memmove(&data[to], &data[from], cnt * sizeof(Value));
        return;
    }
    auto move = [&](uint32_t i) {
        auto val = gc::read_ref(data[from + i]);
        gc::store_ref(data[to + i], val);
        if (isObj(val)) gc::write_b(store, decodeObj(val));
    };
    if (to < from) for (uint32_t i = 0; i < cnt; i++) move(i);
    else for (uint32_t i = cnt; i-- > 0;) move(i);
}
static void zero_slots(rt_arr_store* store, uint32_t from, uint32_t cnt) {
    auto data = store->get_data();
    if (!store->has_obj()) {
        memset(&data[from], 0, cnt * sizeof(Value));
        return;
    }
    for (uint32_t i = from; i < from + cnt; i++) gc::store_ref(data[i], 0);
}

rt_arr_store* rt_arr::reserve(uint32_t min_cap) {
    auto store = get_store();
    if (min_cap <= store->get_data().size()) return store;
    auto _new = rt_arr_store::alloc(min_cap, store->get_data().first(_size));
    gc::store_ref(_storage, _new);
    gc::write_b(this, _new);
    return _new;
}

void rt_arr::push(Value item){
    auto store = reserve(_size + 1);
    gc::store_ref(store->get_data()[_size++], item);
    if (isObj(item)) {
        store->set_has_obj();
        gc::write_b(store, decodeObj(item));
    }
}

Value rt_arr::pop(){
    if (_size == 0) return encodeNil();
    auto data = get_store()->get_data();
    Value item = gc::read_ref(data[--_size]);
    // The whole storage is traced, not just the live part, so dead slots must be cleared
    gc::store_ref(data[_size], 0);
    return item;
}

void rt_arr::resize(uint32_t new_size, Value fill){
    if (new_size == _size) return;
    if (new_size < _size) {
        zero_slots(get_store(), new_size, _size - new_size);
        _size = new_size;
        return;
    }
    auto store = reserve(new_size);
    auto data = store->get_data();
    for (uint32_t i = _size; i < new_size; i++) gc::store_ref(data[i], fill);
    if (isObj(fill)) {
        store->set_has_obj();
        gc::write_b(store, decodeObj(fill));
    }
    _size = new_size;
}

void rt_arr::insert(uint32_t index, Value item){
    if (index >= _size) { push(item); return; }
    auto store = reserve(_size + 1);
    shift_slots(store, index + 1, index, _size - index);
    gc::store_ref(store->get_data()[index], item);
    _size++;
    if (isObj(item)) {
        store->set_has_obj();
        gc::write_b(store, decodeObj(item));
    }
}

void rt_arr::erase(uint32_t index){
    if (index >= _size) return;
    auto store = get_store();
    shift_slots(store, index, index + 1, _size - index - 1);
    gc::store_ref(store->get_data()[--_size], 0);
}

void rt_arr::clear(){
    auto store = get_store();
    zero_slots(store, 0, store->get_data().size());
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
static void barrier_item(rt_arr_store* store, Value key, Value val) {
    gc::write_b(store, decodeObj(key));
    if (isObj(val)) gc::write_b(store, decodeObj(val));
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

static void fill_slot(byte* ctrl, rt_arr_store* store, uint32_t i, uint64_t hash, rt_string* key, Value val) {
    auto slots = store->get_data().data();
    auto k = encodeObj(key);
    ctrl[i] = h2(hash);
    gc::store_ref(slots[2 * i], k);
    gc::store_ref(slots[2 * i + 1], val);
    barrier_item(store, k, val);
}

rt_hashmap::rt_hashmap(uint32_t expected)
    : rt_obj(rt_type::HASH_MAP, false), _count(0), _growth_left(0), _capacity(cap_for(expected)),
      _ctrl(nullptr), _slots(nullptr) {}

void rt_hashmap::gc_init() {
    auto ctrl_buf = rt_buffer::alloc(_capacity, ctrl_empty);
    gc::store_ref(_ctrl, ctrl_buf);
    gc::write_b(this, ctrl_buf);
    auto slots = rt_arr_store::alloc(_capacity * 2, {});
    slots->set_has_obj();
    gc::store_ref(_slots, slots);
    gc::write_b(this, slots);
    _growth_left = usable(_capacity);
}

int64_t rt_hashmap::find_slot(const byte* c, Value* s, rt_string* key, uint64_t hash) {
    auto mask = group_mask(_capacity);
    auto tag = h2(hash);
    uint32_t g = h1(hash) & mask;
    for (uint32_t step = 1;; g = (g + step++) & mask) {
        auto group = load_group(c, g);
        for (auto m = match_h2(group, tag); m; m &= m - 1) {
            auto i = g * group_sz + first_slot(m);
            auto k = asString(gc::read_ref(s[2 * i]));
            if (k == key || k->compare(key)) return i;
        }
        // An empty slot ends every probe sequence that could have placed key here
        if (match_empty(group)) return -1;
    }
}

Value* rt_hashmap::find(rt_string* key) {
    auto s = slots();
    auto i = find_slot(ctrl(), s, key, hash_key(key));
    return i < 0 ? nullptr : &s[2 * i + 1];
}

void rt_hashmap::insert_or_assign(rt_string* key, Value val) {
    auto hash = hash_key(key);
    auto c = ctrl();
    auto store = get_slots();
    auto s = store->get_data().data();
    if (auto i = find_slot(c, s, key, hash); i >= 0) {
        gc::store_ref(s[2 * i + 1], val);
        if (isObj(val)) gc::write_b(store, decodeObj(val));
        return;
    }
    auto i = find_insert_slot(c, _capacity, hash);
    // Tombstone reuse doesn't affect load factor, only rehash when using new empty slot that goes over the limit
    // If we need to rehash but most of the slots are tombstones don't increase capacity
    if (_growth_left == 0 && c[i] == ctrl_empty) {
        rehash(_count >= usable(_capacity) / 2 ? _capacity * 2 : _capacity);
        // A rehash to a different capacity replaces both
        c = ctrl();
        store = get_slots();
        i = find_insert_slot(c, _capacity, hash);
    }
    if (c[i] == ctrl_empty) _growth_left--;
    fill_slot(c, store, i, hash, key, val);
    _count++;
}

bool rt_hashmap::erase(rt_string* key) {
    auto c = ctrl();
    auto s = slots();
    auto i = find_slot(c, s, key, hash_key(key));
    if (i < 0) return false;
    // The whole store is traced so dead slots must be cleared
    gc::store_ref(s[2 * i], 0);
    gc::store_ref(s[2 * i + 1], 0);
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
    zero_slots(get_slots(), 0, _capacity * 2);
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
    for_each([&](Value key, Value val) {
        auto k = asString(key);
        auto hash = hash_key(k);
        fill_slot(nc, new_slots, find_insert_slot(nc, new_cap, hash), hash, k, val);
    });
    gc::store_ref(_ctrl, new_ctrl);
    gc::store_ref(_slots, new_slots);
    _capacity = new_cap;
    gc::write_b(this, new_ctrl);
    gc::write_b(this, new_slots);
    _growth_left = usable(new_cap) - _count;
}

void rt_hashmap::in_place_rehash() {
    auto c = ctrl();
    auto store = get_slots();
    auto s = store->get_data().data();

    for (uint32_t g = 0; g < _capacity / group_sz; g++)
        set_group(c, g, reset_group(load_group(c, g)));

    for (uint32_t i = 0; i < _capacity; i++) {
        if (c[i] != ctrl_deleted) continue;

        auto key = gc::read_ref(s[2 * i]);
        auto hash = hash_key(asString(key));
        auto new_i = find_insert_slot(c, _capacity, hash);

        if (new_i / group_sz == i / group_sz) {
            c[i] = h2(hash);
            continue;
        }

        auto is_empty = c[new_i] == ctrl_empty;
        // Swap the item at i with whatever is at new_i
        auto val = gc::read_ref(s[2 * i + 1]);
        auto other_key = gc::read_ref(s[2 * new_i]);
        auto other_val = gc::read_ref(s[2 * new_i + 1]);
        gc::store_ref(s[2 * new_i], key);
        gc::store_ref(s[2 * new_i + 1], val);
        gc::store_ref(s[2 * i], other_key);
        gc::store_ref(s[2 * i + 1], other_val);

        barrier_item(store, key, val);

        c[new_i] = h2(hash);
        // An empty item already has zeroed slot memory
        if (is_empty)
            c[i] = ctrl_empty;
        else {
            // Since the swap is considered as inserting both items again we have to berrier on both paths
            barrier_item(store, other_key, other_val);
            i--;
        }
    }

    _growth_left = usable(_capacity) - _count;
}
#pragma endregion