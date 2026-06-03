#pragma once

// Treiber stack implementation that offers non thread safe methods to modify and iterate over the stack
// Handles ABA through pointer tagging
namespace gc::detail {
    template<typename T>
    class tnode {
        tnode* _next;
    public:
        tnode() : _next(nullptr) {}

        void link(tnode* next) { _next = next; }
        void unlink() { _next = nullptr; }
        T* next() const { return (T*)_next; }
    };

    template<typename T>
    class tstack {
        tnode<T>* _head;
        constexpr static size_t ptr_bits = 48;
        constexpr static size_t cnt_bits = 16;

        static tnode<T>* pack_node(tnode<T>* node, size_t cnt) {
            return (tnode<T>*)((size_t)node | (cnt << ptr_bits));
        }

        static std::pair<tnode<T>*, size_t> unpack_node(tnode<T>* node) {
            auto ptr = (size_t)node & ((1ull << ptr_bits) - 1);
            auto cnt = (size_t)node >> ptr_bits & ((1ull << cnt_bits) - 1);
            return { (tnode<T>*)ptr, cnt };
        }
    public:
        tstack() : _head(nullptr) {}

        void lfpush(tnode<T>* node) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_relaxed);
            tnode<T>* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                node->link(ptr);
                new_head = pack_node(node, cnt + 1);
            } while (!ref.compare_exchange_weak(old_head, new_head, std::memory_order_release,std::memory_order_relaxed));
        }
        void lfpush_range(tnode<T>* first, tnode<T>* last) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_relaxed);
            tnode<T>* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                last->link(ptr);
                new_head = pack_node(first, cnt + 1);
            } while (!ref.compare_exchange_weak(old_head, new_head, std::memory_order_release,std::memory_order_relaxed));
        }
        T* lfpop() {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_acquire);
            tnode<T>* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                if (ptr == nullptr)
                    return nullptr;
                new_head = pack_node(ptr->next(), cnt + 1);
            } while (!ref.compare_exchange_weak(
                old_head, new_head,
                std::memory_order_acquire,  // observe prior push stores
                std::memory_order_acquire));

            auto [node, _] = unpack_node(old_head);
            node->unlink();
            return (T*)node;
        }
        // NOT thread safe, ABA counter gets reset to 0
        void push(tnode<T>* node) {
            node->link(unpack_node(_head).first);
            _head = node;
        }
        void push_range(tnode<T>* first, tnode<T>* last) {
            last->link(unpack_node(_head).first);
            _head = first;
        }
        T* pop() {
            auto [node, _] = unpack_node(_head);
            _head = node->next();
            return (T*)node;
        }

        T* peek() const {
            return (T*)unpack_node(_head).first;
        }
        void reset_head(tnode<T>* new_head) {
            _head = new_head;
        }
    };
}
