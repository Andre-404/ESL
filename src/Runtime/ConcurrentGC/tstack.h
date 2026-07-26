#pragma once
#include <atomic>
#include <utility>
#include <cassert>
// Treiber stack implementation that offers non thread safe methods to modify and iterate over the stack
// Handles ABA through pointer tagging
namespace gc::detail {
    template<typename T>
    class tnode {
        std::atomic<tnode*> _next;
    public:
        tnode() : _next(nullptr) {}

        void link(tnode* next) { _next.store(next, std::memory_order_relaxed); }
        void unlink() { _next.store(nullptr, std::memory_order_relaxed); }
        T* next() const { return (T*)_next.load(std::memory_order_relaxed); }
    };

    template<typename T>
    class tstack {
        tnode<T>* _head;
        constexpr static size_t ptr_bits = 48;
        constexpr static size_t cnt_bits = 16;

        static tnode<T>* pack_node(tnode<T>* node, size_t cnt) {
            assert((uintptr_t(node) >> ptr_bits) == 0);
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
            } while (!ref.compare_exchange_weak(old_head, new_head, std::memory_order_release, std::memory_order_relaxed));
        }
        T* lfpop() {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_acquire);
            tnode<T>* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                if (ptr == nullptr) return nullptr;
                new_head = pack_node(ptr->next(), cnt + 1);
            } while (!ref.compare_exchange_weak(
                old_head, new_head,
                std::memory_order_acquire,  // observe prior push stores
                std::memory_order_acquire));

            auto [node, _] = unpack_node(old_head);
            node->unlink();
            return (T*)node;
        }

        T* lf_reset_head(tnode<T>* new_head) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_acquire);
            tnode<T>* nh;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                if (ptr == nullptr) return nullptr;
                nh = pack_node(new_head, cnt + 1);
            } while (!ref.compare_exchange_weak(
                old_head, nh,
                std::memory_order_acquire,  // observe prior push stores
                std::memory_order_acquire));

            auto [node, _] = unpack_node(old_head);
            return (T*)node;
        }
    };
}
