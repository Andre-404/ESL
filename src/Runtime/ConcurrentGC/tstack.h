#pragma once
#include <atomic>
#include <concepts>
#include <utility>
#include <cassert>
// Handles ABA through pointer tagging
namespace gc::detail {
    template<typename T>
    concept tstack_node = requires(T* node, T* next) {
        node->link(next);
        node->unlink();
        { node->next() } -> std::convertible_to<T*>;
    };
    
    template<typename T>
    class tnode {
        std::atomic<T*> _next;
    public:
        tnode() : _next(nullptr) {}

        void link(T* next) { _next.store(next, std::memory_order_relaxed); }
        void unlink() { _next.store(nullptr, std::memory_order_relaxed); }
        T* next() const { return _next.load(std::memory_order_relaxed); }
    };

    template<tstack_node T>
    class tstack {
        T* _head;
        constexpr static size_t ptr_bits = 48;
        constexpr static size_t cnt_bits = 16;

        static T* pack_node(T* node, size_t cnt) {
            assert((uintptr_t(node) >> ptr_bits) == 0);
            return (T*)((size_t)node | (cnt << ptr_bits));
        }

        static std::pair<T*, size_t> unpack_node(T* node) {
            auto ptr = (size_t)node & ((1ull << ptr_bits) - 1);
            auto cnt = (size_t)node >> ptr_bits & ((1ull << cnt_bits) - 1);
            return { (T*)ptr, cnt };
        }
    public:
        tstack() : _head(nullptr) {}

        void lfpush(T* node) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_relaxed);
            T* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                node->link(ptr);
                new_head = pack_node(node, cnt + 1);
            } while (!ref.compare_exchange_weak(old_head, new_head, std::memory_order_release, std::memory_order_relaxed));
        }
        T* lfpop() {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_acquire);
            T* new_head;

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
            return node;
        }
        void lfpush_range(T* first, T* last) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_relaxed);
            T* new_head;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                last->link(ptr);
                new_head = pack_node(first, cnt + 1);
            } while (!ref.compare_exchange_weak(old_head, new_head, std::memory_order_release,std::memory_order_relaxed));
        }

        T* lf_reset_head(T* new_head) {
            auto ref = std::atomic_ref { _head };
            auto old_head = ref.load(std::memory_order_acquire);
            T* nh;

            do {
                auto [ptr, cnt] = unpack_node(old_head);
                if (ptr == nullptr) return nullptr;
                nh = pack_node(new_head, cnt + 1);
            } while (!ref.compare_exchange_weak(
                old_head, nh,
                std::memory_order_acquire,  // observe prior push stores
                std::memory_order_acquire));

            auto [node, _] = unpack_node(old_head);
            return node;
        }
    };
}
