// Save rbx, rbx, r12, r13, r14, r15
consteval std::size_t regs_to_store() {
    return 6;
}

__attribute__((noinline)) inline void store_regs(void* storage) {
    asm volatile(
        "movq %%rbx, 0(%0)\n\t"
        "movq %%rbp, 8(%0)\n\t"
        "movq %%r12, 16(%0)\n\t"
        "movq %%r13, 24(%0)\n\t"
        "movq %%r14, 32(%0)\n\t"
        "movq %%r15, 40(%0)\n\t"
        : : "r"(storage) : "memory");
}

__attribute__((noinline)) inline void* read_stack() {
    void *sp;
    asm volatile("mov %%rsp, %0" : "=r"(sp));
    return sp;
}

inline uint64_t read_color() {
    uint64_t c;
    asm volatile("mov %%r14, %0" : "=r"(c));
    return c;
}

inline void write_color(uint64_t c) {
    asm volatile("mov %0, %%r14" : : "r"(c));
}

inline void* read_tcb() {
    void *sp;
    asm volatile("mov %%r15, %0" : "=r"(sp));
    return sp;
}

inline void set_tcb(void* handle) {
    __asm__ volatile("mov %0, %%r15" : : "r" (handle));
}