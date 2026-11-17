constexpr size_t regs_to_store() { return 16; }

inline size_t read_stack_override = 0;

inline size_t read_stack() {
    if (read_stack_override) return read_stack_override;
    return reinterpret_cast<size_t>(__builtin_frame_address(0));
}

inline thread_local uint64_t color_reg = 0;
inline uint64_t read_color() { return color_reg; }
inline void write_color(uint64_t c) { color_reg = c; }

inline void store_regs(size_t* regs) {
    // Tests just need this to write something to all slots.
    std::memset(regs, 0, regs_to_store() * sizeof(size_t));
}
