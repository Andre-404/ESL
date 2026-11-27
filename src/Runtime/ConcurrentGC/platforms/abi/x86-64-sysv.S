// with_regs_pushed (../abi.h) for the System V x86-64 ABI, ELF. All six callee-saved registers are pushed
// and popped like any function's, rbp first so the frame chain holds. The five after it leave rsp 8 short
// of the 16 the call needs
    .text
    .globl gc_platform_with_regs_pushed
    .type gc_platform_with_regs_pushed, @function
    .p2align 4
gc_platform_with_regs_pushed:
    .cfi_startproc
    pushq %rbp
    .cfi_def_cfa_offset 16
    .cfi_offset %rbp, -16
    movq %rsp, %rbp
    .cfi_def_cfa_register %rbp
    pushq %rbx
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    .cfi_offset %rbx, -24
    .cfi_offset %r12, -32
    .cfi_offset %r13, -40
    .cfi_offset %r14, -48
    .cfi_offset %r15, -56
    movq %rsi, %rax
    movq %rsp, %rsi
    subq $8, %rsp
    callq *%rax
    addq $8, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbx
    popq %rbp
    .cfi_def_cfa %rsp, 8
    retq
    .cfi_endproc
    .size gc_platform_with_regs_pushed, .-gc_platform_with_regs_pushed

// Without it the linker takes this object to need an executable stack
    .section .note.GNU-stack,"",@progbits
