#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <vector>
#include "../TCB.h"
#include "../mark-buf.h"
#include "../platform-specific.h"

using namespace gc;
using namespace gc::detail;

#ifdef __x86_64__
// What the asm below leaves for the checks: the caller's sp at the call and its rbp, then rbx and r12-r15
// once the call has returned. Named in the asm, so it needs no operand registers
extern "C" { size_t platform_test_out[7]; }

namespace {
    std::vector<size_t> pushed;
    void record_pushed(void*, size_t* sp) {
        auto* call_sp = reinterpret_cast<size_t*>(platform_test_out[0]);
        if (sp < call_sp && call_sp - sp < 64) pushed.assign(sp, call_sp);
    }
}

// A frame that waits keeps values in callee-saved registers, which a pause only sees if they are between
// the sp fn gets and the caller's frame. Each must also be back as it was once the call returns
TEST(PlatformTest, X86WithRegsPushedPutsEveryCalleeSavedRegisterAboveSp) {
    void* arg = nullptr;
    auto* fn = &record_pushed;
    auto* entry = &platform::with_regs_pushed;
    asm volatile(
        "movq $0xb0, %%rbx\n\t"
        "movq $0x12, %%r12\n\t"
        "movq $0x13, %%r13\n\t"
        "movq $0x14, %%r14\n\t"
        "movq $0x15, %%r15\n\t"
        "movq %%rsp, platform_test_out(%%rip)\n\t"
        "movq %%rbp, platform_test_out+8(%%rip)\n\t"
        "call *%%rax\n\t"
        "movq %%rbx, platform_test_out+16(%%rip)\n\t"
        "movq %%r12, platform_test_out+24(%%rip)\n\t"
        "movq %%r13, platform_test_out+32(%%rip)\n\t"
        "movq %%r14, platform_test_out+40(%%rip)\n\t"
        "movq %%r15, platform_test_out+48(%%rip)"
        : "+D"(arg), "+S"(fn), "+a"(entry)
        :
        : "rbx", "r12", "r13", "r14", "r15", "rcx", "rdx", "r8", "r9", "r10", "r11", "cc", "memory",
          "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
          "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15");
    size_t rbp = platform_test_out[1];
    for (size_t reg : { size_t(0xb0), rbp, size_t(0x12), size_t(0x13), size_t(0x14), size_t(0x15) })
        EXPECT_NE(std::ranges::find(pushed, reg), pushed.end()) << std::hex << reg << " isn't above sp";
    std::array<size_t, 5> after;
    std::copy_n(platform_test_out + 2, 5, after.begin());
    EXPECT_EQ(after, (std::array<size_t, 5> { 0xb0, 0x12, 0x13, 0x14, 0x15 })) << "a register changed across the call";
}
#endif


TEST(TcbHandleTest, TakeStartArgsReturnsConstructorValuesAndClears) {
    std::array<size_t, 3> args{ 10, 20, 30 };
    tcb_handle h{args.data(), static_cast<uint8_t>(args.size())};

    auto [ptr, cnt] = h.take_start_args();
    EXPECT_EQ(ptr, args.data());
    EXPECT_EQ(cnt, args.size());

    auto [ptr2, cnt2] = h.take_start_args();
    EXPECT_EQ(ptr2, nullptr);
    EXPECT_EQ(cnt2, 0u);
}


TEST(ThdMarkInfoTest, DefaultGetStackIsEmpty) {
    thd_mark_info info;
    EXPECT_TRUE(info.get_stack().empty());
}

TEST(ThdMarkInfoTest, GetStackSpansFromTheEndToTheStart) {
    thd_mark_info info;
    std::array<size_t, 8> stack {};
    info.track_stack(reinterpret_cast<size_t>(stack.data() + stack.size()));
    info.set_stack_end(stack.data() + 2);
    EXPECT_EQ(info.get_stack().data(), stack.data() + 2);
    EXPECT_EQ(info.get_stack().size(), 6u);
}

TEST(ThdMarkInfoTest, UntrackStackRestoresEmptyContext) {
    thd_mark_info info;
    std::array<size_t, 8> stack {};
    info.track_stack(reinterpret_cast<size_t>(stack.data() + stack.size()));
    info.set_stack_end(stack.data());
    info.untrack_stack();
    EXPECT_TRUE(info.get_stack().empty());
}

TEST(ThdMarkInfoTest, WbBufGetSet) {
    thd_mark_info info;
    EXPECT_EQ(info.get_wbbuf(), nullptr);

    mark_buf buf;
    info.set_wbbuf(&buf);
    EXPECT_EQ(info.get_wbbuf(), &buf);
}


TEST(TcbTest, DefaultStateIsRunning) {
    tcb t{nullptr, 0};
    EXPECT_EQ(t.load_state(), thd_state::running);
    EXPECT_EQ(t.get_opcode(), 0u);
}

TEST(TcbTest, OpcodeRoundTrip) {
    tcb t{nullptr, 0};
    t.set_opcode(7);
    EXPECT_EQ(t.get_opcode(), 7u);
    t.set_opcode(0);
    EXPECT_EQ(t.get_opcode(), 0u);
}

TEST(TcbTest, TransitionPublishesNewState) {
    tcb t{nullptr, 0};
    t.transition(thd_state::blocked);
    EXPECT_EQ(t.load_state(), thd_state::blocked);
}

TEST(TcbTest, TryTransitionRequiresMatchingExpected) {
    tcb t{nullptr, 0};
    EXPECT_FALSE(t.try_transition(thd_state::blocked, thd_state::running)) << "expected mismatch must fail the CAS";
    EXPECT_EQ(t.load_state(), thd_state::running);

    EXPECT_TRUE(t.try_transition(thd_state::running, thd_state::has_pending));
    EXPECT_EQ(t.load_state(), thd_state::has_pending);
}

TEST(TcbTest, ArenaAccessibleByReference) {
    tcb t{nullptr, 0};
    // Just confirm we can take a reference (the test arena shim is empty).
    arena& a = t.get_arena();
    (void)a;
    SUCCEED();
}

TEST(TcbTest, MarkInfoSurvivesAcrossTcb) {
    tcb t{nullptr, 0};
    mark_buf buf;
    t.get_mark_info().set_wbbuf(&buf);
    EXPECT_EQ(t.get_mark_info().get_wbbuf(), &buf);
}

TEST(TcbTest, InheritsTcbHandle) {
    size_t args[] = { 1, 2 };
    tcb t{args, 2};
    auto [ptr, cnt] = t.take_start_args();
    EXPECT_EQ(ptr, args);
    EXPECT_EQ(cnt, 2u);
}