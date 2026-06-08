#include <gtest/gtest.h>
#include <array>
#include "../TCB.h"
#include "../mark-buf.h"

using namespace gc;
using namespace gc::detail;


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


TEST(ThdMarkInfoTest, DefaultGetCtxIsEmpty) {
    thd_mark_info info;
    auto [stack, regs] = info.get_ctx();
    EXPECT_TRUE(stack.empty());
    // regs span is still empty (untracked stack short-circuits the function).
    EXPECT_TRUE(regs.empty());
}

TEST(ThdMarkInfoTest, TrackAndCaptureGivesNonEmptyContext) {
    thd_mark_info info;
    // Use a "high" stack address so that _stack_start - _stack_end is positive.
    // We pick something well above whatever read_stack() will return.
    size_t fake_top = reinterpret_cast<size_t>(__builtin_frame_address(0)) + 1024;
    info.track_stack(fake_top);
    info.capture_ctx();

    auto [stack, regs] = info.get_ctx();
    EXPECT_FALSE(stack.empty()) << "stack span should cover [stack_end, stack_start)";
    EXPECT_EQ(regs.size(), platform::regs_to_store());
}

TEST(ThdMarkInfoTest, UntrackStackRestoresEmptyContext) {
    thd_mark_info info;
    info.track_stack(reinterpret_cast<size_t>(__builtin_frame_address(0)) + 1024);
    info.capture_ctx();
    info.untrack_stack();

    auto [stack, regs] = info.get_ctx();
    EXPECT_TRUE(stack.empty());
    EXPECT_TRUE(regs.empty());
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