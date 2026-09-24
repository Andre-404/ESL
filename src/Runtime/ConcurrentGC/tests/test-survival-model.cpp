#include <gtest/gtest.h>

#include "../survival-model.h"

using namespace gc;
using namespace gc::detail;

namespace {
    constexpr double alpha = config::survival_alpha;
    constexpr double seed  = config::survival_seed;
    // What one cycle's blend does to an estimate
    double blended(double prev, double sampled) { return (1.0 - alpha) * prev + alpha * sampled; }

    void sample(survival_model& m, uint8_t age, uint64_t before, uint64_t after) {
        auto b = m.start_batch();
        b.add_retained(age, before, after);
    }
}

TEST(SurvivalModelTest, StartsAtTheSeedForEveryAge) {
    survival_model m;
    for (uint8_t age = 0; age < config::pg_age_cnt; age++) EXPECT_EQ(m.survival(age), seed);
}

TEST(SurvivalModelTest, SamplesOnlyLandOnceTheCycleEnds) {
    survival_model m;
    sample(m, 2, 100, 0);
    EXPECT_EQ(m.survival(2), seed) << "a rate that moved mid-cycle would be read by nomination "
                                     "halfway through being updated";
    m.end_cycle();
    EXPECT_NEAR(m.survival(2), blended(seed, 0.0), 1e-12);
}

TEST(SurvivalModelTest, ABatchMergesWhenItGoesOutOfScope) {
    survival_model m;
    {
        auto b = m.start_batch();
        b.add_retained(1, 100, 25);
    }
    m.end_cycle();
    EXPECT_NEAR(m.survival(1), blended(seed, 0.25), 1e-12);
}

TEST(SurvivalModelTest, RepeatedCyclesConvergeOnTheMeasuredRate) {
    survival_model m;
    for (int cycle = 0; cycle < 50; cycle++) {
        sample(m, 0, 1000, 100);
        m.end_cycle();
    }
    EXPECT_NEAR(m.survival(0), 0.1, 1e-3);
}

// A cycle in which no page of an age class was observed has to leave that class alone. Decaying
// it toward anything would make an untouched age look mortal, which is what nomination reads.
TEST(SurvivalModelTest, AnEmptyCycleKeepsTheEstimate) {
    survival_model m;
    sample(m, 4, 100, 0);
    m.end_cycle();
    auto after_sample = m.survival(4);

    m.end_cycle();
    m.end_cycle();
    EXPECT_EQ(m.survival(4), after_sample);
}

TEST(SurvivalModelTest, AgesDoNotBleedIntoEachOther) {
    survival_model m;
    sample(m, 3, 100, 0);
    m.end_cycle();

    EXPECT_LT(m.survival(3), seed);
    for (uint8_t age = 0; age < config::pg_age_cnt; age++) {
        if (age != 3) EXPECT_EQ(m.survival(age), seed) << "age " << int(age);
    }
}

TEST(SurvivalModelTest, RatiosAreClampedToOne) {
    survival_model m;
    sample(m, 0, 10, 20);
    m.end_cycle();
    EXPECT_NEAR(m.survival(0), blended(seed, 1.0), 1e-12);
}

TEST(SurvivalModelTest, AgesPastTheLastBucketFallIntoIt) {
    survival_model m;
    sample(m, 200, 100, 0);
    m.end_cycle();
    EXPECT_NEAR(m.survival(config::pg_age_cnt - 1), blended(seed, 0.0), 1e-12);
    EXPECT_EQ(m.survival(200), m.survival(config::pg_age_cnt - 1));
}

TEST(SurvivalModelTest, SamplesFromSeveralBatchesAddUp) {
    survival_model m;
    sample(m, 5, 100, 100);   // everything survived
    sample(m, 5, 100, 0);     // nothing did
    m.end_cycle();
    EXPECT_NEAR(m.survival(5), blended(seed, 0.5), 1e-12) << "the rate is over blocks, not pages";
}
