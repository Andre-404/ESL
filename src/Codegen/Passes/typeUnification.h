#pragma once
#include "../../TypedAST/Types.h"
#include "../../Includes/unorderedDense.h"

namespace passes {
    namespace typeUnification {
        using std::shared_ptr;
        using std::pair;

        using constraint = shared_ptr<types::TypeConstraint>;
        using tyEnv = vector<pair<types::tyPtr, vector<constraint>>>;
        using constraintSet = ankerl::unordered_dense::set<constraint>;


        class TypeUnificator {
        public:
            TypeUnificator();
            bool hadError;
            vector<types::tyPtr> run(tyEnv& typeEnv);
        private:
            vector<types::tyPtr> collapsedTypes;
            tyEnv typeEnv;

            // Transfers all types with no constraints to collapsedTypes
            void initalPass();

            types::tyPtr collapseType(const types::tyVarIdx idx, shared_ptr<types::TypeConstraint> typeConstraint = nullptr );

            types::tyPtr resolveConstraints(vector<constraint> tyConstraints, constraintSet& processed);

            pair<types::tyPtr, vector<constraint>> processConstraint(shared_ptr<types::AddTyConstraint> addConstraint);
            pair<types::tyPtr, vector<constraint>> processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint);
            pair<types::tyPtr, vector<constraint>> processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint);
            pair<types::tyPtr, vector<constraint>> processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint);
            pair<types::tyPtr, vector<constraint>> processConstraint(shared_ptr<types::ComputeAddTysConstraint> computeAddConstraint);

            // Helpers
            types::tyPtr getPossibleFuncFromFut(shared_ptr<types::AwaitTyConstraint> awaitConstraint,
                                                          types::tyPtr possibleFutureType);

            pair<types::tyPtr, vector<constraint>> getPossibleRetTysFromFuncs(types::tyPtr possibleFuncType);
        };
    }
}
