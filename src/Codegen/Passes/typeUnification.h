#include "ASTToTypedAST.h"
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
            vector<vector<types::tyPtr>> run(tyEnv& typeEnv);
        private:
            vector<vector<types::tyPtr>> collapsedTypes;
            tyEnv typeEnv;

            // Transfers all types with no constraints to collapsedTypes
            void initalPass();

            vector<types::tyPtr> collapseType(const types::tyVarIdx idx, shared_ptr<types::TypeConstraint> typeConstraint = nullptr );

            vector<types::tyPtr> resolveConstraints(vector<constraint>& tyConstraints, constraintSet& processed);

            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::AddTyConstraint> addConstraint);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::ComputeAddTysConstraint> computeAddConstraint);

            // Helpers
            vector<types::tyPtr> getPossibleFuncsFromFuts(shared_ptr<types::AwaitTyConstraint> awaitConstraint,
                                                          vector<types::tyPtr>& possibleFutureTypes);

            pair<vector<types::tyPtr>, vector<constraint>> getPossibleRetTysFromFuncs(vector<types::tyPtr>& possibleFuncTypes);
        };
    }
}
