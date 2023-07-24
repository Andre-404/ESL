#include "ASTToTypedAST.h"
#include "../TypedAST/Types.h"
#include "../../Includes/unorderedDense.h"

namespace passes {
    namespace TypeUnification {
        using std::shared_ptr;
        using std::pair;

        using constraint = shared_ptr<types::TypeConstraint>;
        using tyEnv = vector<pair<types::tyPtr, vector<constraint>>>;
        using constraintSet = ankerl::unordered_dense::set<constraint>;


        class TypeUnificator {
        public:
            TypeUnificator();
            bool hadError;
            void run(tyEnv typeEnv);
        private:
            vector<vector<types::tyPtr>> collapsedTypes;
            tyEnv typeEnv;

            // Transfers all types with no constraints to collapsedTypes
            void initalPass();

            vector<types::tyPtr> collapseType(types::tyVarIdx idx, pair<types::tyPtr, vector<constraint>>& ty);

            vector<types::tyPtr> resolveConstraints(vector<constraint> tyConstraints);

            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::AddTyConstraint> addConstraint, constraintSet& processed);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::CallResTyConstraint> callConstraint, constraintSet& processed);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint, constraintSet& processed);
            pair<vector<types::tyPtr>, vector<constraint>> processConstraint(shared_ptr<types::AwaitTyConstraint> awaitConstraint, constraintSet& processed);

            void error(Token token, string msg);
        };
    }
}
