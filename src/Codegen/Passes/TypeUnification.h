#include "ASTToTypedAST.h"
#include "../TypedAST/Types.h"
#include "../../Includes/unorderedDense.h"

namespace passes {
    namespace TypeUnification {
        using constraint = std::shared_ptr<types::TypeConstraint>;
        class TypeUnificator {
        public:
            TypeUnificator();
            bool hadError;
            void run(vector<std::pair<types::tyPtr, vector<constraint>>> typeEnv);
        private:
            //Types that are in the progress of being collapsed
            ankerl::unordered_dense::set<types::tyPtr> inProgress;
            ankerl::unordered_dense::set<types::TypeFlag> opaqueTypes;
            ankerl::unordered_dense::map<types::tyPtr, vector<types::tyPtr>> collapsedTypes;

            vector<types::tyPtr> resolveConstraints(vector<constraint> tyConstraints, ankerl::unordered_dense::set<constraint> processed);

            vector<types::tyPtr> processConstraint(std::shared_ptr<types::AddTyConstraint> addConstraint);
            vector<types::tyPtr> processConstraint(std::shared_ptr<types::GetCallResTyConstraint> callConstraint);
            vector<types::tyPtr> processConstraint(std::shared_ptr<types::InstGetFieldTyConstraint> instGetConstraint);
            vector<types::tyPtr> processConstraint(std::shared_ptr<types::GetAwaitTyConstraint> awaitConstraint);

            void error(Token token, string msg);
        };
    }
}
