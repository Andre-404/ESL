#pragma once
#include "../../TypedAST/Types.h"
#include "../../Includes/unorderedDense.h"

namespace passes {
    namespace computeClassHierarchy{

        // Used to model the class hierarchy
        struct ClassNode{
            vector<string> subclasses;
        };

        template<class T, class K>
        using fastMap = ankerl::unordered_dense::map<T, K>;
        template<class T>
        using fastSet = ankerl::unordered_dense::set<T>;

        class ComputeClassHierarchyPass{
        public:
            ComputeClassHierarchyPass();
            bool hadError;
            fastMap<string, std::pair<int, int>> run(fastMap<string, ClassNode>& classes);
        private:
            fastSet<string> visited;
            fastMap<string, std::pair<int, int>> hierarchy;
            int order;

            std::pair<int, int> dfs(string className, fastMap<string, ClassNode>& classes);
        };
    }
}