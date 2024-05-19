#pragma once
#include "computeClassHierarchy.h"

using namespace passes::computeClassHierarchy;

ComputeClassHierarchyPass::ComputeClassHierarchyPass(){
    hadError = false;
    order = 0;
}

fastMap<string, std::pair<int, int>> ComputeClassHierarchyPass::run(fastMap<string, ClassNode>& classes){
    for(auto p : classes){
        if(!visited.contains(p.first)){
            hierarchy[p.first] = dfs(p.first, classes);
        }
    }
    return hierarchy;
}

std::pair<int, int> ComputeClassHierarchyPass::dfs(string className, fastMap<string, ClassNode>& classes){
    visited.insert(className);
    int start = order++;
    for(auto s : classes[className].subclasses){
        hierarchy[s] = dfs(s, classes);
    }
    int end = order;
    return std::make_pair(start, end);
}