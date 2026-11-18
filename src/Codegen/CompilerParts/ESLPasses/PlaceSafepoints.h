#pragma once
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace esl_passes {
inline bool may_poll(const llvm::Function& F) { return F.hasGC() && !F.isDeclaration(); }

// A function's callees. F is null for the node standing in for every indirect callee
struct call_node { llvm::Function* F; llvm::SmallVector<call_node*, 4> callees; };
}
template <> struct llvm::GraphTraits<esl_passes::call_node*> {
    using NodeRef = esl_passes::call_node*;
    using ChildIteratorType = llvm::SmallVector<esl_passes::call_node*, 4>::iterator;
    static NodeRef getEntryNode(NodeRef n) { return n; }
    static ChildIteratorType child_begin(NodeRef n) { return n->callees.begin(); }
    static ChildIteratorType child_end(NodeRef n) { return n->callees.end(); }
};
namespace esl_passes {

// An indirect call can land in any jitted function whose address escapes
inline llvm::DenseSet<llvm::Function*> on_call_cycle(llvm::Module& M) {
    std::vector<call_node> nodes(1);
    for (auto& F : M) if (!F.isDeclaration()) nodes.push_back({ &F });
    // nodes[0] is keyed by null, which is what an indirect call has as its callee
    llvm::DenseMap<llvm::Function*, call_node*> node;
    // Build the graph that scc analysis will run on
    // Have to build it instead of using LLVM's helpers because of indirect calls
    call_node root { nullptr };
    for (auto& n : nodes) { node[n.F] = &n; root.callees.push_back(&n); }
    for (auto& n : llvm::drop_begin(nodes)) {
        if (may_poll(*n.F) && n.F->hasAddressTaken()) nodes[0].callees.push_back(&n);
        for (auto& I : llvm::instructions(*n.F))
            if (auto call = llvm::dyn_cast<llvm::CallBase>(&I); call && !call->isInlineAsm())
                if (auto callee = node.lookup(call->getCalledFunction())) n.callees.push_back(callee);
    }
    llvm::DenseSet<llvm::Function*> res;
    for (auto scc = llvm::scc_begin(&root); !scc.isAtEnd(); ++scc)
        if (scc.hasCycle())
            for (auto n : *scc) if (n->F && may_poll(*n->F)) res.insert(n->F);
    return res;
}

struct place_safepoints : llvm::PassInfoMixin<place_safepoints> {
    llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM) {
        auto poll = M.getFunction("safepoint_poll");
        if (!poll) return llvm::PreservedAnalyses::all();
        auto entry_polls = on_call_cycle(M);
        auto polls = [&](const llvm::Instruction& I) {
            auto call = llvm::dyn_cast<llvm::CallBase>(&I);
            auto callee = call ? call->getCalledFunction() : nullptr;
            return callee && (callee == poll || entry_polls.contains(callee));
        };
        auto insert = [&](llvm::BasicBlock::iterator before) {
            llvm::CallInst::Create(poll, "", before)->setDebugLoc(before->getDebugLoc());
        };
        auto& FAM = MAM.getResult<llvm::FunctionAnalysisManagerModuleProxy>(M).getManager();
        for (auto& F : M) {
            if (!may_poll(F)) continue;
            if (entry_polls.contains(&F)) insert(F.getEntryBlock().getFirstNonPHIOrDbgOrAlloca());
            llvm::SmallVector<std::pair<const llvm::BasicBlock*, const llvm::BasicBlock*>> edges;
            llvm::FindFunctionBackedges(F, edges);
            if (edges.empty()) continue;
            auto& DT = FAM.getResult<llvm::DominatorTreeAnalysis>(F);
            auto polls_already = [&](const llvm::BasicBlock* latch, const llvm::BasicBlock* header) {
                bool reducible = DT.dominates(header, latch);
                for (auto node = DT.getNode(latch); ; node = node->getIDom()) {
                    if (llvm::any_of(*node->getBlock(), polls)) return true;
                    if (!reducible || node->getBlock() == header) return false;
                }
            };
            // Inner loops first, each poll inserted right away: an inner latch's poll is often on
            // the outer loop's dominator path too
            auto level = [&](auto& edge) { return DT.getNode(edge.second)->getLevel(); };
            llvm::stable_sort(edges, [&](auto& x, auto& y) { return level(x) > level(y); });
            for (auto [latch, header] : edges)
                if (!polls_already(latch, header)) insert(const_cast<llvm::BasicBlock*>(latch)->getTerminator()->getIterator());
        }
        return llvm::PreservedAnalyses::none();
    }
};
}
