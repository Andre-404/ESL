#include "Optimizer.h"
#include "RuntimeBridge.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"

namespace {
// Lets the helpers marked late_inline_attr be inlined, once the optimizer is done moving them around/merging them
struct expand_gc_barriers : llvm::PassInfoMixin<expand_gc_barriers> {
    llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager&) {
        for (auto& F : M) {
            if (!F.hasFnAttribute(runtime_bridge::late_inline_attr)) continue;
            F.removeFnAttr(llvm::Attribute::NoInline);
            F.addFnAttr(llvm::Attribute::AlwaysInline);
        }
        return llvm::PreservedAnalyses::none();
    }
};

// LLVM only sees a slot_read_attr call as an argmem read of unknown size, so it couldn't tell a
// read of one field apart from a store to the next one. This gives AA the exact 8 bytes
class slot_read_aa : public llvm::AAResultBase {
    static std::optional<llvm::MemoryLocation> slot(const llvm::CallBase* call) {
        auto F = call->getCalledFunction();
        if (!F || !F->hasFnAttribute(runtime_bridge::slot_read_attr) || call->arg_size() != 1) return std::nullopt;
        return llvm::MemoryLocation(call->getArgOperand(0), llvm::LocationSize::precise(8), call->getAAMetadata());
    }
public:
    using llvm::AAResultBase::getModRefInfo;
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase* call, const llvm::MemoryLocation& loc, llvm::AAQueryInfo& AAQI) {
        auto s = slot(call);
        if (!s) return llvm::ModRefInfo::ModRef;
        return AAQI.AAR.alias(*s, loc, AAQI, call) == llvm::AliasResult::NoAlias ? llvm::ModRefInfo::NoModRef : llvm::ModRefInfo::Ref;
    }
    // What c1 does to the memory c2 accesses
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase* c1, const llvm::CallBase* c2, llvm::AAQueryInfo& AAQI) {
        if (auto s = slot(c2)) return AAQI.AAR.getModRefInfo(c1, *s, AAQI) & llvm::ModRefInfo::Mod;
        if (auto s = slot(c1)) return llvm::isModSet(AAQI.AAR.getModRefInfo(c2, *s, AAQI)) ? llvm::ModRefInfo::Ref : llvm::ModRefInfo::NoModRef;
        return llvm::ModRefInfo::ModRef;
    }
    bool invalidate(llvm::Function&, const llvm::PreservedAnalyses&, llvm::FunctionAnalysisManager::Invalidator&) { return false; }
};
struct slot_read_analysis : llvm::AnalysisInfoMixin<slot_read_analysis> {
    using Result = slot_read_aa;
    slot_read_aa run(llvm::Function&, llvm::FunctionAnalysisManager&) { return slot_read_aa(); }
    static inline llvm::AnalysisKey Key;
};

bool may_poll(const llvm::Function& F) { return F.hasGC() && !F.isDeclaration(); }

// A function's callees. F is null for the node standing in for every indirect callee
struct call_node { llvm::Function* F; llvm::SmallVector<call_node*, 4> callees; };
}
template <> struct llvm::GraphTraits<call_node*> {
    using NodeRef = call_node*;
    using ChildIteratorType = llvm::SmallVector<call_node*, 4>::iterator;
    static NodeRef getEntryNode(NodeRef n) { return n; }
    static ChildIteratorType child_begin(NodeRef n) { return n->callees.begin(); }
    static ChildIteratorType child_end(NodeRef n) { return n->callees.end(); }
};
namespace {

// An indirect call can land in any jitted function whose address escapes
llvm::DenseSet<llvm::Function*> on_call_cycle(llvm::Module& M) {
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

void add_gc_passes(llvm::ModulePassManager& MPM) {
    MPM.addPass(place_safepoints());
    MPM.addPass(expand_gc_barriers());
    MPM.addPass(llvm::AlwaysInlinerPass());
    llvm::FunctionPassManager FPM;
    // Stores can only be forwarded into a read once it is a load again
    FPM.addPass(llvm::GVNPass());
    FPM.addPass(llvm::InstCombinePass());
    FPM.addPass(llvm::SimplifyCFGPass());
    MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}
}

void optimize(llvm::Module& module) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    // Before registerFunctionAnalyses, which keeps the first AAManager registered
    FAM.registerPass([] { return slot_read_analysis(); });
    FAM.registerPass([&] {
        auto AA = PB.buildDefaultAAPipeline();
        AA.registerFunctionAnalysis<slot_read_analysis>();
        return AA;
    });
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    add_gc_passes(MPM);
    MPM.run(module, MAM);
}
