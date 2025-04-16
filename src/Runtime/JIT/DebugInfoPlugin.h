#pragma once

#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"

namespace llvm::orc {
    std::pair<std::unique_ptr<DWARFContext>, StringMap<std::unique_ptr<MemoryBuffer>>>
            createDWARFContext(jitlink::LinkGraph &G);
    class DebugInfoPlugin : public ObjectLinkingLayer::Plugin {
    public:
        DebugInfoPlugin(std::function<void(std::pair<std::unique_ptr<DWARFContext>, StringMap<std::unique_ptr<MemoryBuffer>>>)> assignFunc){
            assignContextFn = assignFunc;
        }
        void modifyPassConfig(MaterializationResponsibility &MR, jitlink::LinkGraph &G, jitlink::PassConfiguration &Config) override{
            Config.PrePrunePasses.push_back([](jitlink::LinkGraph& G){
                for (auto &Section : G.sections()) {
                    StringRef SecName = Section.getName();
                    // Every DWARF section should start with this prefix?
                    // Probably better to replace with actual section names
                    if (SecName.find(".debug_") == 0){
                        for (auto sym : Section.symbols())
                            sym->setLive(true);
                    }
                }
                return Error::success();
            });
            Config.PostFixupPasses.push_back([&](jitlink::LinkGraph& G){
                for(auto& sec : G.sections()){
                    StringRef SecName = sec.getName();
                    // Create a DWARFContext object only if we detect debug info in the graph
                    if (SecName.find(".debug_") == 0){
                        assignContextFn(createDWARFContext(G));
                        return Error::success();
                    }
                }
                return Error::success();
            });
        }

        Error notifyEmitted(MaterializationResponsibility &MR) override {
            return Error::success();
        }

        Error notifyFailed(MaterializationResponsibility &MR) override {
            return Error::success();
        }

        Error notifyRemovingResources(JITDylib &JD, ResourceKey K) override {
            return Error::success();
        }

        void notifyTransferringResources(JITDylib &JD, ResourceKey DstKey, ResourceKey SrcKey) override {}
    private:
        std::function<void(std::pair<std::unique_ptr<DWARFContext>, StringMap<std::unique_ptr<MemoryBuffer>>>)>
                assignContextFn;
    };

}
