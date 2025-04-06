#pragma once

#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

namespace llvm::orc {
    class SEHRegistrarPlugin : public ObjectLinkingLayer::Plugin {
    public:
        void modifyPassConfig(MaterializationResponsibility &MR, jitlink::LinkGraph &G, jitlink::PassConfiguration &Config) override{
            Config.PostFixupPasses.push_back([&](jitlink::LinkGraph& G){
                return AddSEHFrames(MR, G);
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
        static Error AddSEHFrames(MaterializationResponsibility &MR, jitlink::LinkGraph &G);
    };

}