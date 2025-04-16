#include "DebugInfoPlugin.h"

#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/ExecutionEngine/Orc/Core.h"

using namespace llvm;

// Taken from llvm/ExecutionEngine/Orc/DebugInfoSupport since it won't compile here for some reason?
static SmallVector<char, 0> getSectionData(jitlink::Section &Sec) {
    SmallVector<char, 0> SecData;
    SmallVector<jitlink::Block *, 8> SecBlocks(Sec.blocks().begin(), Sec.blocks().end());
    std::sort(SecBlocks.begin(), SecBlocks.end(), [](jitlink::Block *LHS, jitlink::Block *RHS) {
        return LHS->getAddress() < RHS->getAddress();
    });
    for (auto *Block : SecBlocks) {
        if (Block->isZeroFill())
            SecData.resize(SecData.size() + Block->getSize(), 0);
        else
            SecData.append(Block->getContent().begin(), Block->getContent().end());
    }
    return SecData;
}

std::pair<std::unique_ptr<DWARFContext>, StringMap<std::unique_ptr<MemoryBuffer>>>
llvm::orc::createDWARFContext(jitlink::LinkGraph &G) {
    StringMap<std::unique_ptr<MemoryBuffer>> DWARFSectionData;
    for (auto &Sec : G.sections()) {
        if (Sec.getName().find(".debug_") == 0) {
            auto SecData = getSectionData(Sec);
            auto Name = Sec.getName();
            // DWARFContext expects the section name to not start with a dot
            Name.consume_front(".");
            DWARFSectionData[Name] = std::make_unique<SmallVectorMemoryBuffer>(std::move(SecData));
        }
    }
    auto Ctx = DWARFContext::create(DWARFSectionData, G.getPointerSize(),
                                 G.getEndianness() == llvm::endianness::little);
    return std::make_pair(std::move(Ctx), std::move(DWARFSectionData));
}

