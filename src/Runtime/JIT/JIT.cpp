#include "JIT.h"

#include <ranges>

#include "llvm/ExecutionEngine/Orc/EHFrameRegistrationPlugin.h"
#include "llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h"
#include "DebugInfoPlugin.h"

ESLJIT* ESLJIT::global = nullptr;

void ESLJIT::createJIT(){
    global = new ESLJIT();
    ESLJIT& JIT = *global;
    llvm::orc::LLJITBuilder builder;

    builder.setObjectLinkingLayerCreator(
            [&JIT](llvm::orc::ExecutionSession &ES, auto& memmng) {
                auto MemMgr =
                        llvm::orc::MapperJITLinkMemoryManager::CreateWithMapper<llvm::orc::InProcessMemoryMapper>(
                                /* Slab size, e.g. 1Gb */ 1024 * 1024 * 1024);
                if (!MemMgr) exit(1);

                auto Layer = std::make_unique<llvm::orc::ObjectLinkingLayer>(ES, std::move(*MemMgr));

                Layer->addPlugin(std::make_shared<llvm::orc::DebugInfoPlugin>(
                        [&](std::pair<std::unique_ptr<llvm::DWARFContext>, llvm::StringMap<std::unique_ptr<llvm::MemoryBuffer>>> ctx){
                            JIT.dwarfContext.push_back(std::move(ctx));
                        }));
                return std::move(Layer);

            });

    auto JTMB = llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost());
    if (!JTMB.getCodeModel())
        JTMB.setCodeModel(llvm::CodeModel::Small);
    JTMB.setRelocationModel(llvm::Reloc::PIC_);
    JTMB.addFeatures({"reserve-r15"});
    builder.setJITTargetMachineBuilder(JTMB);
    llvm::cantFail(builder.prepareForConstruction());
    JIT.underlyingJIT = llvm::cantFail(builder.create());
}

MainFn ESLJIT::getMainFunc(){
    llvm::orc::ExecutorAddr ExprSymbol = llvm::ExitOnError()(underlyingJIT->lookup("func.main"));
    return ExprSymbol.toPtr<MainFn>();
}

using FnKind = llvm::DINameKind;
using FileKind = llvm::DILineInfoSpecifier::FileLineInfoKind;

void ESLJIT::addressToFunc(uint64_t ip){
    ip--;
    for(auto &ctx: dwarfContext | std::views::keys){
        llvm::DILineInfoSpecifier specifier(FileKind::AbsoluteFilePath, FnKind::ShortName, true);
        auto InlineInfo = ctx->getInliningInfoForAddress({ip, llvm::object::SectionedAddress::UndefSection}, specifier);
        int num = InlineInfo.getNumberOfFrames();
        for(int i = 0; i < num; i++){
            auto& frame = InlineInfo.getFrame(i);
            std::cout<<"File "<<frame.FileName<<", line "<<frame.Line<<", in "<<frame.FunctionName<<(i < num-1 ? "(inlined)" : "")<<"\n";
        }
    }
}