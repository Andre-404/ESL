#include "JIT.h"
#include "llvm/ExecutionEngine/Orc/EPCEHFrameRegistrar.h"
#include "SEHFrameRegistrar.h"
#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h"
#include "DebugInfoPlugin.h"
#include "llvm/DebugInfo/DWARF/DWARFCompileUnit.h"

ESLJIT* ESLJIT::global = nullptr;
// JIT needs this if an alloca happens and for some reason it doesn't pick it up from libgcc
#ifdef _WIN32
#include "windows.h"
extern "C" {
#if defined(__x86_64__)
void ___chkstk_ms(uint64_t);
#elif defined(__aarch64__)
void __chkstk(uint64_t);
#elif defined(__arm__)
    void __chkstk(uint32_t);
#endif
}
#endif

void ESLJIT::createJIT(){
    global = new ESLJIT();
    ESLJIT& JIT = *global;
    llvm::orc::LLJITBuilder builder;

    builder.setObjectLinkingLayerCreator(
            [&JIT](llvm::orc::ExecutionSession &ES, const llvm::Triple &TT) {

                auto MemMgr =
                        llvm::orc::MapperJITLinkMemoryManager::CreateWithMapper<llvm::orc::InProcessMemoryMapper>(
                                /* Slab size, e.g. 1Gb */ 1024 * 1024 * 1024);
                if (!MemMgr) exit(1);

                auto Layer = std::make_unique<llvm::orc::ObjectLinkingLayer>(ES, std::move(*MemMgr));

                if(TT.isOSBinFormatCOFF()) {
                    Layer->setOverrideObjectFlagsWithResponsibilityFlags(true);
                    Layer->setAutoClaimResponsibilityForObjectSymbols(true);
                    Layer->addPlugin(std::make_shared<llvm::orc::SEHRegistrarPlugin>());
                }else if(TT.isOSBinFormatELF()){
                    if (auto EHFrameRegistrar = llvm::orc::EPCEHFrameRegistrar::Create(ES))
                        Layer->addPlugin(std::make_unique<llvm::orc::EHFrameRegistrationPlugin>(ES, std::move(*EHFrameRegistrar)));
                }
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
    builder.setJITTargetMachineBuilder(JTMB);
    llvm::cantFail(builder.prepareForConstruction());
    JIT.underlyingJIT = llvm::cantFail(builder.create());

    auto& ES = JIT.underlyingJIT->getExecutionSession();
    auto &TT = JIT.underlyingJIT->getExecutionSession().getTargetTriple();

    // Hack around the ___chkstk_ms routine not being defined
    auto Mangle = llvm::orc::MangleAndInterner(JIT.underlyingJIT->getExecutionSession(), JIT.underlyingJIT->getDataLayout());
    llvm::orc::SymbolMap symbolMap;
    symbolMap[Mangle("___chkstk_ms")] = llvm::orc::ExecutorSymbolDef(llvm::orc::ExecutorAddr::fromPtr(___chkstk_ms),
                                                                     llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable );
    auto Sym = llvm::orc::absoluteSymbols(symbolMap);
    // Define in platformdylib so that main jitdylib can find it because resolution never looks at current dylib for exported symbols
    cantFail(JIT.underlyingJIT->getPlatformJITDylib()->define(std::move(Sym)));
    // TODO: this wont be necessary soon
    llvm::orc::ExecutorAddr ExprSymbolTemp = llvm::ExitOnError()(JIT.underlyingJIT->lookup("__ImageBase"));
}

MainFn ESLJIT::getMainFunc(){
    llvm::orc::ExecutorAddr ExprSymbol = llvm::ExitOnError()(underlyingJIT->lookup("func.main"));
    return ExprSymbol.toPtr<MainFn>();
}

using FnKind = llvm::DINameKind;
using FileKind = llvm::DILineInfoSpecifier::FileLineInfoKind;

void ESLJIT::addressToFunc(uint64_t address){
    for(auto& [ctx, mem] : dwarfContext){
        llvm::DILineInfoSpecifier specifier(FileKind::AbsoluteFilePath, FnKind::ShortName);
        auto InlineInfo = ctx->getInliningInfoForAddress(
                {address, llvm::object::SectionedAddress::UndefSection}, specifier);
        int num = InlineInfo.getNumberOfFrames();
        for(int i = 0; i < num; i++){
            auto& frame = InlineInfo.getFrame(i);
            std::cout<<"File "<<frame.FileName<<", line "<<frame.Line<<", in "<<frame.FunctionName<<(i < num-1 ? "(inlined)" : "")<<"\n";
        }
    }
}