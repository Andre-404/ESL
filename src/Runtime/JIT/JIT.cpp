#include "JIT.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/Object/COFF.h"
#include "llvm/Object/SymbolSize.h"
#include "llvm/ExecutionEngine/Orc/EPCEHFrameRegistrar.h"
#include "SEHFrameRegistrar.h"
#include "llvm/ExecutionEngine/JITLink/JITLink.h"

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
            [](llvm::orc::ExecutionSession &ES, const llvm::Triple &TT) {
                auto Layer = std::make_unique<llvm::orc::ObjectLinkingLayer>(ES);

                if(TT.isOSBinFormatCOFF()) {
                    Layer->setOverrideObjectFlagsWithResponsibilityFlags(true);
                    Layer->setAutoClaimResponsibilityForObjectSymbols(true);
                    Layer->addPlugin(std::make_shared<llvm::orc::SEHRegistrarPlugin>());
                }else if(TT.isOSBinFormatELF()){
                    if (auto EHFrameRegistrar = llvm::orc::EPCEHFrameRegistrar::Create(ES))
                        Layer->addPlugin(std::make_unique<llvm::orc::EHFrameRegistrationPlugin>(ES, std::move(*EHFrameRegistrar)));
                }

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
    auto& JD = JIT.underlyingJIT->getMainJITDylib();
    auto G = std::make_unique<llvm::jitlink::LinkGraph>(
            string("ImageBaseGraph"), TT,
            TT.isArch64Bit() ? 8 : 4,
            TT.isLittleEndian() ? llvm::endianness::little : llvm::endianness::big,
            llvm::jitlink::getGenericEdgeKindName);
    auto &Sec = G->createSection(string(".rodata"), llvm::orc::MemProt::Read);
    auto content = G->allocateBuffer(8);
    *((uint64_t*)(&content.front())) = 0x4000000;
    auto &B = G->createContentBlock(Sec, content, llvm::orc::ExecutorAddr(), 8, 0);
    G->addDefinedSymbol(B, 0, string("__ImageBase"), 1, llvm::jitlink::Linkage::Strong, llvm::jitlink::Scope::Default,
            /* Callable = */ false, /* Live = */ true);
    llvm::cantFail(static_cast<llvm::orc::ObjectLinkingLayer&>(JIT.underlyingJIT->getObjLinkingLayer()).add(JD, std::move(G)));
    if (auto Err = JIT.underlyingJIT->lookup(JD, "__ImageBase")){

    }

    // Hack around the ___chkstk_ms routine no being defined
    auto Mangle = llvm::orc::MangleAndInterner(JIT.underlyingJIT->getExecutionSession(), JIT.underlyingJIT->getDataLayout());
    auto Sym = llvm::orc::absoluteSymbols({{Mangle("___chkstk_ms"),
                                            llvm::orc::ExecutorSymbolDef(llvm::orc::ExecutorAddr::fromPtr(___chkstk_ms),
                                                                         llvm::JITSymbolFlags::Exported |     // Make visible to JIT'd code
                                                                         llvm::JITSymbolFlags::Callable |     // It's a function
                                                                         llvm::JITSymbolFlags::Absolute)}});
    cantFail(JIT.underlyingJIT->getMainJITDylib().define(std::move(Sym)));

    // This is a remnant from the kaleidoscope tutorial, is it needed here after the switch to LLJIT?
    JIT.underlyingJIT->getMainJITDylib().addGenerator(
            cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
                    JIT.underlyingJIT->getDataLayout().getGlobalPrefix())));
}

MainFn ESLJIT::getMainFunc(){
    llvm::orc::ExecutorAddr ExprSymbol = llvm::ExitOnError()(underlyingJIT->lookup("func.main"));
    llvm::DIDumpOptions opts;
    opts.AddrSize = 8;
    opts.Version = 3;
    opts.Verbose = true;
    global->dwarfContext->dump(llvm::errs(), opts);
    return ExprSymbol.toPtr<MainFn>();
}


string ESLJIT::addressToFunc(uint64_t address){

    return dwarfContext->getLineInfoForAddress({address-startAddress, llvm::object::SectionedAddress::UndefSection}).FunctionName;
}