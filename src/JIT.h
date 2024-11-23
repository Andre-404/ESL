#pragma once

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/Object/COFF.h"
#include "llvm/Object/SymbolSize.h"

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

std::unique_ptr<llvm::orc::LLJIT> setupJIT(){
    llvm::orc::LLJITBuilder builder;
#ifdef _WIN32
    builder.setObjectLinkingLayerCreator(
            [](llvm::orc::ExecutionSession &ES, const llvm::Triple &TT) {
                auto GetMemMgr = []() {
                    return std::make_unique<llvm::SectionMemoryManager>();
                };
                auto Layer = std::make_unique<llvm::orc::RTDyldObjectLinkingLayer>(
                        ES, std::move(GetMemMgr));

                Layer->setProcessAllSections(true);
                Layer->setOverrideObjectFlagsWithResponsibilityFlags(true);
                Layer->setNotifyLoaded([](llvm::orc::MaterializationResponsibility &R, const llvm::object::ObjectFile &Obj,
                                          const llvm::RuntimeDyld::LoadedObjectInfo &info){
                    if (auto *COFFObj = llvm::dyn_cast<llvm::object::COFFObjectFile>(&Obj)) {
                        RUNTIME_FUNCTION* unwindData = nullptr;
                        size_t entryCount = 0, relAddr = 0;
                        auto symbols = llvm::object::computeSymbolSizes(Obj);
                        for (auto [symbol, symSize]: symbols) {
                            llvm::StringRef name = llvm::cantFail(symbol.getName());
                            uint64_t addr = info.getSectionLoadAddress(*llvm::cantFail(symbol.getSection()));
                            if(name == ".pdata"){
                                unwindData = reinterpret_cast<RUNTIME_FUNCTION *>(addr);
                                entryCount = symSize / sizeof(RUNTIME_FUNCTION);
                            }else if(name == ".text") relAddr = addr;
                        }
                        // TODO: can this fail?
                        if(unwindData) RtlAddFunctionTable(unwindData, entryCount,relAddr);
                    }
                });

                return std::move(Layer);
            });
#endif
    llvm::cantFail(builder.prepareForConstruction());
    std::unique_ptr<llvm::orc::LLJIT> ptr = llvm::cantFail(builder.create());
    auto Mangle = llvm::orc::MangleAndInterner(ptr->getExecutionSession(), ptr->getDataLayout());
#ifdef _WIN32
    auto Sym = llvm::orc::absoluteSymbols({{Mangle("___chkstk_ms"),
                                            llvm::orc::ExecutorSymbolDef(llvm::orc::ExecutorAddr::fromPtr(___chkstk_ms),
                                                              llvm::JITSymbolFlags::Exported |     // Make visible to JIT'd code
                                                              llvm::JITSymbolFlags::Callable |     // It's a function
                                                              llvm::JITSymbolFlags::Absolute)}});
    cantFail(ptr->getMainJITDylib().define(std::move(Sym)));
#endif

    ptr->getMainJITDylib().addGenerator(cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(ptr->getDataLayout().getGlobalPrefix())));
    return std::move(ptr);
}