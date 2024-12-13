#pragma once

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "../../common.h"

using MainFn = int (*)(int, char*);
class ESLJIT{
public:
    static void createJIT();
    static ESLJIT& getJIT() {
        return *global;
    }
    const llvm::DataLayout& getDL(){
        return underlyingJIT->getDataLayout();
    }
    void addIRModule(llvm::orc::ThreadSafeModule module){
        llvm::cantFail(underlyingJIT->addIRModule(std::move(module)));
    }

    MainFn getMainFunc();

    string addressToFunc(uint64_t address);

private:
    std::unique_ptr<llvm::DWARFContext> dwarfContext;
    uint64_t startAddress;
    std::unique_ptr<llvm::orc::LLJIT> underlyingJIT;
    static ESLJIT* global;
};