#pragma once

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

    void addressToFunc(uint64_t address);

private:
    vector<std::pair<std::unique_ptr<llvm::DWARFContext>, llvm::StringMap<std::unique_ptr<llvm::MemoryBuffer>>>> dwarfContext;
    std::unique_ptr<llvm::orc::LLJIT> underlyingJIT;
    static ESLJIT* global;
};