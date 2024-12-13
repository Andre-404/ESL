#include "SEHFrameRegistrar.h"
#include "windows.h"

namespace llvm::orc {
    using namespace jitlink;
    ExecutorAddr getImageBase(jitlink::LinkGraph &G){
        ExecutorAddr Base;
        for (auto &Sec : G.sections())
            Base = std::min(Base, SectionRange(Sec).getStart());
        return Base;
    }
    Error SEHRegistrarPlugin::AddSEHFrames(jitlink::LinkGraph &G)  {
        if(Section* sec = G.findSectionByName(".pdata")){
            SectionRange SEHFrameRange(*sec);
            Section* base = G.findSectionByName(".text");

            ExecutorAddr imageBase = getImageBase(G);
            uint64_t unwindDataStart = SEHFrameRange.getStart().getValue();
            uint64_t entryCount = SEHFrameRange.getSize() / sizeof(RUNTIME_FUNCTION);
            uint64_t addrBase = imageBase.getValue();
            if(!RtlAddFunctionTable(reinterpret_cast<PRUNTIME_FUNCTION>(unwindDataStart), entryCount, addrBase)){
                return createStringError("Failed to register SEH frames");
            }
            return Error::success();
        }
        return createStringError("Failed to locate the .pdata section");
    }

}