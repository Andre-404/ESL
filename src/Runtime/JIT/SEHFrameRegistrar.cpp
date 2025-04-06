#include "SEHFrameRegistrar.h"
#include "windows.h"

namespace llvm::orc {
    using namespace jitlink;
    Error SEHRegistrarPlugin::AddSEHFrames(MaterializationResponsibility &MR, LinkGraph &G)  {
        if(Section* sec = G.findSectionByName(".pdata")){
            SectionRange SEHFrameRange(*sec);
            // Never fails because __ImageBase needs to be defined by for the JIT to function before any pass runs
            // TODO: think of a cleaner way to do this without needing MR
            auto imageBase = ExitOnError()(MR.getExecutionSession().lookup(
                    {&MR.getTargetJITDylib()}, "__ImageBase"));
            uint64_t unwindDataStart = SEHFrameRange.getStart().getValue();
            uint64_t entryCount = SEHFrameRange.getSize() / sizeof(RUNTIME_FUNCTION);
            uint64_t addrBase = imageBase.getAddress().getValue();
            if(!RtlAddFunctionTable(reinterpret_cast<PRUNTIME_FUNCTION>(unwindDataStart), entryCount, addrBase)){
                return createStringError("Failed to register SEH frames");
            }
        }
        return Error::success();
    }

}