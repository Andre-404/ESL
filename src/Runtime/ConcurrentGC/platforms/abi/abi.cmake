# Picks the assembly for this target's calling convention (see ../abi.h) and sets GC_ABI_SOURCE to it.
# A new ABI is a file here and a branch below
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64)$" AND NOT WIN32 AND NOT APPLE)
    set(GC_ABI_SOURCE ${CMAKE_CURRENT_LIST_DIR}/x86-64-sysv.S)
else()
    message(FATAL_ERROR "No GC calling-convention assembly for ${CMAKE_SYSTEM_PROCESSOR} on ${CMAKE_SYSTEM_NAME}, "
                        "add one to ${CMAKE_CURRENT_LIST_DIR}")
endif()
