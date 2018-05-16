set(
EXE_TESTDICT
    testDict/main.f90
)

set(LIB_FYMCPATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

add_executable(testDict ${EXE_TESTDICT})

target_include_directories(testDict PRIVATE
    ${LIB_FYMCPATH}
)

target_link_libraries(testDict
    fymc
)

install(TARGETS testDict DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
