
set(LIB_FYMCPATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

function(makeFyMcExample FYMC_EXAMPLE_NAME FYMC_EXAMPLE_SOURCE_CODES)

    add_executable(${FYMC_EXAMPLE_NAME}  ${FYMC_EXAMPLE_SOURCE_CODES})
    target_include_directories(${FYMC_EXAMPLE_NAME} PRIVATE ${LIB_FYMCPATH} )
    set_target_properties(${FYMC_EXAMPLE_NAME} PROPERTIES COMPILE_FLAGS "-cpp" )
    target_link_libraries(${FYMC_EXAMPLE_NAME} fymc)

    install(TARGETS ${FYMC_EXAMPLE_NAME} DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

endfunction()

set(
EXE_TESTDICT
    testDict/main.f90
)

set(
EXE_TEST_SPECIAL_FN
    testSpecialFn/main.f90
)

makeFyMcExample(testDict ${EXE_TESTDICT})
makeFyMcExample(testSpecialFn ${EXE_TEST_SPECIAL_FN})
