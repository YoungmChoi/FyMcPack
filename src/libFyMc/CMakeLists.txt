set(
FYMCPACK_SOURCES
    src/mfpGlobal.f90
    src/general/mfpGeneral.f90
    src/geometry/mfpGeometry.f90
    src/specialFunction/mfpSpecialFunction.f90
)

add_library(fymc STATIC ${FYMCPACK_SOURCES})
target_include_directories(fymc PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
set_target_properties(fymc
    PROPERTIES COMPILE_FLAGS "-cpp"
)

install(TARGETS fymc DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
