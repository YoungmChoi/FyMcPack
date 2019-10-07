#... Specify the minimum version for CMake
cmake_minimum_required(VERSION 2.6)

###-------------------------------------------------------------------------###
#... Project Name

set(PROJECT_NAME FyMc)                         #... Project Name
set(COMPILE_LANGUAGE Fortran)
if (NOT CMAKE_Fortran_COMPILER) 
    set(CMAKE_Fortran_COMPILER gfortran)
    #set(CMAKE_Fortran_COMPILER ifort)
endif()
###-------------------------------------------------------------------------###

project(${PROJECT_NAME} ${COMPILE_LANGUAGE})        # PROJECT NAME

###-------------------------------------------------------------------------###

set(PROJECT_COMPILE_PATH ${CMAKE_HOME_DIRECTORY})   #... COMPILE_PATH
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)
###-------------------------------------------------------------------------###
if(NOT WIN32)
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(ColourBold  "${Esc}[1m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
  set(BoldRed     "${Esc}[1;31m")
  set(BoldGreen   "${Esc}[1;32m")
  set(BoldYellow  "${Esc}[1;33m")
  set(BoldBlue    "${Esc}[1;34m")
  set(BoldMagenta "${Esc}[1;35m")
  set(BoldCyan    "${Esc}[1;36m")
  set(BoldWhite   "${Esc}[1;37m")
endif()
###-------------------------------------------------------------------------###

add_subdirectory(src/libFyMc)
add_subdirectory(src/example)
