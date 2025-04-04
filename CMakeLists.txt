cmake_minimum_required(VERSION 3.5)
project(swipl-pack-rolog)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(FindR)
include(FindRcpp)
include(FindRInside)
set(CMAKE_VERBOSE_MAKEFILE ON)
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Include swipl.cmake from the running SWI-Prolog's home
list(INSERT CMAKE_MODULE_PATH 0 $ENV{SWIPL_HOME_DIR}/cmake)
include(swipl)

# Create the library as a CMake module
add_library(rolog MODULE src/rolog.cpp)
target_compile_options(rolog PRIVATE -DPROLOGPACK)
include_directories(${R_INCLUDE_DIR})
include_directories(${RCPP_INCLUDE_DIR})
include_directories(${RINSIDE_INCLUDE_DIR})

# Link the library to SWI-Prolog.  This also removes the `lib` prefix
# from the target on systems that define a common library file prefix
target_link_swipl(rolog)
target_link_directories(rolog PRIVATE ${RINSIDE_LIB_DIR})
target_link_libraries(rolog PRIVATE RInside)
target_link_directories(rolog PRIVATE ${R_LIBRARIES})
target_link_libraries(rolog PRIVATE R)
list(APPEND CMAKE_INSTALL_RPATH ${RINSIDE_LIB_DIR} ${R_LIBRARIES})

# Install the foreign taget. `${swipl_module_dir}` contains the
# directory for installing modules for this architecture.

install(TARGETS rolog
	DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir})

# Run  tests.  This  is  executed   before    the   pack  is  installed.
# swipl_test(name) runs Prolog with the command line below.
#
#    swipl -p foreign=${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir} \
#          -p library=${CMAKE_CURRENT_SOURCE_DIR}/prolog \
#          --on-error=status \
#          -g test_${name} \
#	   -t halt \
#	   ${CMAKE_CURRENT_SOURCE_DIR}/test/test_${name}.pl
#
# This  implies  that  a  test  `name`  must    be  defined  in  a  file
# `test/test_${name}.pl`, which exports a  predicate `test_${name}`. The
# test succeeds if this predicate  succeeds   and  no error messages are
# printed.

enable_testing()
swipl_add_test(rolog)
