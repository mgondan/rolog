# - Try to find RInside
# Once done this will define
#
# RINSIDE_INCLUDE_DIR - the RInside include directory
# RINSIDE_LIB_DIR - the RInside shared library directory

IF(R_EXECUTABLE)
  EXECUTE_PROCESS(
    COMMAND ${R_EXECUTABLE} "--slave" "--no-save" "-e" "cat(system.file('include', package='RInside'))"
    OUTPUT_VARIABLE RINSIDE_INCLUDE_DIR)
  IF(RINSIDE_INCLUDE_DIR)
    MESSAGE(STATUS "RINSIDE_INCLUDE_DIR at ${RINSIDE_INCLUDE_DIR}")
  ELSE()
    MESSAGE(STATUS "Could not determine RINSIDE_INCLUDE_DIR.")
    SET(ABORT_CONFIG TRUE)
  ENDIF()

  EXECUTE_PROCESS(
	  COMMAND ${R_EXECUTABLE} "--slave" "--no-save" "-e" "cat(system.file('lib', package='RInside'))"
    OUTPUT_VARIABLE RINSIDE_LIB_DIR)
  IF(RINSIDE_LIB_DIR)
    IF(WIN32)
      SET(RINSIDE_LIB_DIR ${RINSIDE_LIB_DIR}/x64)
    ENDIF()
    MESSAGE(STATUS "RINSIDE_LIB_DIR at ${RINSIDE_LIB_DIR}")
  ELSE()
    MESSAGE(STATUS "Could not determine RINSIDE_LIB_DIR.")
    SET(ABORT_CONFIG TRUE)
  ENDIF()
ENDIF()
