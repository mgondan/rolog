# - Try to find RInside
# Once done this will define
#
#  RINSIDE_INCLUDE_DIR - the Rcpp include directory

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
    MESSAGE(STATUS "RINSIDE_LIB_DIR at ${RINSIDE_LIB_DIR}")
  ELSE()
    MESSAGE(STATUS "Could not determine RINSIDE_LIB_DIR.")
    SET(ABORT_CONFIG TRUE)
  ENDIF()
ENDIF()
