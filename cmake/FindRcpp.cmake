# - Try to find Rcpp
# Once done this will define
#
#  RCPP_INCLUDE_DIR - the Rcpp include directory

IF(R_EXECUTABLE)
  EXECUTE_PROCESS(
	  COMMAND ${R_EXECUTABLE} "--slave" "--no-save" "-e" "cat(system.file('include', package='Rcpp'))"
    OUTPUT_VARIABLE RCPP_INCLUDE_DIR)
  IF(RCPP_INCLUDE_DIR)
    MESSAGE(STATUS "RCPP_INCLUDE_DIR at ${RCPP_INCLUDE_DIR}")
  ELSE()
    MESSAGE(STATUS "Could not determine RCPP_INCLUDE_DIR.")
    SET(ABORT_CONFIG TRUE)
  ENDIF()
ENDIF()
