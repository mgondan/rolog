# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.18

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/matthias/rolog/src/swipl-devel

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/matthias/rolog/src/linux

# Utility rule file for RDF.

# Include the progress variables for this target.
include packages/RDF/CMakeFiles/RDF.dir/progress.make

RDF: packages/RDF/CMakeFiles/RDF.dir/build.make

.PHONY : RDF

# Rule to build all files generated by this target.
packages/RDF/CMakeFiles/RDF.dir/build: RDF

.PHONY : packages/RDF/CMakeFiles/RDF.dir/build

packages/RDF/CMakeFiles/RDF.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/RDF && $(CMAKE_COMMAND) -P CMakeFiles/RDF.dir/cmake_clean.cmake
.PHONY : packages/RDF/CMakeFiles/RDF.dir/clean

packages/RDF/CMakeFiles/RDF.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/RDF /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/RDF /home/matthias/rolog/src/linux/packages/RDF/CMakeFiles/RDF.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/RDF/CMakeFiles/RDF.dir/depend

