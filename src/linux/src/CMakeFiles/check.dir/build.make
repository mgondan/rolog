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

# Utility rule file for check.

# Include the progress variables for this target.
include src/CMakeFiles/check.dir/progress.make

src/CMakeFiles/check:
	cd /home/matthias/rolog/src/linux/src && ./swipl -f none --no-packs -q /home/matthias/rolog/src/swipl-devel/src/test.pl

check: src/CMakeFiles/check
check: src/CMakeFiles/check.dir/build.make

.PHONY : check

# Rule to build all files generated by this target.
src/CMakeFiles/check.dir/build: check

.PHONY : src/CMakeFiles/check.dir/build

src/CMakeFiles/check.dir/clean:
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -P CMakeFiles/check.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/check.dir/clean

src/CMakeFiles/check.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/src /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/src /home/matthias/rolog/src/linux/src/CMakeFiles/check.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/check.dir/depend

