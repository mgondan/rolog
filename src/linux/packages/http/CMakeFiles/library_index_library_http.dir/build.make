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

# Utility rule file for library_index_library_http.

# Include the progress variables for this target.
include packages/http/CMakeFiles/library_index_library_http.dir/progress.make

packages/http/CMakeFiles/library_index_library_http: home/library/http/INDEX.pl
packages/http/CMakeFiles/library_index_library_http: home/library/http/__INDEX.pl


home/library/http/INDEX.pl:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Build home/library/http/INDEX.pl"
	cd /home/matthias/rolog/src/linux/packages/http && ../../src/swipl -f none --no-packs -t halt --home=/home/matthias/rolog/src/linux/home -q -g "make_library_index('/home/matthias/rolog/src/linux/home/library/http')" --

home/library/http/__INDEX.pl: home/library/http/INDEX.pl
	@$(CMAKE_COMMAND) -E touch_nocreate home/library/http/__INDEX.pl

library_index_library_http: packages/http/CMakeFiles/library_index_library_http
library_index_library_http: home/library/http/INDEX.pl
library_index_library_http: home/library/http/__INDEX.pl
library_index_library_http: packages/http/CMakeFiles/library_index_library_http.dir/build.make

.PHONY : library_index_library_http

# Rule to build all files generated by this target.
packages/http/CMakeFiles/library_index_library_http.dir/build: library_index_library_http

.PHONY : packages/http/CMakeFiles/library_index_library_http.dir/build

packages/http/CMakeFiles/library_index_library_http.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/http && $(CMAKE_COMMAND) -P CMakeFiles/library_index_library_http.dir/cmake_clean.cmake
.PHONY : packages/http/CMakeFiles/library_index_library_http.dir/clean

packages/http/CMakeFiles/library_index_library_http.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/http /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/http /home/matthias/rolog/src/linux/packages/http/CMakeFiles/library_index_library_http.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/http/CMakeFiles/library_index_library_http.dir/depend

