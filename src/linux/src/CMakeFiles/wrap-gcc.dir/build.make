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

# Include any dependencies generated for this target.
include src/CMakeFiles/wrap-gcc.dir/depend.make

# Include the progress variables for this target.
include src/CMakeFiles/wrap-gcc.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/wrap-gcc.dir/flags.make

src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o: src/CMakeFiles/wrap-gcc.dir/flags.make
src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o: /home/matthias/rolog/src/swipl-devel/src/wrap-gcc.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o -c /home/matthias/rolog/src/swipl-devel/src/wrap-gcc.c

src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/wrap-gcc.dir/wrap-gcc.c.i"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/src/wrap-gcc.c > CMakeFiles/wrap-gcc.dir/wrap-gcc.c.i

src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/wrap-gcc.dir/wrap-gcc.c.s"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/src/wrap-gcc.c -o CMakeFiles/wrap-gcc.dir/wrap-gcc.c.s

# Object files for target wrap-gcc
wrap__gcc_OBJECTS = \
"CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o"

# External object files for target wrap-gcc
wrap__gcc_EXTERNAL_OBJECTS =

src/wrap-gcc: src/CMakeFiles/wrap-gcc.dir/wrap-gcc.c.o
src/wrap-gcc: src/CMakeFiles/wrap-gcc.dir/build.make
src/wrap-gcc: src/CMakeFiles/wrap-gcc.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable wrap-gcc"
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/wrap-gcc.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/wrap-gcc.dir/build: src/wrap-gcc

.PHONY : src/CMakeFiles/wrap-gcc.dir/build

src/CMakeFiles/wrap-gcc.dir/clean:
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -P CMakeFiles/wrap-gcc.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/wrap-gcc.dir/clean

src/CMakeFiles/wrap-gcc.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/src /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/src /home/matthias/rolog/src/linux/src/CMakeFiles/wrap-gcc.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/wrap-gcc.dir/depend

