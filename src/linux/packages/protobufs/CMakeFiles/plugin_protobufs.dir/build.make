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
include packages/protobufs/CMakeFiles/plugin_protobufs.dir/depend.make

# Include the progress variables for this target.
include packages/protobufs/CMakeFiles/plugin_protobufs.dir/progress.make

# Include the compile flags for this target's objects.
include packages/protobufs/CMakeFiles/plugin_protobufs.dir/flags.make

packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.o: packages/protobufs/CMakeFiles/plugin_protobufs.dir/flags.make
packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.o: /home/matthias/rolog/src/swipl-devel/packages/protobufs/protobufs.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.o"
	cd /home/matthias/rolog/src/linux/packages/protobufs && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/plugin_protobufs.dir/protobufs.c.o -c /home/matthias/rolog/src/swipl-devel/packages/protobufs/protobufs.c

packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/plugin_protobufs.dir/protobufs.c.i"
	cd /home/matthias/rolog/src/linux/packages/protobufs && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/packages/protobufs/protobufs.c > CMakeFiles/plugin_protobufs.dir/protobufs.c.i

packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/plugin_protobufs.dir/protobufs.c.s"
	cd /home/matthias/rolog/src/linux/packages/protobufs && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/packages/protobufs/protobufs.c -o CMakeFiles/plugin_protobufs.dir/protobufs.c.s

# Object files for target plugin_protobufs
plugin_protobufs_OBJECTS = \
"CMakeFiles/plugin_protobufs.dir/protobufs.c.o"

# External object files for target plugin_protobufs
plugin_protobufs_EXTERNAL_OBJECTS =

packages/protobufs/protobufs.so: packages/protobufs/CMakeFiles/plugin_protobufs.dir/protobufs.c.o
packages/protobufs/protobufs.so: packages/protobufs/CMakeFiles/plugin_protobufs.dir/build.make
packages/protobufs/protobufs.so: packages/protobufs/CMakeFiles/plugin_protobufs.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C shared module protobufs.so"
	cd /home/matthias/rolog/src/linux/packages/protobufs && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/plugin_protobufs.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
packages/protobufs/CMakeFiles/plugin_protobufs.dir/build: packages/protobufs/protobufs.so

.PHONY : packages/protobufs/CMakeFiles/plugin_protobufs.dir/build

packages/protobufs/CMakeFiles/plugin_protobufs.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/protobufs && $(CMAKE_COMMAND) -P CMakeFiles/plugin_protobufs.dir/cmake_clean.cmake
.PHONY : packages/protobufs/CMakeFiles/plugin_protobufs.dir/clean

packages/protobufs/CMakeFiles/plugin_protobufs.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/protobufs /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/protobufs /home/matthias/rolog/src/linux/packages/protobufs/CMakeFiles/plugin_protobufs.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/protobufs/CMakeFiles/plugin_protobufs.dir/depend

