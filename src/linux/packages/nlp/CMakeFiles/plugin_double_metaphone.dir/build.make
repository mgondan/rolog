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
include packages/nlp/CMakeFiles/plugin_double_metaphone.dir/depend.make

# Include the progress variables for this target.
include packages/nlp/CMakeFiles/plugin_double_metaphone.dir/progress.make

# Include the compile flags for this target's objects.
include packages/nlp/CMakeFiles/plugin_double_metaphone.dir/flags.make

packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o: packages/nlp/CMakeFiles/plugin_double_metaphone.dir/flags.make
packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o: /home/matthias/rolog/src/swipl-devel/packages/nlp/double_metaphone.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o"
	cd /home/matthias/rolog/src/linux/packages/nlp && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o -c /home/matthias/rolog/src/swipl-devel/packages/nlp/double_metaphone.c

packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.i"
	cd /home/matthias/rolog/src/linux/packages/nlp && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/packages/nlp/double_metaphone.c > CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.i

packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.s"
	cd /home/matthias/rolog/src/linux/packages/nlp && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/packages/nlp/double_metaphone.c -o CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.s

# Object files for target plugin_double_metaphone
plugin_double_metaphone_OBJECTS = \
"CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o"

# External object files for target plugin_double_metaphone
plugin_double_metaphone_EXTERNAL_OBJECTS =

packages/nlp/double_metaphone.so: packages/nlp/CMakeFiles/plugin_double_metaphone.dir/double_metaphone.c.o
packages/nlp/double_metaphone.so: packages/nlp/CMakeFiles/plugin_double_metaphone.dir/build.make
packages/nlp/double_metaphone.so: packages/nlp/CMakeFiles/plugin_double_metaphone.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C shared module double_metaphone.so"
	cd /home/matthias/rolog/src/linux/packages/nlp && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/plugin_double_metaphone.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
packages/nlp/CMakeFiles/plugin_double_metaphone.dir/build: packages/nlp/double_metaphone.so

.PHONY : packages/nlp/CMakeFiles/plugin_double_metaphone.dir/build

packages/nlp/CMakeFiles/plugin_double_metaphone.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/nlp && $(CMAKE_COMMAND) -P CMakeFiles/plugin_double_metaphone.dir/cmake_clean.cmake
.PHONY : packages/nlp/CMakeFiles/plugin_double_metaphone.dir/clean

packages/nlp/CMakeFiles/plugin_double_metaphone.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/nlp /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/nlp /home/matthias/rolog/src/linux/packages/nlp/CMakeFiles/plugin_double_metaphone.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/nlp/CMakeFiles/plugin_double_metaphone.dir/depend

