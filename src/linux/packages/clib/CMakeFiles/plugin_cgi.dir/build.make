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
include packages/clib/CMakeFiles/plugin_cgi.dir/depend.make

# Include the progress variables for this target.
include packages/clib/CMakeFiles/plugin_cgi.dir/progress.make

# Include the compile flags for this target's objects.
include packages/clib/CMakeFiles/plugin_cgi.dir/flags.make

packages/clib/CMakeFiles/plugin_cgi.dir/error.c.o: packages/clib/CMakeFiles/plugin_cgi.dir/flags.make
packages/clib/CMakeFiles/plugin_cgi.dir/error.c.o: /home/matthias/rolog/src/swipl-devel/packages/clib/error.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object packages/clib/CMakeFiles/plugin_cgi.dir/error.c.o"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/plugin_cgi.dir/error.c.o -c /home/matthias/rolog/src/swipl-devel/packages/clib/error.c

packages/clib/CMakeFiles/plugin_cgi.dir/error.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/plugin_cgi.dir/error.c.i"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/packages/clib/error.c > CMakeFiles/plugin_cgi.dir/error.c.i

packages/clib/CMakeFiles/plugin_cgi.dir/error.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/plugin_cgi.dir/error.c.s"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/packages/clib/error.c -o CMakeFiles/plugin_cgi.dir/error.c.s

packages/clib/CMakeFiles/plugin_cgi.dir/form.c.o: packages/clib/CMakeFiles/plugin_cgi.dir/flags.make
packages/clib/CMakeFiles/plugin_cgi.dir/form.c.o: /home/matthias/rolog/src/swipl-devel/packages/clib/form.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building C object packages/clib/CMakeFiles/plugin_cgi.dir/form.c.o"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/plugin_cgi.dir/form.c.o -c /home/matthias/rolog/src/swipl-devel/packages/clib/form.c

packages/clib/CMakeFiles/plugin_cgi.dir/form.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/plugin_cgi.dir/form.c.i"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/packages/clib/form.c > CMakeFiles/plugin_cgi.dir/form.c.i

packages/clib/CMakeFiles/plugin_cgi.dir/form.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/plugin_cgi.dir/form.c.s"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/packages/clib/form.c -o CMakeFiles/plugin_cgi.dir/form.c.s

packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.o: packages/clib/CMakeFiles/plugin_cgi.dir/flags.make
packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.o: /home/matthias/rolog/src/swipl-devel/packages/clib/cgi.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building C object packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.o"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/plugin_cgi.dir/cgi.c.o -c /home/matthias/rolog/src/swipl-devel/packages/clib/cgi.c

packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/plugin_cgi.dir/cgi.c.i"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/matthias/rolog/src/swipl-devel/packages/clib/cgi.c > CMakeFiles/plugin_cgi.dir/cgi.c.i

packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/plugin_cgi.dir/cgi.c.s"
	cd /home/matthias/rolog/src/linux/packages/clib && /usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/matthias/rolog/src/swipl-devel/packages/clib/cgi.c -o CMakeFiles/plugin_cgi.dir/cgi.c.s

# Object files for target plugin_cgi
plugin_cgi_OBJECTS = \
"CMakeFiles/plugin_cgi.dir/error.c.o" \
"CMakeFiles/plugin_cgi.dir/form.c.o" \
"CMakeFiles/plugin_cgi.dir/cgi.c.o"

# External object files for target plugin_cgi
plugin_cgi_EXTERNAL_OBJECTS =

packages/clib/cgi.so: packages/clib/CMakeFiles/plugin_cgi.dir/error.c.o
packages/clib/cgi.so: packages/clib/CMakeFiles/plugin_cgi.dir/form.c.o
packages/clib/cgi.so: packages/clib/CMakeFiles/plugin_cgi.dir/cgi.c.o
packages/clib/cgi.so: packages/clib/CMakeFiles/plugin_cgi.dir/build.make
packages/clib/cgi.so: packages/clib/CMakeFiles/plugin_cgi.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking C shared module cgi.so"
	cd /home/matthias/rolog/src/linux/packages/clib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/plugin_cgi.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
packages/clib/CMakeFiles/plugin_cgi.dir/build: packages/clib/cgi.so

.PHONY : packages/clib/CMakeFiles/plugin_cgi.dir/build

packages/clib/CMakeFiles/plugin_cgi.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/clib && $(CMAKE_COMMAND) -P CMakeFiles/plugin_cgi.dir/cmake_clean.cmake
.PHONY : packages/clib/CMakeFiles/plugin_cgi.dir/clean

packages/clib/CMakeFiles/plugin_cgi.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/clib /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/clib /home/matthias/rolog/src/linux/packages/clib/CMakeFiles/plugin_cgi.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/clib/CMakeFiles/plugin_cgi.dir/depend

