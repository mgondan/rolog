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

# Utility rule file for plugin_inclpr_example_files.

# Include the progress variables for this target.
include packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/progress.make

packages/inclpr/CMakeFiles/plugin_inclpr_example_files: home/doc/packages/examples/inclpr/benchmarks.pl


home/doc/packages/examples/inclpr/benchmarks.pl: home/doc/packages/examples/inclpr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../../home/doc/packages/examples/inclpr/benchmarks.pl"
	cd /home/matthias/rolog/src/linux/home/doc/packages/examples/inclpr && /usr/bin/cmake -E create_symlink ../../../../../../swipl-devel/packages/inclpr/benchmarks.pl ./benchmarks.pl

home/doc/packages/examples/inclpr/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../../home/doc/packages/examples/inclpr/.created"
	cd /home/matthias/rolog/src/linux/packages/inclpr && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/doc/packages/examples/inclpr
	cd /home/matthias/rolog/src/linux/packages/inclpr && touch -t 200001010000 /home/matthias/rolog/src/linux/home/doc/packages/examples/inclpr/.created

plugin_inclpr_example_files: packages/inclpr/CMakeFiles/plugin_inclpr_example_files
plugin_inclpr_example_files: home/doc/packages/examples/inclpr/benchmarks.pl
plugin_inclpr_example_files: home/doc/packages/examples/inclpr/.created
plugin_inclpr_example_files: packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/build.make

.PHONY : plugin_inclpr_example_files

# Rule to build all files generated by this target.
packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/build: plugin_inclpr_example_files

.PHONY : packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/build

packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/inclpr && $(CMAKE_COMMAND) -P CMakeFiles/plugin_inclpr_example_files.dir/cmake_clean.cmake
.PHONY : packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/clean

packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/inclpr /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/inclpr /home/matthias/rolog/src/linux/packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/inclpr/CMakeFiles/plugin_inclpr_example_files.dir/depend

