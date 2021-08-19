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

# Utility rule file for plugin_turtle_semweb_pl_libs.

# Include the progress variables for this target.
include packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/progress.make

packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs: home/library/semweb/turtle.pl
packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs: home/library/semweb/rdf_turtle_write.pl


home/library/semweb/turtle.pl: home/library/semweb/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../../home/library/semweb/turtle.pl"
	cd /home/matthias/rolog/src/linux/home/library/semweb && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/semweb/turtle.pl ./turtle.pl

home/library/semweb/rdf_turtle_write.pl: home/library/semweb/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../../home/library/semweb/rdf_turtle_write.pl"
	cd /home/matthias/rolog/src/linux/home/library/semweb && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/semweb/rdf_turtle_write.pl ./rdf_turtle_write.pl

home/library/semweb/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../../home/library/semweb/.created"
	cd /home/matthias/rolog/src/linux/packages/semweb && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library/semweb
	cd /home/matthias/rolog/src/linux/packages/semweb && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/semweb/.created

plugin_turtle_semweb_pl_libs: packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs
plugin_turtle_semweb_pl_libs: home/library/semweb/turtle.pl
plugin_turtle_semweb_pl_libs: home/library/semweb/rdf_turtle_write.pl
plugin_turtle_semweb_pl_libs: home/library/semweb/.created
plugin_turtle_semweb_pl_libs: packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/build.make

.PHONY : plugin_turtle_semweb_pl_libs

# Rule to build all files generated by this target.
packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/build: plugin_turtle_semweb_pl_libs

.PHONY : packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/build

packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/semweb && $(CMAKE_COMMAND) -P CMakeFiles/plugin_turtle_semweb_pl_libs.dir/cmake_clean.cmake
.PHONY : packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/clean

packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/semweb /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/semweb /home/matthias/rolog/src/linux/packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/semweb/CMakeFiles/plugin_turtle_semweb_pl_libs.dir/depend

