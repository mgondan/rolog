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

# Utility rule file for core_library_dcg.

# Include the progress variables for this target.
include src/CMakeFiles/core_library_dcg.dir/progress.make

src/CMakeFiles/core_library_dcg: home/library/dcg/basics.pl
src/CMakeFiles/core_library_dcg: home/library/dcg/high_order.pl


home/library/dcg/basics.pl: home/library/dcg/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../home/library/dcg/basics.pl"
	cd /home/matthias/rolog/src/linux/home/library/dcg && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dcg/basics.pl ./basics.pl

home/library/dcg/high_order.pl: home/library/dcg/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../home/library/dcg/high_order.pl"
	cd /home/matthias/rolog/src/linux/home/library/dcg && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dcg/high_order.pl ./high_order.pl

home/library/dcg/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../home/library/dcg/.created"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library/dcg
	cd /home/matthias/rolog/src/linux/src && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/dcg/.created

core_library_dcg: src/CMakeFiles/core_library_dcg
core_library_dcg: home/library/dcg/basics.pl
core_library_dcg: home/library/dcg/high_order.pl
core_library_dcg: home/library/dcg/.created
core_library_dcg: src/CMakeFiles/core_library_dcg.dir/build.make

.PHONY : core_library_dcg

# Rule to build all files generated by this target.
src/CMakeFiles/core_library_dcg.dir/build: core_library_dcg

.PHONY : src/CMakeFiles/core_library_dcg.dir/build

src/CMakeFiles/core_library_dcg.dir/clean:
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -P CMakeFiles/core_library_dcg.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/core_library_dcg.dir/clean

src/CMakeFiles/core_library_dcg.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/src /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/src /home/matthias/rolog/src/linux/src/CMakeFiles/core_library_dcg.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/core_library_dcg.dir/depend

