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

# Utility rule file for core_library_dialect.

# Include the progress variables for this target.
include src/CMakeFiles/core_library_dialect.dir/progress.make

src/CMakeFiles/core_library_dialect: home/library/dialect/bim.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/commons.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/hprolog.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/ifprolog.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/sicstus.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/sicstus4.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/yap.pl
src/CMakeFiles/core_library_dialect: home/library/dialect/xsb.pl


home/library/dialect/bim.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../home/library/dialect/bim.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/bim.pl ./bim.pl

home/library/dialect/commons.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../home/library/dialect/commons.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/commons.pl ./commons.pl

home/library/dialect/hprolog.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../home/library/dialect/hprolog.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/hprolog.pl ./hprolog.pl

home/library/dialect/ifprolog.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Generating ../home/library/dialect/ifprolog.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/ifprolog.pl ./ifprolog.pl

home/library/dialect/sicstus.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Generating ../home/library/dialect/sicstus.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/sicstus.pl ./sicstus.pl

home/library/dialect/sicstus4.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Generating ../home/library/dialect/sicstus4.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/sicstus4.pl ./sicstus4.pl

home/library/dialect/yap.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Generating ../home/library/dialect/yap.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/yap.pl ./yap.pl

home/library/dialect/xsb.pl: home/library/dialect/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Generating ../home/library/dialect/xsb.pl"
	cd /home/matthias/rolog/src/linux/home/library/dialect && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/library/dialect/xsb.pl ./xsb.pl

home/library/dialect/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Generating ../home/library/dialect/.created"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library/dialect
	cd /home/matthias/rolog/src/linux/src && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/dialect/.created

core_library_dialect: src/CMakeFiles/core_library_dialect
core_library_dialect: home/library/dialect/bim.pl
core_library_dialect: home/library/dialect/commons.pl
core_library_dialect: home/library/dialect/hprolog.pl
core_library_dialect: home/library/dialect/ifprolog.pl
core_library_dialect: home/library/dialect/sicstus.pl
core_library_dialect: home/library/dialect/sicstus4.pl
core_library_dialect: home/library/dialect/yap.pl
core_library_dialect: home/library/dialect/xsb.pl
core_library_dialect: home/library/dialect/.created
core_library_dialect: src/CMakeFiles/core_library_dialect.dir/build.make

.PHONY : core_library_dialect

# Rule to build all files generated by this target.
src/CMakeFiles/core_library_dialect.dir/build: core_library_dialect

.PHONY : src/CMakeFiles/core_library_dialect.dir/build

src/CMakeFiles/core_library_dialect.dir/clean:
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -P CMakeFiles/core_library_dialect.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/core_library_dialect.dir/clean

src/CMakeFiles/core_library_dialect.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/src /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/src /home/matthias/rolog/src/linux/src/CMakeFiles/core_library_dialect.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/core_library_dialect.dir/depend

