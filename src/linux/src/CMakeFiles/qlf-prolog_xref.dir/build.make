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

# Utility rule file for qlf-prolog_xref.

# Include the progress variables for this target.
include src/CMakeFiles/qlf-prolog_xref.dir/progress.make

src/CMakeFiles/qlf-prolog_xref: home/library/prolog_xref.qlf


home/library/prolog_xref.qlf: home/library/prolog_xref.pl
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "QLF compiling prolog_xref.qlf"
	cd /home/matthias/rolog/src/linux/src && ./swipl -f none --no-packs -t halt --home=/home/matthias/rolog/src/linux/home -g "use_module(library(prolog_install))" -g "cmake_qcompile" -- --compile /home/matthias/rolog/src/linux/home/library/prolog_xref --qlfdeps /home/matthias/rolog/src/linux/home/library/prolog_xref.pl

home/library/prolog_xref.pl: home/library/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../home/library/prolog_xref.pl"
	cd /home/matthias/rolog/src/linux/home/library && /usr/bin/cmake -E create_symlink ../../../swipl-devel/library/prolog_xref.pl ./prolog_xref.pl

home/library/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../home/library/.created"
	cd /home/matthias/rolog/src/linux/src && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library
	cd /home/matthias/rolog/src/linux/src && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/.created

qlf-prolog_xref: src/CMakeFiles/qlf-prolog_xref
qlf-prolog_xref: home/library/prolog_xref.qlf
qlf-prolog_xref: home/library/prolog_xref.pl
qlf-prolog_xref: home/library/.created
qlf-prolog_xref: src/CMakeFiles/qlf-prolog_xref.dir/build.make

.PHONY : qlf-prolog_xref

# Rule to build all files generated by this target.
src/CMakeFiles/qlf-prolog_xref.dir/build: qlf-prolog_xref

.PHONY : src/CMakeFiles/qlf-prolog_xref.dir/build

src/CMakeFiles/qlf-prolog_xref.dir/clean:
	cd /home/matthias/rolog/src/linux/src && $(CMAKE_COMMAND) -P CMakeFiles/qlf-prolog_xref.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/qlf-prolog_xref.dir/clean

src/CMakeFiles/qlf-prolog_xref.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/src /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/src /home/matthias/rolog/src/linux/src/CMakeFiles/qlf-prolog_xref.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/qlf-prolog_xref.dir/depend

