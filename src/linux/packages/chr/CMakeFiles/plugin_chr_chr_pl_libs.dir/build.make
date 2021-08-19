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

# Utility rule file for plugin_chr_chr_pl_libs.

# Include the progress variables for this target.
include packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/progress.make

packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_runtime.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_op.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_debug.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_messages.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/pairlist.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/clean_code.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/find.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/a_star.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/binomialheap.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/builtins.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_hashtable_store.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/listmap.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_options.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_utility.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_errors.pl
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs: home/library/chr/chr_integertable_store.pl


home/library/chr/chr_runtime.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../../home/library/chr/chr_runtime.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_runtime.pl ./chr_runtime.pl

home/library/chr/chr_op.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../../home/library/chr/chr_op.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_op.pl ./chr_op.pl

home/library/chr/chr_debug.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../../home/library/chr/chr_debug.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_debug.pl ./chr_debug.pl

home/library/chr/chr_messages.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Generating ../../home/library/chr/chr_messages.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_messages.pl ./chr_messages.pl

home/library/chr/pairlist.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Generating ../../home/library/chr/pairlist.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/pairlist.pl ./pairlist.pl

home/library/chr/clean_code.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Generating ../../home/library/chr/clean_code.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/clean_code.pl ./clean_code.pl

home/library/chr/find.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Generating ../../home/library/chr/find.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/find.pl ./find.pl

home/library/chr/a_star.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Generating ../../home/library/chr/a_star.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/a_star.pl ./a_star.pl

home/library/chr/binomialheap.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Generating ../../home/library/chr/binomialheap.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/binomialheap.pl ./binomialheap.pl

home/library/chr/builtins.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Generating ../../home/library/chr/builtins.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/builtins.pl ./builtins.pl

home/library/chr/chr_hashtable_store.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_11) "Generating ../../home/library/chr/chr_hashtable_store.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_hashtable_store.pl ./chr_hashtable_store.pl

home/library/chr/listmap.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_12) "Generating ../../home/library/chr/listmap.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/listmap.pl ./listmap.pl

home/library/chr/chr_compiler_options.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_13) "Generating ../../home/library/chr/chr_compiler_options.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_compiler_options.pl ./chr_compiler_options.pl

home/library/chr/chr_compiler_utility.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_14) "Generating ../../home/library/chr/chr_compiler_utility.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_compiler_utility.pl ./chr_compiler_utility.pl

home/library/chr/chr_compiler_errors.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_15) "Generating ../../home/library/chr/chr_compiler_errors.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_compiler_errors.pl ./chr_compiler_errors.pl

home/library/chr/chr_integertable_store.pl: home/library/chr/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_16) "Generating ../../home/library/chr/chr_integertable_store.pl"
	cd /home/matthias/rolog/src/linux/home/library/chr && /usr/bin/cmake -E create_symlink ../../../../swipl-devel/packages/chr/chr_integertable_store.pl ./chr_integertable_store.pl

home/library/chr/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_17) "Generating ../../home/library/chr/.created"
	cd /home/matthias/rolog/src/linux/packages/chr && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library/chr
	cd /home/matthias/rolog/src/linux/packages/chr && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/chr/.created

plugin_chr_chr_pl_libs: packages/chr/CMakeFiles/plugin_chr_chr_pl_libs
plugin_chr_chr_pl_libs: home/library/chr/chr_runtime.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_op.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_debug.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_messages.pl
plugin_chr_chr_pl_libs: home/library/chr/pairlist.pl
plugin_chr_chr_pl_libs: home/library/chr/clean_code.pl
plugin_chr_chr_pl_libs: home/library/chr/find.pl
plugin_chr_chr_pl_libs: home/library/chr/a_star.pl
plugin_chr_chr_pl_libs: home/library/chr/binomialheap.pl
plugin_chr_chr_pl_libs: home/library/chr/builtins.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_hashtable_store.pl
plugin_chr_chr_pl_libs: home/library/chr/listmap.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_options.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_utility.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_compiler_errors.pl
plugin_chr_chr_pl_libs: home/library/chr/chr_integertable_store.pl
plugin_chr_chr_pl_libs: home/library/chr/.created
plugin_chr_chr_pl_libs: packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/build.make

.PHONY : plugin_chr_chr_pl_libs

# Rule to build all files generated by this target.
packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/build: plugin_chr_chr_pl_libs

.PHONY : packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/build

packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/chr && $(CMAKE_COMMAND) -P CMakeFiles/plugin_chr_chr_pl_libs.dir/cmake_clean.cmake
.PHONY : packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/clean

packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/chr /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/chr /home/matthias/rolog/src/linux/packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/chr/CMakeFiles/plugin_chr_chr_pl_libs.dir/depend

