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

# Utility rule file for plugin_http_css_http_web_css_pl_libs.

# Include the progress variables for this target.
include packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/progress.make

packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs: home/library/http/web/css/dirindex.css
packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs: home/library/http/web/css/openid.css


home/library/http/web/css/dirindex.css: home/library/http/web/css/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating ../../home/library/http/web/css/dirindex.css"
	cd /home/matthias/rolog/src/linux/home/library/http/web/css && /usr/bin/cmake -E create_symlink ../../../../../../swipl-devel/packages/http/web/css/dirindex.css ./dirindex.css

home/library/http/web/css/openid.css: home/library/http/web/css/.created
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating ../../home/library/http/web/css/openid.css"
	cd /home/matthias/rolog/src/linux/home/library/http/web/css && /usr/bin/cmake -E create_symlink ../../../../../../swipl-devel/packages/http/web/css/openid.css ./openid.css

home/library/http/web/css/.created:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/matthias/rolog/src/linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating ../../home/library/http/web/css/.created"
	cd /home/matthias/rolog/src/linux/packages/http && /usr/bin/cmake -E make_directory /home/matthias/rolog/src/linux/home/library/http/web/css
	cd /home/matthias/rolog/src/linux/packages/http && touch -t 200001010000 /home/matthias/rolog/src/linux/home/library/http/web/css/.created

plugin_http_css_http_web_css_pl_libs: packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs
plugin_http_css_http_web_css_pl_libs: home/library/http/web/css/dirindex.css
plugin_http_css_http_web_css_pl_libs: home/library/http/web/css/openid.css
plugin_http_css_http_web_css_pl_libs: home/library/http/web/css/.created
plugin_http_css_http_web_css_pl_libs: packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/build.make

.PHONY : plugin_http_css_http_web_css_pl_libs

# Rule to build all files generated by this target.
packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/build: plugin_http_css_http_web_css_pl_libs

.PHONY : packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/build

packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/clean:
	cd /home/matthias/rolog/src/linux/packages/http && $(CMAKE_COMMAND) -P CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/cmake_clean.cmake
.PHONY : packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/clean

packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/depend:
	cd /home/matthias/rolog/src/linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/matthias/rolog/src/swipl-devel /home/matthias/rolog/src/swipl-devel/packages/http /home/matthias/rolog/src/linux /home/matthias/rolog/src/linux/packages/http /home/matthias/rolog/src/linux/packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : packages/http/CMakeFiles/plugin_http_css_http_web_css_pl_libs.dir/depend

