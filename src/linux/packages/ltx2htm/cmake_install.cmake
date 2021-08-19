# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/ltx2htm

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/ltx2htm/tex.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/tex.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/latex2html" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/latex2html.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/latex.cmd"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/sty_pldoc.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/pldoc.cmd"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/sty_xpce.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/xpce.cmd"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/latex2html.css"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/latex2html/icons" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//home.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//index.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//info.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//next.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//prev.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//up.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/ltx2htm/icons//yellow_pages.gif"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/bin" TYPE PROGRAM MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/ltx2htm/latex2html")
endif()

