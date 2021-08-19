# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/pldoc

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pldoc.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_http.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_latex.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_files.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/pldoc" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_html.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_wiki.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_modes.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_register.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_process.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_index.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_search.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_man.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_library.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/hooks.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_htmlsrc.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_colour.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_util.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_access.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_pack.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/man_index.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/doc_words.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pldoc.css"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pldoc.js"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pllisting.css"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pldoc.sty"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/edit.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/private.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/public.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/reload.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/favicon.ico"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/up.gif"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/source.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/h1-bg.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/pub-bg.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/multi-bg.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/priv-bg.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/h2-bg.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/editpred.png"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/pldoc" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/pldoc/doc_html.qlf")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/pldoc" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/server/README"
    "/home/matthias/rolog/src/swipl-devel/packages/pldoc/server/man_server.pl"
    )
endif()

