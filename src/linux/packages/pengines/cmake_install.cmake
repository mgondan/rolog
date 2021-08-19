# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/pengines

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
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/pengines.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/pengines_io.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/term_to_json.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/pengines_sandbox.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/pengines/http/term_html.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http/web/js" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/pengines/web/js/pengines.js")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http/web/css" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/pengines/web/css/plterm.css")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/pengines" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/client.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/server.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/pengines/web" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//chunking.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//debugging.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//hack.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//index.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//input_output.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//pengine.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//queen.png"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//queens.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//simple.html"
    "/home/matthias/rolog/src/swipl-devel/packages/pengines/examples/web//update-jquery"
    )
endif()

