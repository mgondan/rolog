# Install script for directory: /home/matthias/rolog/src/swipl-devel

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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/LICENSE"
    "/home/matthias/rolog/src/swipl-devel/README.md"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/customize" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/customize/edit"
    "/home/matthias/rolog/src/swipl-devel/customize/init.pl"
    "/home/matthias/rolog/src/swipl-devel/customize/README.md"
    )
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/matthias/rolog/src/linux/src/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/chr/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/clib/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/clpqr/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/inclpr/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/cpp/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/http/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/language_server/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/ltx2htm/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/nlp/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/paxos/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/redis/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/PDT/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/pengines/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/pldoc/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/plunit/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/protobufs/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/RDF/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/semweb/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/sgml/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/table/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/utf8proc/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/zlib/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/archive/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/bdb/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/pcre/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/yaml/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/ssl/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/libedit/cmake_install.cmake")
  include("/home/matthias/rolog/src/linux/packages/readline/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/matthias/rolog/src/linux/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
