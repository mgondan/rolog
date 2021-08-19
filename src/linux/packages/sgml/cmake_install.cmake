# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/sgml

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
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/sgml/sgml2pl.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/sgml2pl.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/sgml.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/xsdp_types.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/iso_639.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/sgml_write.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/xpath.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/pwp.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/c14n2.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/DTD" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTML4.dcl"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTML4.dtd"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTML4.soc"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTML5.dtd"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTMLlat1.ent"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTMLspec.ent"
    "/home/matthias/rolog/src/swipl-devel/packages/sgml/DTD//HTMLsym.ent"
    )
endif()

