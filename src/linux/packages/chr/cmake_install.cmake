# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/chr

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/chr" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_runtime.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_op.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_debug.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_messages.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/pairlist.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/clean_code.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/find.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/a_star.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/binomialheap.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/builtins.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_hashtable_store.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/listmap.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_compiler_options.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_compiler_utility.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_compiler_errors.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/chr_integertable_store.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/chr//chr.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/chr" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/linux/packages/chr//guard_entailment.pl"
    "/home/matthias/rolog/src/linux/packages/chr//chr_translate.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/chr" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//chrfreeze.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//fib.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//gcd.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//primes.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//bool.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//family.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//fibonacci.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//leq.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//listdom.chr"
    "/home/matthias/rolog/src/swipl-devel/packages/chr/Examples//chrdif.chr"
    )
endif()

