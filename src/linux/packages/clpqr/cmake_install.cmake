# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/clpqr

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp/clpq" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//bb_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//bv_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//fourmotz_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//ineq_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//itf_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//nf_q.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpq//store_q.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp/clpr" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//bb_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//bv_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//fourmotz_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//ineq_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//itf_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//nf_r.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpr//store_r.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp/clpqr" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//class.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//dump.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//geler.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//itf.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//ordering.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//project.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//redund.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/clpqr/clpqr//highlight.pl"
    )
endif()

