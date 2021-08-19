# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/protobufs

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
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/protobufs/protobufs.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/protobufs.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/protobufs/protobufs.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/protobufs" TYPE PROGRAM MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/protobufs/bootstrap/protoc-gen-swipl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/protobufs/protoc_gen_prolog_pb/google/protobuf" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/protobufs/bootstrap/protoc_gen_prolog_pb/google/protobuf/descriptor_pb.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/protobufs/protoc_gen_prolog_pb/google/protobuf" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/protobufs/protoc_gen_prolog_pb/google/protobuf/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/protobufs/protoc_gen_prolog_pb/google/protobuf/compiler" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/protobufs/bootstrap/protoc_gen_prolog_pb/google/protobuf/compiler/plugin_pb.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/protobufs/protoc_gen_prolog_pb/google/protobuf/compiler" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/protobufs/protoc_gen_prolog_pb/google/protobuf/compiler/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/protobufs" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/some_message.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/some_message.py"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/eventually_implies.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/protobufs/demo" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/demo/README.md"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/demo/Makefile"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/demo/foo.cpp"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/demo/pb_vector.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/demo/vector_demo.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/protobufs/interop" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/README.md"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/Makefile"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/addressbook.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/addressbook2.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/addressbook.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test2.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test2b.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_read.cc"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_read.py"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_interop.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_templates.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_write.cc"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/test_write.py"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/google/protobuf/unittest_import.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/google/protobuf/unittest.proto"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/interop/google/protobuf/unittest_import_public.proto"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/protobufs/bootstrap" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/bootstrap/README.md"
    "/home/matthias/rolog/src/swipl-devel/packages/protobufs/bootstrap/common.mk"
    )
endif()

