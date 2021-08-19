# Install script for directory: /home/matthias/rolog/src/swipl-devel/packages/http

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/html_write.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_client.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_header.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_sgml_plugin.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/mimepack.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/mimetype.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/dcg_basics.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_wrapper.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_open.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_proxy.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_error.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_parameters.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_dispatch.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_authenticate.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_log.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_path.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_hook.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/html_head.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_exception.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_dirindex.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_server_files.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_pwp.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_host.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/js_write.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/js_grammar.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_cookie.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_files.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_cors.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/yadis.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/ax.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/html_quasiquotations.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_load.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_multipart_plugin.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_digest.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_redis_plugin.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/README.md"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/http/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/http/http_stream.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/http_stream.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/packages/http/http_stream.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/http/json.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/json.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/json.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/json_convert.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/http_json.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE MODULE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/http/websocket.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux/websocket.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/websocket.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/hub.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http/web/css" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/css/dirindex.css"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/css/openid.css"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http/web/icons" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//back.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//compressed.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//c.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//folder.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//generic.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//layout.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//openid-logo-square.png"
    "/home/matthias/rolog/src/swipl-devel/packages/http/web/icons//openid-logo-tiny.png"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_packagesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/http" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/packages/http/jquery.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/http" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//README.md"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_body.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_client.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_threads.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//calc.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_files.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_pwp.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_openid.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_daemon.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//upstart-script.conf"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//systemd-script.service"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//linux-init-script"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_login.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_rest.pl"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples//demo_hello.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xExamplesx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/doc/packages/examples/http/pwp" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//context.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//index.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp1.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp2.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp3.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp4.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp5.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp6.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp7.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwp8.pwp"
    "/home/matthias/rolog/src/swipl-devel/packages/http/examples/pwp//pwpdb.pl"
    )
endif()

