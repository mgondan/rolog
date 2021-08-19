# Install script for directory: /home/matthias/rolog/src/swipl-devel/src

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/clp/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dcg" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/dcg/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/unicode" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/unicode/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/lynx" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/lynx/INDEX.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/prolog_colour.qlf")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/library/prolog_xref.qlf")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl"
         RPATH "/usr/local/lib/swipl/lib/x86_64-linux")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux" TYPE EXECUTABLE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/swipl")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl"
         OLD_RPATH ":::::::::::::::::::::::::::::::::::::"
         NEW_RPATH "/usr/local/lib/swipl/lib/x86_64-linux")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/lib/x86_64-linux" TYPE STATIC_LIBRARY MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/libswipl.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/home/boot.prc")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/boot" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/boot/init.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/syspred.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/toplevel.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/license.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/bags.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/apply.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/expand.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/dcg.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/history.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/attvar.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/packs.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/dwim.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/rc.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/predopts.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/autoload.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/qlf.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/topvars.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/messages.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/load.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/dicts.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/gc.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/engines.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/iri.pl"
    "/home/matthias/rolog/src/swipl-devel/boot/tabling.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/explain.pl"
    "/home/matthias/rolog/src/swipl-devel/library/sort.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_config.pl"
    "/home/matthias/rolog/src/swipl-devel/library/qsave.pl"
    "/home/matthias/rolog/src/swipl-devel/library/shlib.pl"
    "/home/matthias/rolog/src/swipl-devel/library/statistics.pl"
    "/home/matthias/rolog/src/swipl-devel/library/system.pl"
    "/home/matthias/rolog/src/swipl-devel/library/error.pl"
    "/home/matthias/rolog/src/swipl-devel/library/backcomp.pl"
    "/home/matthias/rolog/src/swipl-devel/library/gensym.pl"
    "/home/matthias/rolog/src/swipl-devel/library/listing.pl"
    "/home/matthias/rolog/src/swipl-devel/library/debug.pl"
    "/home/matthias/rolog/src/swipl-devel/library/vm.pl"
    "/home/matthias/rolog/src/swipl-devel/library/quintus.pl"
    "/home/matthias/rolog/src/swipl-devel/library/edinburgh.pl"
    "/home/matthias/rolog/src/swipl-devel/library/ctypes.pl"
    "/home/matthias/rolog/src/swipl-devel/library/files.pl"
    "/home/matthias/rolog/src/swipl-devel/library/modules.pl"
    "/home/matthias/rolog/src/swipl-devel/library/edit.pl"
    "/home/matthias/rolog/src/swipl-devel/library/shell.pl"
    "/home/matthias/rolog/src/swipl-devel/library/check.pl"
    "/home/matthias/rolog/src/swipl-devel/library/heaps.pl"
    "/home/matthias/rolog/src/swipl-devel/library/console_input.pl"
    "/home/matthias/rolog/src/swipl-devel/library/tty.pl"
    "/home/matthias/rolog/src/swipl-devel/library/readln.pl"
    "/home/matthias/rolog/src/swipl-devel/library/readutil.pl"
    "/home/matthias/rolog/src/swipl-devel/library/make.pl"
    "/home/matthias/rolog/src/swipl-devel/library/hotfix.pl"
    "/home/matthias/rolog/src/swipl-devel/library/option.pl"
    "/home/matthias/rolog/src/swipl-devel/library/oset.pl"
    "/home/matthias/rolog/src/swipl-devel/library/ordsets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/occurs.pl"
    "/home/matthias/rolog/src/swipl-devel/library/lists.pl"
    "/home/matthias/rolog/src/swipl-devel/library/pprint.pl"
    "/home/matthias/rolog/src/swipl-devel/library/atom.pl"
    "/home/matthias/rolog/src/swipl-devel/library/www_browser.pl"
    "/home/matthias/rolog/src/swipl-devel/library/url.pl"
    "/home/matthias/rolog/src/swipl-devel/library/utf8.pl"
    "/home/matthias/rolog/src/swipl-devel/library/main.pl"
    "/home/matthias/rolog/src/swipl-devel/library/assoc.pl"
    "/home/matthias/rolog/src/swipl-devel/library/nb_set.pl"
    "/home/matthias/rolog/src/swipl-devel/library/threadutil.pl"
    "/home/matthias/rolog/src/swipl-devel/library/qpforeign.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dif.pl"
    "/home/matthias/rolog/src/swipl-devel/library/when.pl"
    "/home/matthias/rolog/src/swipl-devel/library/ugraphs.pl"
    "/home/matthias/rolog/src/swipl-devel/library/checklast.pl"
    "/home/matthias/rolog/src/swipl-devel/library/checkselect.pl"
    "/home/matthias/rolog/src/swipl-devel/library/operators.pl"
    "/home/matthias/rolog/src/swipl-devel/library/date.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_stack.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_clause.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_xref.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_source.pl"
    "/home/matthias/rolog/src/swipl-devel/library/broadcast.pl"
    "/home/matthias/rolog/src/swipl-devel/library/pairs.pl"
    "/home/matthias/rolog/src/swipl-devel/library/base64.pl"
    "/home/matthias/rolog/src/swipl-devel/library/record.pl"
    "/home/matthias/rolog/src/swipl-devel/library/rbtrees.pl"
    "/home/matthias/rolog/src/swipl-devel/library/settings.pl"
    "/home/matthias/rolog/src/swipl-devel/library/thread.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect.pl"
    "/home/matthias/rolog/src/swipl-devel/library/apply_macros.pl"
    "/home/matthias/rolog/src/swipl-devel/library/apply.pl"
    "/home/matthias/rolog/src/swipl-devel/library/nb_rbtrees.pl"
    "/home/matthias/rolog/src/swipl-devel/library/aggregate.pl"
    "/home/matthias/rolog/src/swipl-devel/library/pure_input.pl"
    "/home/matthias/rolog/src/swipl-devel/library/pio.pl"
    "/home/matthias/rolog/src/swipl-devel/library/thread_pool.pl"
    "/home/matthias/rolog/src/swipl-devel/library/terms.pl"
    "/home/matthias/rolog/src/swipl-devel/library/charsio.pl"
    "/home/matthias/rolog/src/swipl-devel/library/portray_text.pl"
    "/home/matthias/rolog/src/swipl-devel/library/csv.pl"
    "/home/matthias/rolog/src/swipl-devel/library/persistency.pl"
    "/home/matthias/rolog/src/swipl-devel/library/fastrw.pl"
    "/home/matthias/rolog/src/swipl-devel/library/coinduction.pl"
    "/home/matthias/rolog/src/swipl-devel/library/ansi_term.pl"
    "/home/matthias/rolog/src/swipl-devel/library/base32.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_history.pl"
    "/home/matthias/rolog/src/swipl-devel/library/optparse.pl"
    "/home/matthias/rolog/src/swipl-devel/library/arithmetic.pl"
    "/home/matthias/rolog/src/swipl-devel/library/writef.pl"
    "/home/matthias/rolog/src/swipl-devel/library/predicate_options.pl"
    "/home/matthias/rolog/src/swipl-devel/library/random.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_breakpoints.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_autoload.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_colour.pl"
    "/home/matthias/rolog/src/swipl-devel/library/varnumbers.pl"
    "/home/matthias/rolog/src/swipl-devel/library/codesio.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_codewalk.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_pack.pl"
    "/home/matthias/rolog/src/swipl-devel/library/git.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_metainference.pl"
    "/home/matthias/rolog/src/swipl-devel/library/quasi_quotations.pl"
    "/home/matthias/rolog/src/swipl-devel/library/sandbox.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_format.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_install.pl"
    "/home/matthias/rolog/src/swipl-devel/library/check_installation.pl"
    "/home/matthias/rolog/src/swipl-devel/library/solution_sequences.pl"
    "/home/matthias/rolog/src/swipl-devel/library/iostream.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dicts.pl"
    "/home/matthias/rolog/src/swipl-devel/library/yall.pl"
    "/home/matthias/rolog/src/swipl-devel/library/tabling.pl"
    "/home/matthias/rolog/src/swipl-devel/library/lazy_lists.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_jiti.pl"
    "/home/matthias/rolog/src/swipl-devel/library/zip.pl"
    "/home/matthias/rolog/src/swipl-devel/library/obfuscate.pl"
    "/home/matthias/rolog/src/swipl-devel/library/wfs.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_wrap.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_trace.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_code.pl"
    "/home/matthias/rolog/src/swipl-devel/library/intercept.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_deps.pl"
    "/home/matthias/rolog/src/swipl-devel/library/tables.pl"
    "/home/matthias/rolog/src/swipl-devel/library/hashtable.pl"
    "/home/matthias/rolog/src/swipl-devel/library/strings.pl"
    "/home/matthias/rolog/src/swipl-devel/library/increval.pl"
    "/home/matthias/rolog/src/swipl-devel/library/prolog_debug.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/clp" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/clp/bounds.pl"
    "/home/matthias/rolog/src/swipl-devel/library/clp/clp_events.pl"
    "/home/matthias/rolog/src/swipl-devel/library/clp/clp_distinct.pl"
    "/home/matthias/rolog/src/swipl-devel/library/clp/clpfd.pl"
    "/home/matthias/rolog/src/swipl-devel/library/clp/clpb.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dcg" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/dcg/basics.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dcg/high_order.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/unicode" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/unicode/blocks.pl"
    "/home/matthias/rolog/src/swipl-devel/library/unicode/unicode_data.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/lynx" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/lynx/format.pl"
    "/home/matthias/rolog/src/swipl-devel/library/lynx/html_style.pl"
    "/home/matthias/rolog/src/swipl-devel/library/lynx/html_text.pl"
    "/home/matthias/rolog/src/swipl-devel/library/lynx/pldoc_style.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/dialect/bim.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/commons.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/hprolog.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/ifprolog.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/yap.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/swi" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/dialect/swi/syspred_options.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/eclipse" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/dialect/eclipse/test_util_iso.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/hprolog" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/dialect/hprolog/format.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/sicstus" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/arrays.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/block.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/lists.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/ordsets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/README.TXT"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/sockets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/swipl-lfr.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/system.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/terms.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus/timeout.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/sicstus4" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/aggregate.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/between.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/clpfd.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/file_systems.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/lists.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/ordsets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/samsort.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/sets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/sockets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/system.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/terms.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/timeout.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/sicstus4/types.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/iso" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/dialect/iso/iso_predicates.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/yap" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/dialect/yap/README.TXT")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/dialect/xsb" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/README.md"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/source.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/basics.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/machine.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/storage.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/ordsets.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/intern.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/string.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/setof.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/consult.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/curr_sym.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/error_handler.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/lists.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/timed_call.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/standard.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/gpp.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/thread.pl"
    "/home/matthias/rolog/src/swipl-devel/library/dialect/xsb/gensym.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/theme" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/library/theme/auto.pl"
    "/home/matthias/rolog/src/swipl-devel/library/theme/dark.pl"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/library/iri_scheme" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/library/iri_scheme/file.pl")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/demo" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/demo/likes.pl"
    "/home/matthias/rolog/src/swipl-devel/demo/README.md"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/include" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/swipl-devel/src/SWI-Prolog.h"
    "/home/matthias/rolog/src/swipl-devel/src/os/SWI-Stream.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/include/sicstus" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/src/compat/sicstus.h")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/include/Yap" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/src/compat/YapInterface.h")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl" TYPE FILE MESSAGE_NEVER RENAME "swipl.home" FILES "/home/matthias/rolog/src/linux/src/dot.txt")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/bin" TYPE FILE MESSAGE_NEVER RENAME "swipl.home" FILES "/home/matthias/rolog/src/linux/src/dotdot.txt")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl/SWIPLTargets.cmake")
    file(DIFFERENT EXPORT_FILE_CHANGED FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl/SWIPLTargets.cmake"
         "/home/matthias/rolog/src/linux/src/CMakeFiles/Export/lib/cmake/swipl/SWIPLTargets.cmake")
    if(EXPORT_FILE_CHANGED)
      file(GLOB OLD_CONFIG_FILES "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl/SWIPLTargets-*.cmake")
      if(OLD_CONFIG_FILES)
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl/SWIPLTargets.cmake\" will be replaced.  Removing files [${OLD_CONFIG_FILES}].")
        file(REMOVE ${OLD_CONFIG_FILES})
      endif()
    endif()
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/CMakeFiles/Export/lib/cmake/swipl/SWIPLTargets.cmake")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/CMakeFiles/Export/lib/cmake/swipl/SWIPLTargets-relwithdebinfo.cmake")
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/swipl" TYPE FILE MESSAGE_NEVER FILES
    "/home/matthias/rolog/src/linux/src/SWIPLConfig.cmake"
    "/home/matthias/rolog/src/linux/src/SWIPLConfigVersion.cmake"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/pkgconfig" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/swipl.pc")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/man/man1" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/swipl.1")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/man/man1" TYPE FILE MESSAGE_NEVER FILES "/home/matthias/rolog/src/swipl-devel/src/swipl-ld.1")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld"
         RPATH "/usr/local/lib/swipl/lib/x86_64-linux")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux" TYPE EXECUTABLE MESSAGE_NEVER FILES "/home/matthias/rolog/src/linux/src/swipl-ld")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld"
         OLD_RPATH ":::::::::::::::::::::::::::::::::::::"
         NEW_RPATH "/usr/local/lib/swipl/lib/x86_64-linux")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/swipl/bin/x86_64-linux/swipl-ld")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE DIRECTORY MESSAGE_NEVER FILES "")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  EXECUTE_PROCESS(COMMAND ln -sf ../lib/swipl/bin/x86_64-linux/swipl ./swipl
				WORKING_DIRECTORY $ENV{DESTDIR}/usr/local/bin)
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xCore_systemx" OR NOT CMAKE_INSTALL_COMPONENT)
  EXECUTE_PROCESS(COMMAND ln -sf ../lib/swipl/bin/x86_64-linux/swipl-ld ./swipl-ld
				WORKING_DIRECTORY $ENV{DESTDIR}/usr/local/bin)
endif()

