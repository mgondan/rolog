# Load rolog.dll/rolog.so on startup
# 
# This cannot be delegated to a useDynLib directive in NAMESPACE. The reason is
# that rolog.so itself is able to load other packages, and therefore exports 
# a number of prolog-specific symbols. The additional option "local=FALSE" makes
# sure these symbols are imported on startup. This option is not available in 
# if we use useDynLib in NAMESPACE.
#
.onLoad = function(libname, pkgname)
{
  library.dynam(chname='rolog', package=pkgname, lib.loc=libname, local=FALSE)
  return(TRUE)
}

.onUnload = function(libpath)
{
  library.dynam.unload('rolog', libpath=libpath)
  return(TRUE)
}

# This is a bit of a mystery.
#
# Initialization of the rolog system works fine under linux, under Windows using
# RStudio.exe, under Windows using RTerm.exe, but fails under RGui.exe (the 
# "blue R"). Even stranger, it works in the second attempt. 
#
# For this reason, I invoke rolog_init twice here. Any hint to a cleaner
# solution is highly appreciated.
#
.onAttach = function(libname, pkgname)
{
  if(rolog_init())
    return(TRUE)
  
  # Try again
  if(rolog_init())
    return(TRUE)
  
  # Give up
  stop('rolog: initialization of swipl failed.')  
}

.onDetach = function(libpath)
{
  if(!rolog_done())
    stop('rolog: not initialized')
}

rolog_init = function(argv1=commandArgs()[1])
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
}

consult = function(fname=system.file('likes.pl', package='rolog'))
{
  consult_(fname)
}

once = function(query=call('member', expression(X), list(1, 2, 3)))
{
  once_(call)
}

findall = function(query=call('member', expression(X), list(1, 2, 3)))
{
  findall_(predicate)
}
