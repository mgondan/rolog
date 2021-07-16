.onLoad = function(libname, pkgname)
{
  # local=FALSE is needed under Linux
  library.dynam(chname="rolog", package=pkgname, lib.loc=libname, local=FALSE)
  return(TRUE)
}

.onUnload = function(libpath)
{
  library.dynam.unload("rolog", libpath=libpath, verbose=TRUE)
  return(TRUE)
}

.onAttach = function(libname, pkgname)
{
  if(rolog_init(commandArgs()[1]))
    return(TRUE) ;
  
  print("Try again")  
  rolog_init(commandArgs()[1])
}

.onDetach = function(libpath)
{
  if(!rolog_done())
    stop("rolog: not initialized")  
}

rolog_init = function(argv1)
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
}

rolog_consult = function(fname='likes.pl')
{
  consult_(fname)
}

rolog_once = function(call=quote(consult('likes')))
{
  once_(call)
}

rolog_findall = function(predicate=quote(likes(sam)))
{
  findall_(predicate)
}
