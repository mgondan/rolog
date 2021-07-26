# Load rolog.dll/rolog.so on startup
# 
# This cannot be delegated to a useDynLib directive in NAMESPACE (at least not
# under linux). The reason is that rolog.so itself is able to load other 
# packages (i.e. prolog libraries), and therefore exports a number of 
# prolog-specific symbols. The additional option "local=FALSE" makes sure these
# symbols are imported on startup. This option is not available in if we use
# useDynLib in NAMESPACE.
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
# RStudio.exe, under Windows using RTerm.exe, but fails under RGui.exe (aka. 
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

#' rolog_init
#'
#' SWI prolog is automatically initialized when the rolog library is loaded, so
#' this function is normally not directly invoked.
#' 
#' @param argv1: file name of the R executable
#' @return TRUE on success
#' 
rolog_init = function(argv1=commandArgs()[1])
{
  init_(argv1)
}

#' rolog_done
#'
#' Clean up when detaching the library. At this stage, this function is of
#' little practical use, since it is not yet possible to initialize prolog
#' twice in the same R session. See the source file rolog.cpp for details.
#' 
#' @return `TRUE` on success
#' @md
#' 
rolog_done = function()
{
  done_()
}

#' consult
#'
#' Consult a prolog database
#' 
#' @param fname: file name of database
#' @return `TRUE` on success
#' @md
#'
#' @seealso [once()] and [findall()] for executing queries
#' 
#' @examples
#' consult(fname=system.file("likes.pl", package="rolog"))
#' findall(call("likes", quote(sam), expression(X)))
#' 
consult = function(fname=system.file('likes.pl', package='rolog'))
{
  consult_(fname)
}

#' portray
#'
#' Translate an R call to a prolog compound and pretty print it
#' 
#' @param query: an R call
#' @return a character string with the prolog version of the call
#' @md
#'
#' @details
#' R to prolog
#' 
#' * numeric -> real
#' * integer -> integer
#' * character -> string
#' * symbol/name -> atom
#' * call/language -> compound
#' * expression -> variable
#' * boolean -> true, false (atoms)
#' 
portray = function(query=call('member', expression(X), list(1, 2, 3)))
{
  portray_(query)
}

#' Invoke a query once
#'
#' @param query: an R call
#' @param portray: boolean, add the prolog translation as an attribute
#' @return `FALSE` if the query fails; otherwise, a list with conditions
#' @md
#' 
#' @seealso [findall()] for querying all solutions
#' @seealso [portray()] for pretty-printing a query
#'
#' @examples
#' once(call("=", 1, 2)) # FALSE 
#' once(call("=", 1, 1)) # empty list
#' once(call("member", 1, list(2, expression(X)))) # list stating that it works if X = 1
#' once(call("=", list(expression(X), expression(Y)), list(1, expression(Z)))) # list stating that X = 1 and Z = Y
#' once(call("member", 1, expression(X))) # works for X = [1 | _]; i.e. something like [|](1, expression(_6330))
#'
once = function(query=call('member', expression(X), list(1, 2, 3)), portray=TRUE)
{
  r = once_(query)

  if(portray)
    attr(r, 'query') = portray(query)
  
  return(r)
}

#' Invoke a query several times
#'
#' @param query: an R call
#' @param portray: boolean, add the prolog translation as an attribute
#' @return empty list if the query fails; otherwise, a list of conditions for each solution
#' @md
#'
#' @seealso [once()] for a single query
#' 
#' @examples
#' findall(call("member", expression(X), list(1, 2, 3)))
#' 
findall = function(query=call('member', expression(X), list(1, 2, 3)), portray=TRUE)
{
  r = findall_(query)
  
  if(portray)
    attr(r, 'query') = portray(query)
  
  return(r)
}
