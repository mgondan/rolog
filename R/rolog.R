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
  
  op.rolog = list(
    rolog.realvec = '#',  # prolog representation of R numeric vectors
    rolog.intvec = '%',   # prolog representation of R integer vectors
    rolog.boolvec = '!',  # prolog representation of R boolean vectors
    rolog.charvec = '$',  # prolog representation of R character vectors
    rolog.portray = TRUE, # return prolog call, nicely formatted
    rolog.scalar = TRUE)  # convert R vectors of size 1 to scalars in Prolog

  set = !(names(op.rolog) %in% names(options()))
  if(any(set))
    options(op.rolog[set])

  invisible()
}

.onUnload = function(libpath)
{
  library.dynam.unload('rolog', libpath=libpath)
  invisible()
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
  if(.Platform$OS.type == "unix")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, 'swipl', 'home'))
    if(!rolog_init())
      stop('rolog: initialization of swipl failed.')  
  }

  if(.Platform$OS.type == "windows")
  {
    if(!rolog_init() && !rolog_init())
      stop('rolog: initialization of swipl failed.')  
  }
  
  W = once(call('message_to_string', quote(welcome), expression(W)))$W
  packageStartupMessage(W)
  invisible()
}

.onDetach = function(libpath)
{
  if(.Platform$OS.type == "unix")
    Sys.unsetenv('SWI_HOME_DIR')
  
  if(!rolog_done())
    stop('rolog: not initialized')
}

#' Start prolog
#'
#' @param argv1 file name of the R executable
#' @return TRUE on success
#' 
#' @details 
#' SWI-prolog is automatically initialized when the rolog library is loaded, so
#' this function is normally not directly invoked.
#'
rolog_init = function(argv1=commandArgs()[1])
{
  .init(argv1)
}

#' Clean up when detaching the library
#' 
#' @return `TRUE` on success
#' @md
#' 
#' @details
#' At this stage, this function is of
#' little practical use, since it is not yet possible to initialize prolog
#' twice in the same R session. See the source file rolog.cpp for details.
#' 
rolog_done = function()
{
  .done()
}

#' Consult a prolog database
#' 
#' @param fname file name of database
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
  .consult(fname)
}

#' Quick access to rolog's own options
#' 
#' @return list with some options for translating R expressions to Prolog 
#' @md
# 
#' @details
#' Translation of R to Prolog
#' 
#' * numeric vector of size N -> $realvec/N (default is #)
#' * integer vector of size N -> $intvec/N (default is %)
#' * boolean vector of size N -> $boolvec/n (default is !)
#' * character vector of size N -> $charvec/N (default is $)
#' * $scalar: if TRUE (default), translate R vectors of size 1 to scalars in
#'   Prolog
#' * $portray: whether to return the prolog translation as an attribute to 
#'   once and findall (default is TRUE)
rolog_options = function()
{
  list(
    realvec=getOption('rolog.realvec', default='#'),
    intvec=getOption('rolog.intvec', default='%'),
    boolvec=getOption('rolog.boolvec', default='!'),
    charvec=getOption('rolog.charvec', default='$'),
    portray=getOption('rolog.portray', default=TRUE),
    scalar=getOption('rolog.scalar', default=TRUE))
}

#' Translate an R call to a prolog compound and pretty print it
#' 
#' @param query an R call
#' @param options boolean. See rolog_options
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
#' @seealso [rolog_options()] for options controlling R to prolog translation
#' 
portray = function(query=call('member', expression(X), list(1, 2, 3)), 
  options=NULL)
{
  options = c(options, rolog_options())
  .portray(query, options)
}

#' Invoke a query once
#'
#' @param query an R call, consisting of symbols (= prolog atoms), 
#'   numbers (= prolog numbers), strings (= prolog strings), 
#'   boolean values (= prolog atoms true and false), 
#'   expressions (= prolog variables) and lists (= prolog lists), and other
#'   calls (= prolog compounds). Vectors of booleans, integers, floating point
#'   numbers, and strings with length _N_ > 1 are translated to prolog 
#'   compounds !/N, %/N, #/N and $/N, respectively. The names can be modified
#'   with the options below.
#'
#' @param options list of options controlling translation from and to prolog: 
#'   boolvec (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for boolean vectors. intvec, realvec and charvec define
#'   the compound names for vectors of integers, doubles and strings, 
#'   respectively (defaults are %, # and $). If _scalar_ is TRUE (default), 
#'   vectors of length 1 are translated to scalar prolog elements. If _scalar_
#'   is FALSE, even vectors of length 1 are translated to compounds.
#'   
#' @return If the query fails, an empty list is returned. If the query 
#'   succeeds _N_ >= 1 times, a list of length _N_ is returned, each element
#'   being a list of conditions for each solution.
#'
#' @return `FALSE` if the query fails; otherwise, a list with conditions.
#' 
#' @md
#' 
#' @seealso [findall()] for querying all solutions
#' @seealso [rolog_options()] for options controlling R to prolog translation
#'
#' @examples
#' 
#' # This query returns FALSE
#' once(call("=", 1, 2))
#' 
#' # This query returns an empty list meaning yes, it works
#' once(call("=", 1, 1))
#' 
#' # This query returns a list stating that it works if X = 1
#' once(call("member", 1, list(2, expression(X))))
#' 
#' # This query returns a list stating that X = 1 and Z = Y
#' once(call("=", list(expression(X), expression(Y)), list(1, expression(Z))))
#' 
#' # works for X = [1 | _]; i.e. something like [|](1, expression(_6330))
#' once(call("member", 1, expression(X)))
#'
#' # This works for S = '1.0' (scalar)
#' once(call("format", call("string", expression(S)), quote(`~w`), list(1)), 
#'   options=list(scalar=TRUE))
#'   
#' # This works for S = '#(1.0)' (vector)
#' once(call("format", call("string", expression(S)), quote(`~w`), list(1)), 
#'   options=list(scalar=FALSE))
#'
once = function(query=call('member', expression(X), list(1, 2, 3)), options=NULL)
{
  options = c(options, rolog_options())
  
  r = .once(query, options)
  if(options$portray)
    attr(r, 'query') = portray(query, options)
  return(r)
}

#' Invoke a query several times
#'
#' @param query an R call, consisting of symbols (= prolog atoms), 
#'   numbers (= prolog numbers), strings (= prolog strings), 
#'   boolean values (= prolog atoms true and false), 
#'   expressions (= prolog variables) and lists (= prolog lists), and other
#'   calls (= prolog compounds). Vectors of booleans, integers, floating point
#'   numbers, and strings with length _N_ > 1 are translated to prolog 
#'   compounds !/N, %/N, #/N and $/N, respectively. The names can be modified
#'   with the options below.
#'   
#' @param options list of options controlling translation from and to prolog: 
#'   boolvec (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for boolean vectors. intvec, realvec and charvec define
#'   the compound names for vectors of integers, doubles and strings, 
#'   respectively (defaults are %, # and $). If _scalar_ is TRUE (default), 
#'   vectors of length 1 are translated to scalar prolog elements. If _scalar_
#'   is FALSE, even vectors of length 1 are translated to compounds.
#'   
#' @return If the query fails, an empty list is returned. If the query 
#'   succeeds _N_ >= 1 times, a list of length _N_ is returned, each element
#'   being a list of conditions for each solution.
#'   
#' @md
#'
#' @seealso [once()] for a single query
#' @seealso [rolog_options()]
#' 
#' @examples
#' findall(call("member", expression(X), list(1, 2, 3)))
#' 
findall = function(query=call('member', expression(X), list(1, 2, 3)), options=NULL)
{
  options = c(options, rolog_options())

  r = .findall(query, options)
  if(options$portray)
    attr(r, 'query') = portray(query, options)
  return(r)
}
