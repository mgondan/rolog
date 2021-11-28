# Load rolog.dll/rolog.so on startup
# 
# This cannot be delegated to a useDynLib directive in NAMESPACE (at least not
# under linux). The reason is that rolog.so itself is able to load other 
# packages (i.e. prolog libraries), and therefore exports a number of 
# prolog-specific symbols. The additional option local=FALSE makes sure these
# symbols are imported on startup. This option is not available in if we use
# useDynLib in NAMESPACE.
#
.onLoad <- function(libname, pkgname)
{
  library.dynam(chname='rolog', package=pkgname, lib.loc=libname, local=FALSE)
  
  op.rolog <- list(
    rolog.quote = TRUE,   # accept simplified syntax, use rolog_quote
    rolog.realvec = '#',  # prolog representation of R numeric vectors
    rolog.intvec = '%',   # prolog representation of R integer vectors
    rolog.boolvec = '!',  # prolog representation of R boolean vectors
    rolog.charvec = '$',  # prolog representation of R character vectors
    rolog.portray = TRUE, # return prolog call, nicely formatted
    rolog.scalar = TRUE)  # convert R vectors of size 1 to scalars in Prolog

  set <- !(names(op.rolog) %in% names(options()))
  if(any(set))
    options(op.rolog[set])

  invisible()
}

.onUnload <- function(libpath)
{
  library.dynam.unload('rolog', libpath=libpath)
  invisible()
}

# This is a bit of a mystery.
#
# Initialization of the SWI-Prolog works fine under linux, under Windows using
# RStudio.exe, under Windows using RTerm.exe, but fails under RGui.exe (aka. 
# "blue R"). Even stranger, it works in the second attempt. 
#
# For this reason, I invoke rolog_init twice here. Any hint to a cleaner
# solution is highly appreciated.
#
.onAttach <- function(libname, pkgname)
{
  if(.Platform$OS.type == 'unix')
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, 'swipl', 'lib', 'swipl'))
    if(!rolog_init())
      stop('Rolog: initialization of swipl failed.')  
  }

  if(.Platform$OS.type == 'windows')
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, 'swipl'))
    if(!rolog_init() && !rolog_init())
      stop('Rolog: initialization of swipl failed.')  
  }
  
  # SWI startup message
  W <- once(message_to_string(welcome), W))
  packageStartupMessage(W$W)
  invisible()
}

.onDetach <- function(libpath)
{
  if(.Platform$OS.type == 'unix')
    Sys.unsetenv('SWI_HOME_DIR')
  
  if(.Platform$OS.type == 'windows')
    Sys.unsetenv('SWI_HOME_DIR')

  # In case there are open queries, clear them
  clear()
  
  if(!rolog_done())
    stop('Rolog: not initialized')
}

#' Start prolog
#'
#' @param argv1
#' file name of the R executable
#'
#' @return
#' `TRUE` on success
#' 
#' @details 
#' SWI-prolog is automatically initialized when the rolog library is loaded, so
#' this function is normally not directly invoked.
#'
rolog_init <- function(argv1=commandArgs()[1])
{
  .init(argv1)
}

#' Clean up when detaching the library
#' 
#' @return
#' `TRUE` on success
#'
#' @md
#' 
#' @details
#' At this stage, this function is of little practical use, since it is not yet
#' possible to initialize prolog twice in the same R session. See the source file
#' rolog.cpp for details.
#' 
rolog_done = function()
{
  .done()
}

#' Consult a prolog database
#' 
#' @param fname
#' file name of database
#'
#' @return
#' `TRUE` on success
#'
#' @md
#'
#' @seealso
#' [once()], [findall()], and [query()]/[submit()]/[clear()] for executing queries
#' 
#' @examples
#' consult(fname=system.file('likes.pl', package='rolog'))
#' findall(likes(sam, X))
#' 
consult = function(fname=system.file('likes.pl', package='rolog'))
{
  .consult(fname)
}

#' Quick access to Rolog's own options
#' 
#' @return
#' list with some options for translating R expressions to prolog 
#'
#' @md
# 
#' @details
#' Translation of R to Prolog
#' 
#' * `quote`: if TRUE (default), use simplified syntax (see [query()])
#' * numeric vector of size _N_ -> `realvec`/N (default is #)
#' * integer vector of size _N_ -> `intvec`/N (default is %)
#' * boolean vector of size _N_ -> `boolvec`/N (default is !)
#' * character vector of size _N_ -> `charvec`/N (default is $$)
#' * `scalar`: if TRUE (default), translate R vectors of length 1 to scalars
#' * `portray`: whether to return the prolog translation as an attribute to 
#'   the return value of once, query and findall (default is TRUE)
#'
rolog_options = function()
{
  list(
    quote=getOption('rolog.quote', default=TRUE),
    realvec=getOption('rolog.realvec', default='#'),
    intvec=getOption('rolog.intvec', default='%'),
    boolvec=getOption('rolog.boolvec', default='!'),
    charvec=getOption('rolog.charvec', default='$$'),
    portray=getOption('rolog.portray', default=TRUE),
    scalar=getOption('rolog.scalar', default=TRUE))
}

#' Translate an R call to a prolog compound and pretty print it
#' 
#' @param query 
#' an R call. The R call consists of symbols, integers and real numbers, 
#' character strings, boolean values, expressions and lists, and other calls.
#' Vectors of booleans, integers, floating point numbers, and strings with
#' length _N_ > 1 are translated to prolog compounds !/N, %/N, #/N and $$/N,
#' respectively. The names can be modified with the options below.
#'
#' @param options
#' This is a list of options controlling translation from and to prolog.
#' * if _quote_ is `TRUE` (default), the query is translated to its
#'   canonical form using `rolog_quote`.
#' * _boolvec_ (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for vectors of booleans.
#' * _intvec_, _realvec_, _charvec_ define the compound names for vectors of
#'   integers, doubles and strings, respectively (defaults are %, # and $$).
#' * If _scalar_ is `TRUE` (default), vectors of length 1 are translated to 
#'   scalar prolog elements. If _scalar_ is `FALSE`, vectors of length 1 are
#'   also translated to compounds.
#'
#' @return
#' character string with the prolog syntax of the call
#'
#' @md
#'
#' @details
#' If `options$quote` is `TRUE` (default), then R elements are translated
#' to the following prolog citizens:
#' 
#' * numeric -> real (vectors of size _N_ -> #/N)
#' * integer -> integer (vectors -> %/N)
#' * character -> string (vectors -> $$/N)
#' * symbol/name starting with lowercase -> atom
#' * symbol/name starting with uppercase or _ -> variable
#' * call/language -> compound
#' * boolean -> true, false (vectors -> !/N)
#' * list -> list
#' * call/language with the name "list" -> list
#'
#' If `options$quote` is `FALSE`, the following rules apply:
#' 
#' * numeric -> real
#' * integer -> integer (vectors of size _N_ -> %/N)
#' * character -> string (vectors -> $$/N)
#' * symbol/name -> atom
#' * expression -> variable
#' * call/language -> compound
#' * boolean -> true, false (atoms)
#' * list -> list
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
portray = function(query=member(X, list(a, "b", 3L, 4, TRUE, Y)), options=NULL)
{
  options = c(options, rolog_options())
  
  # Check if simplified syntax is used
  if(options$quote)
    query = rolog_quote(query)

  .portray(query, options)
}

#' Invoke a query once
#'
#' @param query 
#' an R call. The R call consists of symbols, integers and real numbers, 
#' character strings, boolean values, expressions, lists, and other calls.
#' Vectors of booleans, integers, floating point numbers, and strings with
#' length _N_ > 1 are translated to prolog compounds !/N, %/N, #/N and $$/N,
#' respectively. The names can be modified with the options below.
#'   
#' @param options
#' This is a list of options controlling translation from and to prolog.
#' * if _quote_ is `TRUE` (default), the query is translated to its
#'   canonical form using `rolog_quote`.
#' * _boolvec_ (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for vectors of booleans.
#' * _intvec_, _realvec_, _charvec_ define the compound names for vectors of
#'   integers, doubles and strings, respectively (defaults are %, # and $$).
#' * If _scalar_ is `TRUE` (default), vectors of length 1 are translated to 
#'   scalar prolog elements. If _scalar_ is `FALSE`, vectors of length 1 are
#'   also translated to compounds.
#'   
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'
#' @md
#' 
#' @seealso [findall()]
#' for querying all solutions
#' 
#' @seealso [query()], [submit()], and [clear()] for fine-grained control over
#' non-deterministic queryies
#' 
#' @seealso [rolog_options()]
#' for options controlling R to prolog translation
#'
#' @examples
#' 
#' # This query returns FALSE
#' once(1 = 2)
#' 
#' # This query returns an empty list meaning yes, it works
#' once(1 = 1)
#' 
#' # This query returns a list stating that it works if X = 1
#' once(member(1, list(a, X))
#' 
#' # Same query in canonical form, without intermediate call to prolog_quote()
#' once(call("member", 1, list(a, expression(X))), options=list(quote=FALSE))
#' 
#' # This query returns a list stating that X = 1 and Z = expression(Y)
#' once(list(X, Y) = list(1, Z))
#' 
#' # This works for X = [1 | _]; i.e. something like [|](1, expression(_6330))
#' once(member(1, X))
#'
#' # This returns S = '1.0' (scalar)
#' once(format(string(S), "~w", list(1)), options=list(scalar=TRUE))
#'   
#' # This returns S = '#(1.0)' (vector)
#' once(format(string(S), "~w", list(1)), options=list(scalar=FALSE))
#'
once = function(query=member(X, list(a, "b", 3L, 4, TRUE, Y)), options=NULL)
{
  options = c(options, rolog_options())
  
  # Translate to canonical form
  # member(X, list(1, 2, 3)) => call("member", expression(X), list(1, 2, 3))
  if(options$quote)
    query = rolog_quote(query)
  
  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q = portray(query, options)

  # Invoke C++ function that calls prolog
  r = .once(query, options)
  
  if(options$portray)
    attr(r, 'query') = q

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
  if(options$portray)
    q = portray(query, options)

  r = .findall(query, options)
  if(options$portray)
    attr(r, 'query') = q
  return(r)
}

#' Create a query
#'
#' @return If the creation of the query succeeds, TRUE.
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
#' @md
#'
#' @seealso [once()] for a single query
#' 
#' @examples
#' query(call('member', expression(X), list(1, 2, 3)))
#' query_submit() # 1
#' query_submit() # 2
#' query_submit() # 3
#' query_submit() # fails
#' query_close()
#' 
query = function(query=call('member', expression(X), list(1, 2, 3)), options=NULL)
{
  options = c(options, rolog_options())
  if(options$portray)
    q = portray(query, options)

  r = .query(query, options)
  if(options$portray)
    attr(r, 'query') = q
  
  return(r)
}

#' Close current query
#'
#' @return TRUE (invisible)
#'
#' @md
#'
#' @seealso [once()] for a single query
#'
#' @examples
#' query(call('member', expression(X), list(1, 2, 3)))
#' query_submit() # 1
#' query_submit() # 2
#' query_submit() # 3
#' query_submit() # fails
#' query_close()
#'
query_close = function()
{
  invisible(.query_close())
}

#' Submit a query
#'
#' @return If a solution is found, TRUE.
#'   
#' @md
#'
#' @seealso [once()] for a single query
#' 
#' @examples
#' submit()
#' 
submit = function()
{
  r = .submit()
  return(r)
}
