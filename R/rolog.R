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
  # Load libswipl.so
  if(.Platform$OS.type == "unix")
  {
    # Find folder like x86_64-linux
    folder <- dir(file.path(libname, pkgname, "swipl", "lib", "swipl", "lib"),
      pattern=R.version$arch, full.names=TRUE)

    # Are we in roxygenize mode?
    if(length(folder) == 0)
    {
      inst <- dir(file.path(libname, pkgname), pattern="inst", full.names=FALSE)
      if("inst" %in% inst)
        folder <- dir(file.path(libname, pkgname, "inst", "swipl", "lib", "swipl", "lib"),
                      pattern=R.version$arch, full.names=TRUE)

      if(length(folder) == 0)
        stop("Rolog: could not load libswipl.dll/so/dylib")
    }

    if(R.version$os == "linux-gnu")
      dyn.load(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
    else
      dyn.load(file.path(folder, "libswipl.dylib")) # macOS
  }

  # Load rolog.so
  library.dynam(chname="rolog", package=pkgname, lib.loc=libname, local=FALSE)
  
  op.rolog <- list(
    rolog.realvec = "#",  # prolog representation of R numeric vectors
    rolog.intvec  = "%",  # prolog representation of R integer vectors
    rolog.boolvec = "!",  # prolog representation of R boolean vectors
    rolog.charvec = "$$", # prolog representation of R character vectors
    rolog.portray = TRUE, # return prolog call, nicely formatted
    rolog.scalar  = TRUE) # convert R vectors of size 1 to scalars in prolog

  set <- !(names(op.rolog) %in% names(options()))
  if(any(set))
    options(op.rolog[set])

  invisible()
}

.onUnload <- function(libpath)
{
  print(libpath)

  # See .onLoad for details
  library.dynam.unload("rolog", libpath=libpath)

  if(.Platform$OS.type == "unix")
  {
    folder = dir(file.path(libpath, "swipl", "lib", "swipl", "lib"), 
		 pattern=R.version$arch, full.names=TRUE)

    if(R.version$os == "linux-gnu")
      dyn.unload(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
    else
      dyn.unload(file.path(folder, "libswipl.dylib"))
  }

  invisible()
}

.onAttach <- function(libname, pkgname)
{
  if(.Platform$OS.type == "unix")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, "swipl", "lib", "swipl"))
    if(!rolog_init())
      stop("Rolog: initialization of swipl failed.")  
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
  if(.Platform$OS.type == "windows")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, "swipl"))
    if(!rolog_init() && !rolog_init())
      stop("Rolog: initialization of swipl failed.")  
  }
  
  # SWI startup message
  welcome <- call("message_to_string", quote(welcome), expression(W))
  W <- once(welcome, options=list(quote=FALSE))
  packageStartupMessage(W$W)
  invisible()
}

.onDetach <- function(libpath)
{
  # Clear any open queries
  clear() 
  if(!rolog_done())
    stop("Rolog: not initialized.")

  Sys.unsetenv("SWI_HOME_DIR")
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
rolog_done <- function()
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
#' consult(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
#' findall(call("ancestor", quote(pam), expression(X)))
#' 
consult <- function(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
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
#' * _quote_: if `TRUE` (default), use simplified syntax (see [query()])
#' * numeric vector of size N -> _realvec_/N (default is #)
#' * integer vector of size N -> _intvec_/N (default is %)
#' * boolean vector of size N -> _boolvec_/N (default is !)
#' * character vector of size N -> _charvec_/N (default is $$)
#' * _scalar_: if `TRUE` (default), translate R vectors of length 1 to scalars
#' * _portray_: if `TRUE` (default) whether to return the prolog translation 
#'   as an attribute to the return value of [once()], [query()] and [findall()] 
#'
rolog_options <- function()
{
  list(
    realvec=getOption("rolog.realvec", default="#"),
    intvec=getOption("rolog.intvec", default="%"),
    boolvec=getOption("rolog.boolvec", default="!"),
    charvec=getOption("rolog.charvec", default="$$"),
    portray=getOption("rolog.portray", default=TRUE),
    scalar=getOption("rolog.scalar", default=TRUE))
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
#' * _boolvec_ (see option `rolog.boolvec`, default is !) is the name of the
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
#' The R elements are translated to the following prolog citizens:
#' 
#' * numeric -> real (vectors of size _N_ -> #/N)
#' * integer -> integer (vectors -> %/N)
#' * character -> string (vectors -> $$/N)
#' * symbol/name -> atom
#' * expression -> variable
#' * call/language -> compound
#' * boolean -> true, false (atoms)
#' * list -> list
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
portray <- function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))), 
  options=NULL)
{
  options = c(options, rolog_options())
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
#' once(call("member", 1, list(quote(a), quote(b), quote(c))))
#' 
#' # This query returns an empty list meaning yes, it works
#' once(call("member", 3, list(1, 2, 3)))
#'
#' # This query returns a list stating that it works if X = 1
#' once(call("member", 1, list(quote(a), expression(X))))
#' 
#' # This query returns a list stating that X = 1 and Z = expression(Y)
#' once(call("=", list(expression(X), expression(Y)), list(1, expression(Z))))
#' 
#' # This works for X = [1 | _]; i.e. something like [|](1, expression(_6330))
#' once(call("member", 1, expression(X)))
#'
#' # This returns S = '1.0' (scalar)
#' once(call("format", call("string", expression(S)), "~w", list(1)), options=list(scalar=TRUE))
#'   
#' # This returns S = '#(1.0)' (vector), because the 1 is translated to #(1.0). 
#' # To prevent "~w" from being translated to $$("~w"), it is given as an atom.
#' once(call("format", call("string", expression(S)), as.symbol("~w"), list(1)), 
#'   options=list(scalar=FALSE))
#'
once <- function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
  options=NULL)
{
  options <- c(options, rolog_options())
  
  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  # Invoke C++ function that calls prolog
  r <- .once(query, options)
  
  if(options$portray)
    attr(r, "query") <- q

  return(r)
}

#' Invoke a query several times
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
#' * _boolvec_ (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for vectors of booleans.
#' * _intvec_, _realvec_, _charvec_ define the compound names for vectors of
#'   integers, doubles and strings, respectively (defaults are %, # and $$).
#' * If _scalar_ is `TRUE` (default), vectors of length 1 are translated to 
#'   scalar prolog elements. If _scalar_ is `FALSE`, vectors of length 1 are
#'   also translated to compounds.
#'   
#' @return
#' If the query fails, an empty list is returned. If the query 
#' succeeds _N_ >= 1 times, a list of length _N_ is returned, each element
#' being a list of conditions for each solution, see [once()].
#'   
#' @md
#'
#' @seealso [once()]
#' for a single query
#'
#' @seealso [query()], [submit()], and [clear()] for fine-grained control over
#' non-deterministic queryies
#'
#' @seealso [rolog_options()]
#' 
#' @examples
#' # This query returns a list stating that it works if X = a, "b", ...
#' findall(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, NULL, NA)))
#'
#' # Continued
#' findall(call("member", expression(X), list(call("sin", call("/", quote(pi), 2)), expression(Y))))
#' 
findall <- function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))), options=NULL)
{
  options <- c(options, rolog_options())
  
  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  # Invoke C++ function that calls prolog
  r <- .findall(query, options)
  
  if(options$portray)
    attr(r, 'query') <- q
  
  return(r)
}

#' Create a query
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
#' * _boolvec_ (see option rolog.boolvec, default is !) is the name of the
#'   prolog compound for vectors of booleans.
#' * _intvec_, _realvec_, _charvec_ define the compound names for vectors of
#'   integers, doubles and strings, respectively (defaults are %, # and $$).
#' * If _scalar_ is `TRUE` (default), vectors of length 1 are translated to 
#'   scalar prolog elements. If _scalar_ is `FALSE`, vectors of length 1 are
#'   also translated to compounds.
#'
#' @return
#' If the creation of the query succeeds, `TRUE`.
#'   
#' @details
#' SWI-Prolog does not allow multiple open queries. If another query is open, it
#' it is closed and a warning is shown.
#' 
#' @md
#'
#' @seealso [once()] for a query that is submitted only a single time.
#' 
#' @seealso [findall()] for a query that is submitted until it fails.
#' 
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))))
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # FALSE
#' submit() # warning that no query is open
#' 
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' query(call("member", expression(X), list(TRUE, expression(Y)))) # warning that another query is open
#' clear()
#' 
query = function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
  options=NULL)
{
  options <- c(options, rolog_options())
  
  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  r <- .query(query, options)
  
  if(options$portray)
    attr(r, "query") <- q
  
  return(r)
}

#' Clear current query
#'
#' @return
#' TRUE (invisible)
#'
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#'
#' @seealso [submit()]
#' for a submitting a query.
#'
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
clear <- function()
{
  invisible(.clear())
}

#' Submit a query that has been opened with [query()] before.
#'
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'   
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#' 
#' @seealso [clear()]
#' for a clearing a query.
#' 
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#' 
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, expression(Y))))
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # FALSE
#' submit() # warning that no query is open
#'
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#' 
submit <- function()
{
  r <- .submit()
  return(r)
}

#' Translate simplified to canonical representation
#'
#' @param query
#' an R call representing a prolog query with prolog-like syntax, e.g.,
#' `member(.X, list(a, b, .Y))` that is used in [query()], [once()], and
#' [findall()]. This query is translated to Rolog's canonical representation, 
#' with R calls and prolog variables enclosed in an R expression, in this 
#' example, `call("member", expression(X), list(a, b, expression(Y))))`.
#'
#' @seealso [query()], [once()], [findall()]
#'
#' @examples
#' q = quote(member(.X, list(a, "b", 3L, 4, TRUE, .Y)))
#' findall(as.rolog(q))
#'
as.rolog <- function(query=quote(member(.X, list(a, "b", 3L, 4, TRUE, .Y))))
{
  if(is.symbol(query))
  {
    # Variable
    s = as.character(query)
    if(s == ".")
      return(expression(`_`))

    if(substr(s, 1, 1) == ".")
      return(as.expression(as.symbol(substr(s, 2, nchar(s)))))
  }

  if(is.call(query))
  {
    args <- as.list(query)
    args[-1] <- lapply(args[-1], FUN=as.rolog)
	
    # list(1, 2, 3) is a list not a call
    if(args[[1]] == "list")
      return(args[-1])

    return(as.call(args))
  }
	
  return(query)
}
