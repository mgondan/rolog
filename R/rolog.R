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
  rolog.ok <- FALSE
  msg <- ""
  libswipl <- ""
  home <- .find.swipl64()
  if(!is.na(home))
  {
    msg <- sprintf("Found SWI-Prolog at %s", home)
    libswipl <- .find.libswipl()
    if(!is.na(libswipl))
      rolog.ok <- TRUE
  }

  if(rolog.ok & libswipl != "")
    dyn.load(libswipl, local=FALSE)
  
  if(!rolog.ok)
    msg <- "This package requires the SWI-Prolog runtime.\n\nIf SWI-Prolog is not on your system\n- You can install SWI-Prolog from https://swi-prolog.org.\n- Alternatively, install the R package rswipl.\n\nIf SWI-Prolog has been installed on your system\n- Please add swipl to the PATH.\n- Alternatively, let the environment variable SWI_HOME_DIR point to the correct folder."

  op.rolog <- list(
    rolog.swi_home_dir = home,  # restore on .onUnload
    rolog.home         = home,
    rolog.ok           = rolog.ok,
    rolog.lib          = libswipl,
    rolog.message      = msg,
    rolog.realvec      = "##",     # prolog representation of R numeric vectors
    rolog.realmat      = "###",    # same for matrices
    rolog.intvec       = "%%",     # prolog representation of R integer vectors
    rolog.intmat       = "%%%",    # same for matrices
    rolog.boolvec      = "!!",     # prolog representation of R boolean vectors
    rolog.boolmat      = "!!!",    # same for matrices
    rolog.charvec      = "$$",     # prolog representation of R char vectors
    rolog.charmat      = "$$$",    # same for matrices
    rolog.portray      = TRUE,     # query() pretty prints prolog call
    rolog.preproc      = preproc,  # preprocessing hook in R
    rolog.postproc     = postproc, # postprocessing hook in R
    rolog.scalar       = TRUE)     # convert R singletons 1 to prolog scalars

  set <- !(names(op.rolog) %in% names(options()))
  if(any(set))
    options(op.rolog[set])

  if(!rolog_ok(warn=TRUE))
    return(FALSE)

  if(.Platform$OS.type == "windows")
    library.dynam("rolog", package=pkgname, lib.loc=libname, 
      DLLpath=file.path(home, "bin"))

  if(.Platform$OS.type == "unix")
    library.dynam(chname="rolog", package=pkgname, lib.loc=libname, local=FALSE)

  invisible()
}

.onUnload <- function(libpath)
{
  # See .onLoad for details
  library.dynam.unload("rolog", libpath=libpath)

  if(options()$rolog.ok & .Platform$OS.type == "unix")
  {
    lib <- options()$rolog.lib
    if(length(lib))
      dyn.unload(lib)
  }

  invisible()
}

.onAttach <- function(libname, pkgname)
{
  if(!rolog_ok())
    return(FALSE)

  Sys.setenv(SWI_HOME_DIR=options()$rolog.home)
  if(!rolog_init())
  {
    warning("rolog: initialization of swipl failed.")  
    return(FALSE)
  }

  packageStartupMessage(options()$rolog.message)

  # Commented out, needs swipl version 9
  W1 <- once(call("message_to_string", quote(threads), expression(W)))
  W2 <- once(call("message_to_string", quote(address_bits), expression(W)))
  W3 <- once(call("message_to_string", quote(version), expression(W)))
  packageStartupMessage(sprintf("Welcome to SWI-Prolog (%s%sversion %s)", W1$W, W2$W, W3$W))
  invisible()
}

.onDetach <- function(libpath)
{
  # Clear any open queries
  clear() 
  if(!rolog_done())
    stop("rolog: not initialized.")

  home = options()$rolog.swi_home_dir
  if(home == "")
    Sys.unsetenv("SWI_HOME_DIR")
  else
    Sys.setenv(SWI_HOME_DIR=home)
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
rolog_done <- function()
{
  .done()
}

#' Check if rolog is properly loaded
#'
#' @param warn
#' raise a warning if problems occurred
#'
#' @param stop
#' raise an error if problems occurred
#'
#' @return
#' TRUE if rolog is properly loaded
#'
rolog_ok <- function(warn=FALSE, stop=FALSE)
{
  if(options()$rolog.ok)
    return(TRUE)

  if(warn)
    warning(options()$rolog.message)

  if(stop)
    stop(options()$rolog.message)

  return(FALSE)
}

#' Quick access the package options
#' 
#' @return
#' list with some options for translating R expressions to prolog 
#'
#' @md
# 
#' @details
#' Translation from R to Prolog
#' 
#' * numeric vector of size N -> _realvec_/N (default is ##)
#' * integer vector of size N -> _intvec_/N (default is %%)
#' * boolean vector of size N -> _boolvec_/N (default is !!)
#' * character vector of size N -> _charvec_/N (default is $$)
#' * _scalar_: if `TRUE` (default), translate R vectors of length 1 to scalars
#' * _portray_: if `TRUE` (default) whether to return the prolog translation 
#'   as an attribute to the return value of [once()], [query()] and [findall()]
#'
rolog_options <- function()
{
  list(
    swi_home_dir=getOption("rolog.swi_home_dir", default="unknown"),
    home=getOption("rolog.home", default="home"),
    ok=getOption("rolog.ok", default=FALSE),
    lib=getOption("rolog.lib", default="unknown"),
    message=getOption("rolog.message", default=NA),
    realvec=getOption("rolog.realvec", default="##"),
    realmat=getOption("rolog.realmat", default="###"),
    intvec=getOption("rolog.intvec", default="%%"),
    intmat=getOption("rolog.intmat", default="%%%"),
    boolvec=getOption("rolog.boolvec", default="!!"),
    boolmat=getOption("rolog.boolmat", default="!!!"),
    charvec=getOption("rolog.charvec", default="$$"),
    charmat=getOption("rolog.charmat", default="$$$"),
    portray=getOption("rolog.portray", default=TRUE),
    preproc=getOption("rolog.preproc", default=preproc),
    postproc=getOption("rolog.postproc", default=postproc),
    scalar=getOption("rolog.scalar", default=TRUE))
}
