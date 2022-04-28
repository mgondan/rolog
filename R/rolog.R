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
  # Load libswipl.dylib under macOS
  if(.Platform$OS.type == "unix" & R.version$os != "linux-gnu")
  {
    # Find folder like x86_64-linux
    fp <- file.path(libname, pkgname, "swipl", "lib", "swipl", "lib")
    arch <- R.version$arch
    if(arch == 'aarch64')
      arch <- 'arm64'
    folder <- dir(fp, pattern=arch, full.names=TRUE)
	
    # Preload libswipl.dll
    dyn.load(file.path(folder, "libswipl.dylib")) # macOS
  }
	
  if(.Platform$OS.type == "windows")
  {
    folder <- file.path(libname, pkgname, "swipl", "bin")
    dyn.load(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
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
  # See .onLoad for details
  library.dynam.unload("rolog", libpath=libpath)

  if(.Platform$OS.type == "unix" & R.version$os != "linux-gnu")
  {
    fp <- file.path(libpath, "swipl", "lib", "swipl", "lib")
    arch <- R.version$arch
    if(arch == 'aarch64')
      arch <- 'arm64'
    folder <- dir(fp, pattern=arch, full.names=TRUE)
    dyn.unload(file.path(folder, "libswipl.dylib"))
  }
	
  if(.Platform$OS.type == "windows")
  {
    folder <- file.path(libpath, "swipl", "bin")
    dyn.unload(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
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
  # For this reason, I invoke rolog_init twice here if needed. Any hint to a
  # cleaner solution is appreciated.
  if(.Platform$OS.type == "windows")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, "swipl"))

    if(!rolog_init() && !rolog_init())
      stop("Rolog: initialization of swipl failed.")  
  }
  
  # SWI startup message
  W <- once(call("message_to_string", quote(welcome), expression(W)))
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
rolog_done <- function()
{
  .done()
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
