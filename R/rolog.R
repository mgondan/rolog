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
  #
  # Search SWI-Prolog in the environment
  #
  libswipl = character(0)
  home <- Sys.getenv("SWI_HOME_DIR")
  msg <- ""
  if(home != "" & .Platform$OS.type == "windows" & R.Version()$arch == "x86_64")
  {
    pl0 <- try(system2(c(file.path(home, "bin", "swipl"),
      "--dump-runtime-variables"), stdout=TRUE, stderr=FALSE), silent=TRUE)
    if(!isa(pl0, "try-error"))
    {
      pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
      arch <- pl["PLARCH", ]
      if(arch == "x64-win64")
      {
        folder <- pl["PLLIBDIR", ]
        lib <- gsub("-l", "lib", pl["PLLIB", ])
        libswipl <- dir(folder,
          pattern=paste("^", lib, .Platform$dynlib.ext, "$", sep=""),
          full.names=TRUE)
  			
        if(length(libswipl))
          msg <- sprintf("Found SWI-Prolog at SWI_HOME_DIR: %s", home)
      }
    }
  }

  # SWI_HOME_DIR pointing to e.g. rswipl (no swipl.exe)
  if(home != "" & msg == "" & .Platform$OS.type == "windows")
  {
    libswipl <- dir(file.path(home, "bin"),
      pattern=paste("libswipl", .Platform$dynlib.ext, "$", sep=""),
      full.names=TRUE)
  		
    if(length(libswipl))
      msg <- sprintf("Found SWI-Prolog at SWI_HOME_DIR: %s", home)
  }

  # Typical installation in /usr/local/lib/swipl
  if(home != "" & .Platform$OS.type == "unix" & R.Version()$arch == "x86_64")
  {
    lib <- dir(file.path(home, "lib"), pattern=R.Version()$arch, full.names=TRUE)
    if(R.Version()$os == "linux-gnu")
      libswipl <- dir(lib, pattern="libswipl.so$", full.names=TRUE)
    else
      libswipl <- dir(lib, pattern="libswipl.dylib$", full.names=TRUE)

    if(length(libswipl) == 1)
    {
      dyn.load(libswipl, local=FALSE)
      msg <- sprintf("Found SWI-Prolog at SWI_HOME_DIR: %s", home)
    }
  }

  # Use ldd
  if(home != "" & msg == "" & .Platform$OS.type == "unix")
  {
    pl0 <- dir(file.path(home, "bin"), pattern="swipl$", full.names=TRUE)
    if(length(pl0) == 0)
    {
      arch <- dir(file.path(home, "bin"), pattern=R.Version()$arch, full.names=TRUE)
      if(length(arch) == 1)
        pl0 <- dir(arch, pattern="swipl$", full.names=TRUE)
    }

    if(length(pl0) == 1)
    {
      pl1 <- try(silent=TRUE, system2(c("ldd", pl0), stdout=TRUE, stderr=FALSE))
      if(!isa(pl1, "try-error"))
      {
        pl <- read.table(text=pl1, sep=" ", row.names=1, fill=TRUE)

        pl <- pl[pl[, 1] == "=>", ]
        libswipl <- pl[grep("^\\tlibswipl.so", rownames(pl)), 2]
        if(length(libswipl) == 1)
        {
          dyn.load(libswipl, local=FALSE)
          msg <- sprintf("Found SWI-Prolog at SWI_HOME_DIR: %s", home)
        }
      }
    }
  }

  #
  # Search SWI-Prolog in the PATH
  #
  if(msg == "" & .Platform$OS.type == "windows" & R.Version()$arch == "x86_64")
  {
  	pl0 <- try(silent=TRUE, system2(c("swipl", "--dump-runtime-variables"),
  	  stdout=TRUE, stderr=FALSE))
  	if(!isa(pl0, "try-error"))
  	{ 
  		pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
  		arch <- pl["PLARCH", ]
  		if(arch == "x64-win64")
  		{
  			home <- pl["PLBASE", ]
  			folder <- pl["PLLIBDIR", ]
  			lib <- gsub("-l", "lib", pl["PLLIB", ])
  			libswipl <- dir(folder, 
  				pattern=paste("^", lib, .Platform$dynlib.ext, "$", sep=""),
  				full.names=TRUE)
  		
	  		if(length(libswipl))
  				msg <- sprintf("Found SWI-Prolog in the PATH: %s", home)
  		}
  	}
  }

  # Installed from sources
  if(msg == "" & .Platform$OS.type == "unix")
  {
    pl0 <- try(silent=TRUE, system2(c("swipl", "--dump-runtime-variables"),
      stdout=TRUE, stderr=FALSE))
    if(!isa(pl0, "try-error"))
    {
      pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
      arch <- pl["PLARCH", ]
      home <- pl["PLBASE", ]
      folder <- pl["PLLIBDIR", ]
      lib <- gsub("-l", "lib", pl["PLLIB", ])
      if(R.version$os == "linux-gnu")
        libswipl <- dir(folder, 
          pattern=paste("^", lib, .Platform$dynlib.ext, "$", sep=""),
          full.names=TRUE)
      else
        libswipl <- file.path(folder, paste(lib, ".dylib", sep=""))

      if(length(libswipl))
      {
        dyn.load(libswipl, local=FALSE)
        msg <- sprintf("Found SWI-Prolog in the PATH: %s", home)
      }
    }
  }

  # Use ldd to find libswipl
  if(msg == "" & .Platform$OS.type == "unix")
  {
    pl0 <- try(silent=TRUE, system2(c("swipl", "--dump-runtime-variables"),
      stdout=TRUE, stderr=FALSE))
    if(!isa(pl0, "try-error"))
    {
      pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
      arch <- pl["PLARCH", ]
      home <- pl["PLBASE", ]

      pl0 <- try(silent=TRUE, system2(c("which", "swipl"), stdout=TRUE, stderr=FALSE))
      if(!isa(pl0, "try-error"))
      {
        pl1 <- try(silent=TRUE, system2(c("ldd", pl0), stdout=TRUE, stderr=FALSE))
        if(!isa(pl1, "try-error"))
        {
          pl <- read.table(text=pl1, sep=" ", row.names=1, fill=TRUE)

          # Keep it simple
          pl <- pl[pl[, 1] == "=>", ]
          libswipl <- pl[grep("^\\tlibswipl.so", rownames(pl)), 2]
          if(length(libswipl) == 1)
          {
            dyn.load(libswipl, local=FALSE)
            msg <- sprintf("Found SWI-Prolog in the PATH: %s", home)
          }
        }
      }
    }
  }

  #
  # Search SWI-Prolog in the registry
  #
  if(msg == "" & .Platform$OS.type == "windows" & R.Version()$arch == "x86_64")
  {
    pl0 <- try(silent=TRUE,
      utils::readRegistry("SOFTWARE\\SWI\\Prolog", hive="HLM", view="64-bit"))
    if(!isa(pl0, "try-error"))
    {
      home = pl0$home
      pl1 <- try(system2(c(file.path(home, "bin", "swipl"),
	"--dump-runtime-variables"), stdout=TRUE, stderr=FALSE))
      if(!isa(pl1, "try-error"))
      { 
        pl <- read.table(text=pl1, sep="=", row.names=1, comment.char=";")
        arch <- pl["PLARCH", ]
        if(arch == "x64-win64")
        {
          folder <- pl["PLLIBDIR", ]
          lib <- gsub("-l", "lib", pl["PLLIB", ])
          libswipl <- dir(folder,
            pattern=paste("^", lib, .Platform$dynlib.ext, "$", sep=""),
            full.names=TRUE)

          if(length(libswipl))
            msg <- sprintf("Found SWI-Prolog in the registry: %s", home)
        }
      }
    }
  }

  #
  # Find R package rswipl
  #
  if(msg == "" & .Platform$OS.type == "windows")
  {
    pl0 <- try(silent=TRUE, find.package("rswipl"))
    if(!isa(pl0, "try-error"))
    {
      home <- dir(pl0, pattern="swipl$", full.names=TRUE)
      libswipl <- dir(file.path(home, "bin"),
        pattern=paste("libswipl", .Platform$dynlib.ext, "$", sep=""),
        full.names=TRUE)
  		
      if(length(libswipl))
        msg <- sprintf("Found R package rswipl: %s", home)
    }
  }

  if(msg == "" & .Platform$OS.type == "unix")
  {
    pl0 <- try(silent=TRUE, find.package("rswipl"))
    if(!isa(pl0, "try-error"))
    {
      home <- dir(file.path(pl0, "swipl", "lib"), pattern="swipl$", full.names=TRUE)
      arch <- R.Version()$arch
      lib <- dir(file.path(home, "lib"), pattern=arch, full.names=TRUE)
      if(length(lib) == 0 & arch == "aarch64")
        lib <- dir(file.path(home, "lib"), pattern="arm64", full.names=TRUE)

      if(R.Version()$os == "linux-gnu")
        libswipl <- dir(lib, pattern="libswipl.so$", full.names=TRUE)
      else
        libswipl <- dir(lib, pattern="libswipl.dylib$", full.names=TRUE)

      if(length(libswipl) == 1)
      {
        dyn.load(libswipl, local=FALSE)
        msg <- sprintf("Found R package rswipl: %s", home)
      }
    }
  }

  if(length(libswipl) == 0)
    msg <- "SWI-Prolog not found. Please set SWI_HOME_DIR accordingly, or add swipl to the PATH, or install the R package rswipl."

  op.rolog <- list(
    rolog.swi_home_dir = home,  # restore on .onUnload
    rolog.home         = home,
    rolog.ok           = (length(libswipl) == 1),
    rolog.lib          = libswipl,
    rolog.message      = msg,
    rolog.realvec      = "#",      # prolog representation of R numeric vectors
    rolog.realmat      = "##",     # same for matrices
    rolog.intvec       = "%",      # prolog representation of R integer vectors
    rolog.intmat       = "%%",     # same for matrices
    rolog.boolvec      = "!",      # prolog representation of R boolean vectors
    rolog.boolmat      = "!!",     # same for matrices
    rolog.charvec      = "$$",     # prolog representation of R char vectors
    rolog.charmat      = "$$$",    # same for matrices
    rolog.portray      = TRUE,     # query() pretty prints prolog call
    rolog.preproc      = preproc,  # preprocessing hook in R
    rolog.postproc     = postproc, # postprocessing hook in R
    rolog.scalar       = TRUE)     # convert R singletons 1 to prolog scalars

  set <- !(names(op.rolog) %in% names(options()))
  if(any(set))
    options(op.rolog[set])

  if(!op.rolog$rolog.ok)
    return(invisible())

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
    dyn.unload(options()$rolog.lib)

  invisible()
}

.onAttach <- function(libname, pkgname)
{
  ok <- options()$rolog.ok
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(invisible())
  }

  Sys.setenv(SWI_HOME_DIR=options()$rolog.home)
  if(!rolog_init())
  {
    warning("rolog: initialization of swipl failed.")  
    return(FALSE)
  }

  packageStartupMessage(options()$rolog.message)
  W1 <- once(call("message_to_string", quote(threads), expression(W)))
  W2 <- once(call("message_to_string", quote(address_bits), expression(W)))
  W3 <- once(call("message_to_string", quote(version), expression(W)))

  packageStartupMessage(
    sprintf("Welcome to SWI-Prolog (%s%sversion %s)", W1$W, W2$W, W3$W))
  invisible()
}

.onDetach <- function(libpath)
{
  ok <- options()$rolog.ok
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(invisible())
  }

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
  ok <- options()$rolog.ok
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(invisible())
  }
 
  .init(argv1)
}

#' Clean up when detaching the library
#' 
#' @return
#' `TRUE` on success
rolog_done <- function()
{
  ok <- options()$rolog.ok
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(invisible())
  }

  .done()
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
    swi_home_dir=getOption("rolog.swi_home_dir", default="unknown"),
    home=getOption("rolog.home", default="home"),
    ok=getOption("rolog.ok", default=FALSE),
    lib=getOption("rolog.lib", default="unknown"),
    message=getOption("rolog.message", default=NA),
    realvec=getOption("rolog.realvec", default="#"),
    realmat=getOption("rolog.realmat", default="##"),
    intvec=getOption("rolog.intvec", default="%"),
    intmat=getOption("rolog.intmat", default="%%"),
    boolvec=getOption("rolog.boolvec", default="!"),
    boolmat=getOption("rolog.boolmat", default="!!"),
    charvec=getOption("rolog.charvec", default="$$"),
    charmat=getOption("rolog.charmat", default="$$$"),
    portray=getOption("rolog.portray", default=TRUE),
    preproc=getOption("rolog.preproc", default=preproc),
    postproc=getOption("rolog.postproc", default=postproc),
    scalar=getOption("rolog.scalar", default=TRUE))
}
