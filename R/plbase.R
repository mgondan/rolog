# This function is invoked by Makevars
.cat.swipl64 <- function(warn=FALSE)
{
  plbase <- .find.swipl64(warn)
  if(!is.na(plbase))
  {
    if(.Platform$OS.type == "windows")
      plbase = shortPathName(plbase)
    cat(plbase)
  }
  
  if(warn)
    warning("plbase.R: SWI-Prolog not found")
}

.cat.swilibs <- function(warn=FALSE)
{
  plbase <- .find.swipl64(warn)
  if(is.na(plbase))
  {
    if(warn)
      warning("plbase.R: SWI-Prolog not found")
    return()
  }

  if(.Platform$OS.type == "windows")
    plbase = shortPathName(plbase)

  if(.Platform$OS.type == "unix")
  {
    swipllib <- dir(file.path(plbase, "lib"), pattern="libswipl.a", recursive=TRUE)
    if(length(swipllib))
    {
      swipllib <- dir(file.path(plbase, "lib"), full.names=TRUE)
      cat(sprintf("-L%s -lswipl", swipllib))
    }
  }
}

# Search for swipl in the various places
.find.swipl64 <- function(warn=FALSE)
{
  plbase <- .env(warn)
  if(!is.na(plbase))
    return(plbase)

  plbase <- .rswipl(warn)
  if(!is.na(plbase))
    return(plbase)

  plbase <- .path(warn)
  if(!is.na(plbase))
    return(plbase)

  if(.Platform$OS.type == "windows")
  {
    plbase <- .registry(warn)
    if(!is.na(plbase))
      return(plbase)
  }

  if(warn)
    warning("plbase.R: SWI-Prolog not found")
  return(NA)
}

.path <- function(warn=FALSE)
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    if(warn)
      warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return(NA)
  }

  if(Sys.which("swipl") == "")
  {
    if(warn)
      warning("plbase.R: swipl not found in PATH")
    return(NA)
  }

  if(.Platform$OS.type == "windows")
  {
    arch <- system("swipl --arch", intern=TRUE)
    if(arch != "x64-win64")
    {
      warning("plbase.R: swipl in PATH is not x64-win64")
      return(NA)
    }
  }

  if(.Platform$OS.type == "windows")
  {
    vars <- system("swipl --dump-runtime-variables=cmd", intern=TRUE)
    plbase <- grep("^SET PLBASE=", vars, value=TRUE)
    plbase <- gsub("^SET PLBASE=", "", plbase)
    return(plbase)
  }
  
  vars <- system("swipl --dump-runtime-variables=sh", intern=TRUE)
  plbase <- grep("^PLBASE=", vars, value=TRUE)
  plbase <- gsub("^PLBASE=\"", "", plbase)
  plbase <- gsub("\"\\;$", "", plbase)
  return(plbase)
}

# Search for swipl in the registry, return PLBASE
.registry <- function(warn=FALSE)
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    if(warn)
      warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return("")
  }

  reg <- tryCatch(
  {
    readRegistry("SOFTWARE\\SWI\\Prolog", hive="HLM")
  }, error=function(e) NA)

  if(is.list(reg))
    return(reg$home)

  if(warn)
    warning("plbase.R: swipl not found in registry")
  return(NA)
}

# Search for R package rswipl
.rswipl <- function(warn=FALSE)
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    if(warn)
      warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return(NA)
  }

  rswipl <- find.package("rswipl", quiet=TRUE)
  if(length(rswipl) == 0)
  {
    if(warn)
      warning("plbase.R: R package rswipl not found")
    return(NA)
  }

  plbase <- file.path(rswipl, "swipl")
  if(.Platform$OS.type == "unix")
    plbase <- file.path(plbase, "lib", "swipl")
  return(plbase)
}

# Search for SWI_HOME_DIR
.env <- function(warn=FALSE)
{
  plbase <- Sys.getenv("SWI_HOME_DIR")
  if(plbase == "")
  {
    if(warn)
      warning("plbase.R: SWI_HOME_DIR is not set")
    return(NA)
  }
  return(plbase)
}

# Search for libswipl.dll
.find.libswipl <- function(warn=FALSE)
{
  plbase <- .env(warn)
  if(!is.na(plbase))
    return(.env.libswipl(plbase, warn))
  
  plbase <- .rswipl(warn)
  if(!is.na(plbase))
    return(.rswipl.libswipl(plbase, warn))
  
  plbase <- .path(warn)
  if(!is.na(plbase))
    return(.path.libswipl(plbase, warn))
  
  if(.Platform$OS.type == "windows")
  {
    plbase <- .registry(warn)
    if(!is.na(plbase))
      return(.registry.libswipl(plbase, warn))
  }
  
  if(warn)
    warning("plbase.R: SWI-Prolog not found")
  return(NA)
}

.env.libswipl <- function(plbase, warn=FALSE)
{
  if(.Platform$OS.type == "windows" & R.Version()$arch == "x86_64")
  {
    pl0 <- try(system2(c(file.path(plbase, "bin", "swipl"),
      "--dump-runtime-variables"), stdout=TRUE, stderr=FALSE), silent=TRUE)
    if(!isa(pl0, "try-error"))
    {
      pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
      bits <- pl["PLBITS", ]
      if(bits == "64")
      {
        shared <- pl["PLSHARED", ]
        if(shared == "no")
        {
	  message("plbase.R: found static libswipl.a in PATH")
          return("")
        }

        if(shared == "yes")
        {
          libswipl <- pl["PLLIBSWIPL", ]
          if(length(libswipl) == 1 & !is.na(libswipl))
            return(libswipl)
        }
      }
    }
  }
  
  # SWI_HOME_DIR pointing to rswipl
  if(.Platform$OS.type == "windows")
  {
    libswipl = dir(file.path(plbase, "bin"), full.names=TRUE,
                   pattern=paste("libswipl", .Platform$dynlib.ext, "$", sep=""))
    
    if(length(libswipl))
      return(libswipl)

    if(warn)
      warning("plbase.R: libswipl.dll not found in rswipl")
    return(NA)
  }
  
  arch <- R.Version()$arch
  lib <- dir(file.path(plbase, "lib"), pattern=arch, full.names=TRUE)
  if(length(lib) == 0 & arch == "aarch64")
    lib <- dir(file.path(plbase, "lib"), pattern="arm64", full.names=TRUE)
  
  if(R.Version()$os == "linux-gnu")
    libswipl <- dir(lib, pattern="libswipl.so$", full.names=TRUE)
  else
    libswipl <- dir(lib, pattern="libswipl.dylib$", full.names=TRUE)
  
  if(length(libswipl))
    return(libswipl)
  
  lib <- dir(file.path(plbase, "lib"), pattern=arch, full.names=TRUE)
  if(length(lib) == 0 & arch == "aarch64")
    lib <- dir(file.path(plbase, "lib"), pattern="arm64", full.names=TRUE)
  
  if(R.Version()$os == "linux-gnu")
  {
    static <- dir(lib, pattern="libswipl.a$", full.names=TRUE)
    if(length(static) == 1)
    {
      if(warn)
        message("plbase.R: found static libswipl.a in rswipl")
      return("")
    }
  }
  
  if(warn)
    warning("plbase.R: libswipl.dll not found in rswipl")
  return(NA)
}

.rswipl.libswipl <- function(plbase, warn=FALSE)
{
  if(.Platform$OS.type == "windows")
  {
    libswipl = dir(file.path(plbase, "bin"), full.names=TRUE,
      pattern=paste("libswipl", .Platform$dynlib.ext, "$", sep=""))
    
    if(length(libswipl))
      return(libswipl)

    if(warn)
      warning("plbase.R: libswipl.dll not found in rswipl")
    return(NA)
  }
  
  arch <- R.Version()$arch
  lib <- dir(file.path(plbase, "lib"), pattern=arch, full.names=TRUE)
  if(length(lib) == 0 & arch == "aarch64")
    lib <- dir(file.path(plbase, "lib"), pattern="arm64", full.names=TRUE)
  
  if(R.Version()$os == "linux-gnu")
    libswipl <- dir(lib, pattern="libswipl.so$", full.names=TRUE)
  else
    libswipl <- dir(lib, pattern="libswipl.dylib$", full.names=TRUE)
  
  if(length(libswipl))
    return(libswipl)

  # Check if static library
  lib <- dir(file.path(plbase, "lib"), pattern=arch, full.names=TRUE)
  if(length(lib) == 0 & arch == "aarch64")
    lib <- dir(file.path(plbase, "lib"), pattern="arm64", full.names=TRUE)
  
  if(R.Version()$os == "linux-gnu")
  {
    static <- dir(lib, pattern="libswipl.a$", full.names=TRUE)
    if(length(static) == 1)
    {
      if(warn)
        message("plbase.R: found static libswipl.a in rswipl")
      return("")
    }
  }
  
  if(warn)
    warning("plbase.R: libswipl.dll not found in rswipl")
  return(NA)
}

.registry.libswipl <- function(plbase, warn=FALSE)
{
  pl0 <- try(system2(c(file.path(plbase, "bin", "swipl"),
    "--dump-runtime-variables"), stdout=TRUE, stderr=FALSE), silent=TRUE)
  if(!isa(pl0, "try-error"))
  {
    pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
    bits <- pl["PLBITS", ]
    if(bits == "64")
    {
      shared <- pl["PLSHARED", ]
      if(shared == "no")
      {
	message("plbase.R: found static libswipl.a in PATH")
        return("")
      }

      if(shared == "yes")
      {
        libswipl <- pl["PLLIBSWIPL", ]
        if(length(libswipl) == 1 & !is.na(libswipl))
          return(libswipl)
      }
    }
  }

  if(warn)
    warning("plbase.R: libswipl.dll not found in registry")
  return(NA)
}

.path.libswipl <- function(plbase, warn=FALSE)
{
  pl0 <- try(system2(c("swipl", "--dump-runtime-variables"), 
    stdout=TRUE, stderr=FALSE), silent=TRUE)
  if(!isa(pl0, "try-error"))
  {
    pl <- read.table(text=pl0, sep="=", row.names=1, comment.char=";")
    bits <- pl["PLBITS", ]
    if(bits == "64")
    {
      shared <- pl["PLSHARED", ]
      if(shared == "no")
      {
	message("plbase.R: found static libswipl.a in PATH")
        return("")
      }

      if(shared == "yes")
      {
        libswipl <- pl["PLLIBSWIPL", ]
        if(length(libswipl) == 1 & !is.na(libswipl))
          return(libswipl)
      }
    }
  }

  if(warn)
    warning("plbase.R: shared libswipl not found in PATH")
  return(NA)
}
