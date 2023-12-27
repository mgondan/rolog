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
  plbase <- gsub("^PLBASE=", "", plbase)
  plbase <- gsub("\\;$", "", plbase)
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

  if(is.na(reg))
  {
    if(warn)
      warning("plbase.R: swipl not found in registry")
    return(NA)
  }

  plbase <- reg$home
  return(plbase)
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
