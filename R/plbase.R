# This function is invoked by Makevars.win
.cat.swipl64 <- function()
{
  plbase <- .find.swipl64()
  if(!is.na(plbase))
  {
    if(.Platform$OS.type == "windows")
      plbase = shortPathName(plbase)
    cat(plbase)
  }
}

# Search for swipl in the various places
.find.swipl64 <- function()
{
  plbase <- .env()
  if(!is.na(plbase))
    return(plbase)

  plbase <- .rswipl()
  if(!is.na(plbase))
    return(plbase)

  plbase <- .path()
  if(!is.na(plbase))
    return(plbase)

  if(.Platform$OS.type == "windows")
  {
    plbase <- .registry()
    if(!is.na(plbase))
      return(plbase)
  }

  warning("plbase.R: SWI-Prolog not found")
  return(NA)
}

.path <- function()
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return(NA)
  }

  if(Sys.which("swipl") == "")
  {
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
.registry <- function()
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return("")
  }

  reg <- tryCatch(
  {
    readRegistry("SOFTWARE\\SWI\\Prolog", hive="HLM")
  }, error=function(e) NA)

  if(is.na(reg))
  {
    warning("plbase.R: swipl not found in registry")
    return(NA)
  }

  plbase <- reg$home
  return(plbase)
}

# Search for R package rswipl
.rswipl <- function()
{
  if(Sys.getenv("SWI_HOME_DIR") != "")
  {
    warning("plbase.R: SWI_HOME_DIR is set, autodetection skipped")
    return(NA)
  }

  rswipl <- find.package("rswipl", quiet=TRUE)
  if(length(rswipl) == 0)
  {
    warning("plbase.R: R package rswipl not found")
    return(NA)
  }

  plbase <- file.path(rswipl, "swipl")
  if(.Platform$OS.type == "unix")
    plbase <- file.path(plbase, "lib", "swipl")
  return(plbase)
}

# Search for SWI_HOME_DIR
.env <- function()
{
  plbase <- Sys.getenv("SWI_HOME_DIR")
  if(plbase == "")
    return(NA)

  return(plbase)
}
