# This function is invoked by Makevars.win
.cat.swipl64 <- function()
{
  plbase <- .find.swipl64()
  if(!is.na(plbase))
    cat(shortPathName(plbase))
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

  plbase <- .registry()
  if(!is.na(plbase))
    return(plbase)

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

  arch <- shell("swipl --arch", intern=TRUE)
  if(arch != "x64-win64")
  {
    warning("plbase.R: swipl in PATH is not x64-win64")
    return(NA)
  }

  vars <- shell("swipl --dump-runtime-variables=cmd", intern=TRUE)
  plbase <- grep("^SET PLBASE=", vars, value=TRUE)
  plbase <- gsub("^SET PLBASE=", "", plbase)
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

  tryCatch(
  {
    reg <- readRegistry("SOFTWARE\\SWI\\Prolog1", hive="HLM")
  }, error=NA)

  if(is.na(plbase))
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

  rswipl <- find.package("rswipl")
  if(is.na(rswipl))
  {
    warning("plbase.R: R package rswipl not found")
    return(NA)
  }

  plbase <- file.path(rswipl, "swipl")
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
