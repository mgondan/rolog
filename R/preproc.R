# R-to-Prolog translation of not equal etc.
.table = c("!=" = "\\=", "<=" = "=<")

# Retrieve function call from builtin primitives
.fcall <- function(fun)
{
  chunk <- tail(deparse(fun), 1)
  name <- strsplit(chunk, "\"")[[1]][2]
  args <- formalArgs(args(fun))
  body <- as.call(lapply(FUN=as.name, c(name, args)))

  head <- replicate(length(args), substitute())
  names(head) <- args
  head <- as.pairlist(head)
  
  eval(call("function", head, body))
}

#' Default hook for preprocessing
#' 
#' @param query 
#' the R call representing the Prolog query. 
#'
#' @return
#' The default hook translates the inequality and smaller-than-or-equal-to from
#' R (!=, <=) to Prolog (\=, =<). Moreover, primitive functions are converted to
#' regular functions.
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
preproc <- function(query=quote(1 <= sin))
{
  if(inherits(query, "formula"))
  {
    for(i in 2:length(query))
      query[[i]] <- preproc(query[[i]])
    return(query)
  }

  if(is.call(query))
  {
    args <- as.list(query)
    index <- which(args[[1]] == names(.table))
    if(length(index) == 1)
      args[[1]] <- as.name(.table[index])

    args[-1] <- lapply(args[-1], FUN=preproc)
    return(as.call(args))
  }

  if(is.function(query))
    if(is.primitive(query))
      query <- .fcall(query)
    else
      body(query) <- preproc(body(query))

  if(is.list(query))
    return(lapply(query, FUN=preproc))

  return(query)
}

.preprocess <- function(query, preproc)
{
  if(is.function(preproc))
    return(preproc(query))

  if(is.list(preproc))
  {
    for(pp in preproc)
      query <- pp(query)
    return(query)
  }

  warning("Use dontCheck to skip preprocessing, or give a (list of) functions.")
  return(query)
}

#' Default hook for postprocessing
#' 
#' @param constraint
#' the R call representing constraints of the Prolog query. 
#'
#' @return
#' The default hook translates the inequality and smaller-than-or-equal-to back
#' from Prolog (\=, =<) to R (!=, <=).
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
postproc <- function(constraint=call("=<", 1, 2))
{
  if(is.call(constraint))
  {
    args <- as.list(constraint)

    index <- which(args[[1]] == .table)
    if(length(index) == 1)
      args[[1]] <- as.name(names(.table)[index])

    args[-1] <- lapply(args[-1], FUN=postproc)
    return(as.call(args))
  }

  if(is.function(constraint))
    body(constraint) <- postproc(body(constraint))

  if(is.list(constraint))
    return(lapply(constraint, FUN=postproc))

  return(constraint)
}

.postprocess <- function(constraint, postproc)
{
  if(is.function(postproc))
    return(postproc(constraint))

  if(is.list(postproc))
    for(pp in postproc)
      constraint <- pp(constraint)

  return(constraint)
}

.postprocess_list <- function(constraints=list(call("=<", 1, 2)), postproc=postproc)
{
  if(is.list(constraints))
    return(lapply(constraints, FUN=postproc))

  return(constraints)
}
