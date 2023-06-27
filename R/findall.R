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
#' @param env
#' The R environment in which the query is run (default: globalenv()). This is
#' mostly relevant for r_eval/2.
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
#' non-deterministic queries
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
#' # The same using simplified syntax
#' q <- quote(member(.X, ""[a, "b", 3L, 4, TRUE, NULL, NA, sin(pi/2), .Y]))
#' findall(as.rolog(q))
#' 
findall <- function(
    query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
    options=list(portray=FALSE),
    env=globalenv())
{
  options <- c(options, rolog_options())
  query <- .preprocess(query, preproc=options$preproc)

  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  # Invoke C++ function that calls prolog
  r <- .findall(query, options, env)

  # Hooks for postprocessing
  r <- lapply(r, FUN=.postprocess, postproc=options$postproc)
  if(options$portray)
    attr(r, 'query') <- q

  return(r)
}
