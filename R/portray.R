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
