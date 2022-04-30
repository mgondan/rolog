#' Invoke a query once
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
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'
#' @md
#' 
#' @seealso [findall()]
#' for querying all solutions
#'
#' @seealso [query()], [submit()], and [clear()] for fine-grained control over
#' non-deterministic queries
#' 
#' @seealso [rolog_options()]
#' for options controlling R to prolog translation
#'
#' @examples
#' # This query returns FALSE
#' once(call("member", 1, list(quote(a), quote(b), quote(c))))
#' 
#' @examples
#' # This query returns an empty list meaning yes, it works
#' once(call("member", 3, list(1, 2, 3)))
#'
#' @examples
#' # This query returns a list stating that it works if X = 1
#' once(call("member", 1, list(quote(a), expression(X))))
#' 
#' @examples
#' # The same query using simplified syntax
#' q = quote(member(1, ""[a, .X]))
#' once(as.rolog(q))
#' 
#' @examples
#' # This query returns a list stating that X = 1 and Z = expression(Y)
#' once(call("=", list(expression(X), expression(Y)), list(1, expression(Z))))
#' 
#' @examples
#' # This works for X = [1 | _]; i.e. something like [|](1, expression(_6330))
#' once(call("member", 1, expression(X)))
#'
#' @examples
#' # This returns S = '1.0' (scalar)
#' once(call("format", call("string", expression(S)), "~w", list(1)), options=list(scalar=TRUE))
#'   
#' @examples
#' # This returns S = '#(1.0)' (vector), because the 1 is translated to #(1.0). 
#' # To prevent "~w" from being translated to $$("~w"), it is given as an atom.
#' once(call("format", call("string", expression(S)), as.symbol("~w"), list(1)), 
#'   options=list(scalar=FALSE))
#'
once <- function(
	query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
	options=NULL)
{
	options <- c(options, rolog_options())
	
	# Decorate result with the prolog syntax of the query
	if(options$portray)
		q <- portray(query, options)
	
	# Invoke C++ function that calls prolog
	r <- .once(query, options)
	
	if(options$portray)
		attr(r, "query") <- q
	
	return(r)
}
