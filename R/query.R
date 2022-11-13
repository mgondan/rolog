#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y),
#'   NA, NaN, Inf, NULL, function(x) {y <- sin(x); y^2})))
#' submit() # X = a
#' submit() # X = "b"
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # X = NA
#' submit() # X = NaN
#' submit() # X = Inf
#' submit() # X = NULL
#' submit() # X = function(x) {y <- sin(x); y^2}))
#' submit() # FALSE (no more results)
#' submit() # warning that no query is open
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' query(call("member", expression(X), list(TRUE, expression(Y)))) # warning that another query is open
#' clear()
query <- function(
  query=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
  options=NULL)
{
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(FALSE)
  }

  options <- c(options, rolog_options())
	
  # Decorate result with the prolog syntax of the query
  if(options$portray)
    q <- portray(query, options)

  r <- .query(query, options)
	
  if(options$portray)
    attr(r, "query") <- q	
  return(r)
}

#' Clear current query
#'
#' @return
#' TRUE (invisible)
#'
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#'
#' @seealso [submit()]
#' for a submitting a query.
#'
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#'
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#'
clear <- function()
{
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(FALSE)
  }

  invisible(.clear())
}

#' Submit a query that has been opened with [query()] before.
#'
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'   
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#' 
#' @seealso [clear()]
#' for a clearing a query.
#' 
#' @seealso [once()]
#' for a opening a query, submitting it, and clearing it again.
#'
#' @seealso [findall()]
#' for a opening a query, collecting all solutions, and clearing it again.
#' 
#' @examples
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4, expression(Y))))
#' submit() # X = 3L
#' submit() # X = 4.0
#' submit() # X = TRUE
#' submit() # X = expression(Y) or Y = expression(X)
#' submit() # FALSE
#' submit() # warning that no query is open
#'
#' query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
#' submit() # X = a
#' submit() # X = "b"
#' clear()
#' 
submit <- function()
{
  if(!options()$rolog.ok)
  {
    warning("swipl not found in the PATH. Please set SWI_HOME_DIR accordingly or install R package rswipl.")
    return(FALSE)
  }

  .submit()
}
