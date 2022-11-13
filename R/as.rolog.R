#' Translate simplified to canonical representation
#'
#' @param query
#' an R call representing a Prolog query with prolog-like syntax,
#' e.g., `member(.X, ""[a, b, .Y])` for use in [query()], [once()], 
#' and [findall()]. The argument is translated to Rolog's representation, 
#' with R calls corresponding to Prolog terms and R expressions corresponding to
#' Prolog variables. Variables and expressions in parentheses are evaluated.
#'
#' @seealso [query()], [once()], [findall()]
#'
#' @examples
#' q <- quote(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]))
#' as.rolog(q)
#' 
#' @examples
#' q <- quote(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]))
#' findall(as.rolog(q))
#'
as.rolog <- function(query=quote(member(.X, ""[a, "b", 3L, 4, (pi), TRUE, .Y])))
{
  if(is.symbol(query))
  {
    # Anonymous variable
    if(query == ".")
      return(expression(`_`))
	
    if(substr(query, 1, 1) == ".")
      return(as.expression(as.symbol(substr(query, 2, nchar(query)))))
  }
	
  if(is.call(query))
  {
    args <- as.list(query)
	
    # Things like (2 + 3) or (a) are evaluated
    if(args[[1]] == "(")
      return(as.rolog(eval(args[[2]])))

    # `[`("", 1, 2, 3), aka. ""[1, 2, 3] is a list
    if(args[[1]] == "[" & length(args[[2]]) == 1 & args[[2]] == "")
      return(lapply(args[c(-1, -2)], FUN=as.rolog))
	
    # list(1, 2, 3) is a list not a call
    if(args[[1]] == "list")
      return(lapply(args[-1], FUN=as.rolog))
	
    args[-1] <- lapply(args[-1], FUN=as.rolog)
      return(as.call(args))
  }
	
  if(is.list(query))
    return(lapply(query, FUN=as.rolog))

  return(query)
}
